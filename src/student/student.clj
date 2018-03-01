(ns student.student
  (:use clojure.walk)
  (:use clojure.pprint) ; common lisp formatting
  (:use clojure.stacktrace)
  (:gen-class))

;; Begin Pattern Matching
;; ================================================================================
(declare unknown-parameter)


(defn index-in-seq
  "Finds the index of item in the given sequence; the optional start parameter
  specifies the starting index to start looking from. Returns -1 when not found.
  Note that this is using a generic approach unoptimized for vectors."
  ([seq item start]
   (let [idx (.indexOf (nthrest seq start) item)]
     (if (>= idx 0)
       (+ start idx)
       -1))))


(def fail
  "Denotes a failure in matching"
  nil)


  ; Some required new types
(defmulti get-lhs class)
(defmethod get-lhs clojure.lang.PersistentList [expre]
  (if (> (count expre) 3)
    fail
    (second expre)))

(defmulti get-op class)
(defmethod get-op clojure.lang.PersistentList [expre]
  (if (> (count expre) 3)
    fail
    (first expre)))

(defmulti get-rhs class)
(defmethod get-rhs clojure.lang.PersistentList [expre]
  (if (> (count expre) 3)
    fail
    (nth expre 2)))


(defn expre? 
  [e]
  (and
    (not= nil (get-op e))
    (not= nil (get-lhs e))
    (not= nil (get-rhs e))))

(def no-bindings
  "Denotes successful match with no variable bindings"
  {})

(defn variable?
  [x]
  (or (and 
    (unknown-parameter x) (= \? (get (str x) 0)))
  ))

(defn get-binding
  "Find a variable->value binding in the given binding."
  [v bindings]
  (get bindings v))

(defn extend-bindings
  "Add a v -> value mappping to bindings."
  [v value bindings]
  (assoc bindings v value))

(defn match-variable
  "Does v match input? Uses (or updates) and returns bindings."
  [v input bindings]
  (let [b (get-binding v bindings)]
    (cond (nil? b) (extend-bindings v input bindings)
          (= input b) bindings
          :else fail)))

(declare pat-match)

(defn match-is
  "Suceed and bind var if the input satisfied pred.
  var-and-pred is the list (var pred)."
  [var-and-pred input bindings]
  (let [[v pred] var-and-pred
        new-bindings (pat-match v input bindings)]
    (if (or (= new-bindings fail)
            (not ((resolve pred) input)))
      fail
      new-bindings)))

(defn match-and
  "Succeed if all the patterns match the input."
  [patterns input bindings]
  (cond (= bindings fail) fail
        (empty? patterns) bindings
        :else (match-and
               (rest patterns)
               input
               (pat-match (first patterns) input bindings))))

(defn match-or
  "Succeed if any of the patterns match the input."
  [patterns input bindings]
  (if (empty? patterns)
    fail
    (let [new-bindings (pat-match (first patterns) input bindings)]
      (if (= new-bindings fail)
        (match-or (rest patterns) input bindings)
        new-bindings))))

(defn match-not
  "Succeed if none of the patterns match the input.
   This will never bind variables."
  [patterns input bindings]
  (if (match-or patterns input bindings)
    fail
    bindings))

(def single-matcher-table
  "Table mapping single matcher names to matching functions."
  {'?is match-is
   '?or match-or
   '?and match-and
   '?not match-not})

(defn first-match-pos
  "Find the first position that pat1 could possibly match input, starting
   at position start. If pat1 is non-constant, then just return start,
   conservatively assuming it could match."
  [pat1 input start]
  (cond (and (not (seq? pat1))
             (not (variable? pat1)))
    (let [idx (index-in-seq input pat1 start)]
        (if (< idx 0)
            nil
           idx))
        (< start (count input)) start
        :else nil))

(defn segment-match-*
  "Match the segment pattern ((?* ?var) . pat) against input. The optional start
  parameter specifices where to start matching (index in input) the pattern
  after the current ?* match."
  ([pattern input bindings] (segment-match-* pattern input bindings 0))
  ([pattern input bindings start]
   (let [v (second (first pattern))
         pat (next pattern)]
     (if (nil? pat) ;<<<<<< at the end of a sentence pat is emply list
       (match-variable v input bindings)
       (let [pos (first-match-pos (first pat) input start)]
         (if (nil? pos)
           fail
           (let [b2 (pat-match pat
                               (nthrest input pos)
                               (match-variable v
                                               (take pos input)
                                               bindings))]
             ;; If this match failed, try another longer one
             (if (= b2 fail)
               (segment-match-* pattern input bindings (+ pos 1))
               b2))))))))

(defn segment-match-+
  "Match ?+ -- one or more elements of input."
  [pattern input bindings]
  (segment-match-* pattern input bindings 1))

(defn segment-match-?
  "Match ?? -- zero or one elements of input."
  [pattern input bindings]
  (let [v (second (first pattern))
        pat (rest pattern)]
    (or (pat-match (conj pat v) input bindings)
        (pat-match pat input bindings))))

(defn segment-match-if
  "Test if an arbitrary input is true"
  [pattern input bindings]
  (let [f (postwalk-replace bindings (second (first pattern)))] ; Replace all occurrences of ?var with appropriate bindings using a map
      (when (eval f)
          (pat-match (rest pattern) input bindings))))



(def segment-matcher-table
  "Table mapping segment matcher names to matching functions."
  {'?* segment-match-*
   '?+ segment-match-+
   '?? segment-match-?
   '?if segment-match-if})

(defn single-pattern?
  "Is this a single-matching pattern?"
  [pattern]
  ;(println "in single-pattern?")
  (and (seq? pattern) (get single-matcher-table (first pattern))))

(defn single-matcher
  "Call the right single-pattern matching function."
  [pattern input bindings]
  ((get single-matcher-table (first pattern)) (rest pattern) input bindings))

(defn segment-pattern?
  "Is this a segment-matching pattern?"
  [pattern]
  (and (seq? pattern)
       (seq? (first pattern))
       (symbol? (first (first pattern)))
       (get segment-matcher-table (first (first pattern)))))

(defn segment-matcher
  "Call the right function for this kind of segment pattern."
  [pattern input bindings]
  ((get segment-matcher-table (first (first pattern))) pattern input bindings))

(defn flatten-shit
    "Prep the final pattern matcher thing to do the
     the correct pattern match."
    [exp]
    (and (seq? exp) (= (count exp) 2)) (flatten (rest exp)))

(defn pat-match  ; If count is equal to 2 AKA (is (?* ?y)) then next pat-match pattern should be flattened --> ((?* ?y)) now equals (?* ?y)!! 
  ([pattern input] (pat-match pattern input no-bindings))
  ([pattern input bindings]
    ;(do (println "pattern // pat-match : " pattern))    
    ;(do (println "input // pat-match : " input))
    ;(do (println "bindings // pat-match : " bindings))
   (cond
     (= bindings fail)  fail
     (variable? pattern) (match-variable pattern input bindings)
     (= pattern input) bindings
     (segment-pattern? pattern) (segment-matcher pattern input bindings)
     (single-pattern? pattern) (single-matcher pattern input bindings)
   ;  (and (seq? pattern) (seq? input) (= (count pattern) 2))
   ;  (pat-match (flatten (rest pattern)) (rest input)
   ;     (pat-match (first pattern) (first input) bindings))
     (and (seq? pattern) (seq? input))
          (pat-match (rest pattern) (rest input)
              (pat-match (first pattern) (first input) bindings))
         :else fail)))

;; Rule based translator
;; ================================================================================
(defn call-arg-on-remaining-args ; funcall
  "Calls an argument on the optionals provided after the initial argument
   eg: (call-arg-on-remaining-args #'* 2 2 2) OUTPUT: 8"
  [arg & remaining-args]
   (apply arg remaining-args))

(defn reject-empty-list ; consp in common-lisp
    "Only accept a list if it isn't empty"
    [object]
    (and (list? object) (not (empty? object))))

(defn append-to
"Append all values to a list and sort it
 (append-to '(1 2 3) '() '(4 5 6) '1) --> (1 1 2 3 4 5 6)"
[& input]
(sort (flatten (conj input))))

(def abbreviation-table
  (atom {}))



(defn expand-pat-match-abbrev
  "Expand all pattern matching abbreviations in pat"
  [pat]
  (cond
      (symbol? pat)(get @abbreviation-table pat pat)
      (empty? pat) pat
          :else (lazy-seq(cons (expand-pat-match-abbrev (first pat))
                      (expand-pat-match-abbrev (rest pat))))))

(defn pat-match-abbrev
  "Define symbol as macro and swap for a pat-match(ed) patted"
  [sym expansion] ; symbol expansion
  (swap! abbreviation-table (fn [x] (assoc x sym expansion)))
      (expand-pat-match-abbrev expansion))

(defn rule-based-translator
  "Apply a set of rules"
  [input rules & {:keys [matcher rule-if action rule-then]
                  :or {matcher pat-match
                       rule-if first
                       rule-then next
                       action postwalk-replace}}]
  (first (some
      (fn [rule]
          (let [result (matcher (rule-if rule) input)]  
              (if (not (= result fail))
                  (action result
                      (rule-then rule))))) rules)))


;; Begin Student
;; ================================================================================
(defn exp-p ; is an expression parameter? --> see book for more
[x]
(reject-empty-list x))

(defn exp-args ; rest of expression arguments
[x]          ; --> exp-p and exp-args will be used in conjuction
(rest x))

(defn make-expression
  "Turn 1 + 2 into + 1 2"
  [lhs op rhs] ; method for taking it from a listy
  (list op lhs rhs))

(defn noise-word-p ; CHECK according to the book
"A word we don't really care about"
[word]
('#{a an An the The this This number of $} word))


(def operators-and-their-inverses
  "Helper variables for return-inverse-operation"
  '((+ -) (- +) (* /) (/ *) (= =)))

(defn return-inverse-operation ; CHECK
  "Return the inverse operation"
  [operation ops-inverse]
  (let [inverse-poss ops-inverse]
    (if (= (ffirst inverse-poss)) operation)
      (first (second inverse-poss))
    (if (not (= (ffirst inverse-poss) operation))
      (return-inverse-operation operation (rest inverse-poss) ) (second (first inverse-poss)))))

(defn unknown-parameter ;
  "Is the argument an unknown variable? (implements symbolp)"
  [expre]
  (cond
    (= expre nil) true
    (symbol? expre) true
    (keyword? expre) true
    (= '() expre) true
    :else nil))

(defn commutative-p
"Is the operation commutative (* + =)?"
[operand]
( '#{+ = *} operand ))

(defn make-var-for-word
 "Will make a variable given a word (ex: Tom has 3 assignments and 2 days to do it. Will he have enough days?
  Word = Assignment = 3
  Word = days = 2
  We assume these words will be at the beginning of a pattern match sequence based on lhs rhs etc"
  [input-words]
   (do (println "make var for word: " input-words))
   (first input-words))

(defn binary-expre-p
    "Is the input expression binary?"
    [expre]
    (and (exp-p expre) (= (count (exp-args expre)) 2)))

(defn prefix-to-infix-notation ; atom in cl == (not (seq? x)) in clojure
       "Translate from prefix to infix notation
       EX: Infix  --> X + Y
       EX: Prefix --> + X Y"
       [expre & all-elem-test]
       (if (not (seq? expre)) expre ; works
           (map #'prefix-to-infix-notation
            (if (binary-expre-p expre)
              (seq (list (get-lhs expre) (get-op expre) (get-rhs expre)))) expre))) ; We need to create a left hand side and right hand side representation
                        ; the expression.

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(def ^:dynamic *basic-basic-student-rules*
  `[
    ~['(?x is ?y)  '(= ?x ?y)]
  ])


; Utilize the word comma as our actual comma during the input phase of things
  (def ^:dynamic *basic-student-rules* 
    `[
      ~['(?x* .)                                     '?x]
      ~['(?x* . ?y*)                            '(?x ?y)]
      ~['(if ?x* comma then ?y*)                '(?x ?y)] ;comma
      ~['(If ?x* comma then ?y*)                '(?x ?y)] ;comma
      ~['(if ?x* then ?y*)                      '(?x ?y)]
      ~['(If ?x* then ?y*)                      '(?x ?y)]
      ~['(if ?x* comma ?y*)                     '(?x ?y)] ;comma
      ~['(If ?x* comma ?y*)                     '(?x ?y)] ;comma
      ~['(?x* comma and ?y*)                    '(?x ?y)]   ;comma
      ~['(?x* and ?y*)                          '(?x ?y)]
      ~['(if ?x* and ?y*)                       '(?x ?y)]
      ~['(find ?x* and ?y*)       '((= to-find-1 ?x) (= to-find-2 ?y))]
      ~['(find ?x*)                      '(= to-find ?x)]
      ~['(?x* equals ?y*)                     '(= ?x ?y)]
      ~['(?x* same as ?y*)                    '(= ?x ?y)]
      ~['(?x* = ?y*)                          '(= ?x ?y)]
      ~['(?x* is equal to ?y*)                '(= ?x ?y)]
      ~['(?x* is ?y*)                         '(= ?x ?y)]
      ~['(?x* - ?y*)                          '(- ?x ?y)]
      ~['(?x* minus ?y*)                      '(- ?x ?y)]
      ~['(difference between ?x* and ?y*)     '(- ?y ?x)]
      ~['(difference ?x* and ?y*)             '(- ?y ?x)]
      ~['(?x* + ?y*)                          '(+ ?x ?y)]
      ~['(?x* plus ?y*)                       '(+ ?x ?y)]
      ~['(sum ?x* and ?y*)                    '(+ ?x ?y)]
      ~['(product ?x* and ?y*)                '(* ?x ?y)]
      ~['(?x* * ?y*)                          '(* ?x ?y)]
      ~['(?x* times ?y*)                      '(* ?x ?y)]
      ~['(?x* / ?y*)                          '(/ ?x ?y)]
      ~['(?x* per ?y*)                        '(/ ?x ?y)]
      ~['(?x* divided by ?y*)                 '(/ ?x ?y)]
      ~['(half ?x*)                            '(/ ?x 2)]
      ~['(one half ?x*)                        '(/ ?x 2)]
      ~['(twice ?x*)                           '(* 2 ?x)]
      ~['(square ?x*)                         '(* ?x ?x)]
      ~['(?x* % less than ?y*)  '(* ?y (/ (- 100 ?x) 100))]
      ~['(?x* % more than ?y*)  '(* ?y (/ (+ 100 ?x) 100))]
      ~['(?x* % ?y*)                  '(* (/ ?x 100) ?y)]
    ])

(defn map-expand-to-rules
  "Expand all the rules to allow us to actually match/translate"
  [func [key value]]
  [(func key) value])

(def ^:dynamic *student-rules*
  (map (partial map-expand-to-rules expand-pat-match-abbrev)
       *basic-student-rules*))


; STUDENT FUNCTIONS NOT COMPLETED
; ===================================================================================
(defn in-exp ; Is this equal to contains? --> https://clojuredocs.org/clojure.core/contains_q --> Contains? can't act on lists
"Return true if input is within the expression"
[x expre]
  (or (= x expre)
    (and (list? expre)
      (or (in-exp x (get-lhs expre)) (in-exp x (get-rhs expre))))))

(defn no-unknown-var
"Returns true if all variables in expression are now known"
  [expre]
  (cond (unknown-parameter expre) nil
    (not (seq? expre)) true
    (no-unknown-var (get-lhs expre)) (no-unknown-var (get-rhs expre))
    :else fail))

(defn one-unknown-var
"Returns the single unkown expression if only one exists"
  [expre]
  (print)
  (cond
    (symbol? expre) expre ; CHECK
    (not (expre? expre)) fail ; CHECK
    (no-unknown-var (get-lhs expre))(one-unknown-var (get-rhs expre))
    (no-unknown-var (get-rhs expre))(one-unknown-var (get-lhs expre))
    :else fail))

(defn solve-arithmetic ; We may need to add a constructor class to this to have proper formatting
"Do the arithmetic for the right hand side
 This assumes the right hand side is in the
 correct form"
 [equation]
 (list (get-lhs equation) '= (eval (get-rhs equation))))

 (defn print-equation ; Not working just use printf instead of formatting the bullshit
  "Format and print the equation so we can
   see the student work"
    [header equation]
    (printf header)
    (map #'prefix-to-infix-notation equation))

(defn isolate
"Isolate the lone x in e on the left hand side of e
 Requires many other functions --> probably one of the
 last functions we will finish."
 [e x]
  (do (println "this the unknown in the following expression: " x))
  (do (println "expression: " e))
  (cond
    ; First case
    (= (get-lhs e) x) e
    ; Second case
    (in-exp x (get-rhs e)) (isolate (make-expression (get-rhs e) '= (get-lhs e)) x)
    ; Third case
    (in-exp x (get-lhs (get-lhs e))) (isolate (make-expression (get-lhs (get-lhs e)) '=
                   (make-expression (get-rhs e)
                         (return-inverse-operation (get-op (get-lhs e)) operators-and-their-inverses)
                         (get-rhs (get-lhs e)))) x)
    ; Fourth case
    (commutative-p (get-op (get-lhs e))) (isolate (make-expression (get-rhs (get-lhs e)) '= (make-expression (get-rhs e)
                                                  (return-inverse-operation
                                                          (get-op (get-lhs e)) operators-and-their-inverses)
                                                  (get-lhs (get-lhs e)))) x)
    ; Fifth case
    :else (isolate (make-expression (get-rhs (get-lhs e)) '= (make-expression (get-lhs (get-lhs e)) (get-op (get-lhs e)) (get-rhs e))) 
                                                          x)))


; Broken at line 515 index out of bounds
(defn solve
"Solve a system of equations by constraint propagation"
[equations known]

(or
  (some (fn [equation]
    (let [x (one-unknown-var equation)]
            
          ;  (do (println "solve || This is x: " x))
            (when x
              ;(do (println x))
              (let [answer  (solve-arithmetic
                            (isolate equation x)) ; should look like: y = 5
                    action postwalk-replace]
              (do (println "system of equations: " equations))
              (do (println "this is the equation with one known: " equation))
              (do (println "this is the answer: " answer))
              (do (println "\n"))
              (solve (action {(first answer) (get-rhs answer)}
                                        ; idk if the line below this is right, we'll see.
                                        (remove (partial = equation) equations))
                      (cons answer known))))))
        equations)
  known))

(defn solve-equations
"Print the equations and their solution"
[equations]
  ;(do (println equations))
  (print-equation "The equations I am currently solving are: " equations)
  (print-equation "The solution is: " (solve equations nil)))

(defn create-list-of-equations
"Separate the equations into nested parenthesis"
[expre]
(do (println "// create-list-of-equations // expre: " expre )) ;((= (+ 3 4) (* (- (+ 2 x)) 7)) (= (+ (* 3 x ) y) 12 ))
  (cond
    (empty? expre) fail
    (not (sequential? (first expre))) (list expre)
    :else
      (concat
          (create-list-of-equations (first expre))
                 (create-list-of-equations (rest expre)))))

(declare translate-to-expression)

(defn translate-pair ;depricated
  [value-pair]
  (do (println "// translate-pair // This is a first pair : " value-pair))
  (cons
        (rest value-pair)
        (translate-to-expression (rest value-pair))))

; Perhaps broken, but need other things to check
(defn translate-to-expression ; rule based translator takes input rule & keys rule-if rule-then (first and rest) ;rule response sublis
  "Translate an English phrase into an equation or expression"
  [sentence & { :keys [rule-if rule-then p-replace]
                               :or {
                                    p-replace postwalk-replace
                                    rule-if first
                                    rule-then rest
                                   }}]
  (do (println "// translate-to-expression // current sentence: " sentence ))
  (or (rule-based-translator sentence *student-rules* 
    :action (fn [bindings response]
            (p-replace  (into {}
                           (map (fn [[var binding-to]]
                                 [var (translate-to-expression binding-to)]) ; This throws the variable to translate-pair
                                  bindings))
                                    response))) ; THis is returning
      (make-var-for-word sentence)))

(defn student
"Solve certain algebra word problems"
[words]
  (solve-equations
    (create-list-of-equations
      (translate-to-expression
        (remove noise-word-p words)))))


;=========Test Cases=========
; '(The daily cost of living for a group is the overhead cost plus the running cost for each person times the number of people in the group . This cost for one group equals $ 100 comma and the number of people in the group is 40 . If the overhead cost is 10 times the running cost comma find the overhead and running cost for each person .)
; '(If the number of customers Tom gets is twice the square of 20% of the number of advertisements he runs comma and the number of advertisements is 45 , then what is the number of customers Tom gets ?)
; '(Fran's age divided by Robin's height is one half Kelly's IQ . Kelly's IQ minus 80 is Robin's height . If Robin is 4 feet tall comma how old is Fran ?)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello Clojure"))
