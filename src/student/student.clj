(ns student.student
    (:use clojure.walk)
    (:gen-class))
  
;; Begin Pattern Matching
;; ================================================================================
(def comma
    (symbol ","))

(defn index-in-seq
    "Finds the index of item in the given sequence; the optional start parameter
    specifies the starting index to start looking from. Returns -1 when not found.
    Note that this is using a generic approach unoptimized for vectors."
    ([seq item start]
     (let [idx (.indexOf (nthrest seq start) item)]
       (if (>= idx 0)
         (+ start idx)
         -1)))
    ([seq item] (.indexOf seq item)))
  
  
  (def fail
    "Denotes a failure in matching"
    nil)
  
  (def no-bindings
    "Denotes successful match with no variable bindings"
    {})
  
  (defn variable?
    [x]
    (and (symbol? x) (= \? (get (str x) 0))))
  
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
    (cond (and (not (list? pat1))
               (not (variable? pat1))) (let [idx (index-in-seq input pat1 start)]
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
           pat (rest pattern)]
       (if (nil? pat)
         ;; If there's no more pat to match, this is a simple variable match of
         ;; ?var on the whole input.
         (match-variable v input bindings)
         ;; Otherwise, find the first position in the input where pat could match.
         ;; Try to match our segment until there pat from there. If this fails,
         ;; rerun with start+1 to try matching at the next position.
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
    (and (list? pattern) (get single-matcher-table (first pattern))))
  
  (defn single-matcher
    "Call the right single-pattern matching function."
    [pattern input bindings]
    ((get single-matcher-table (first pattern)) (rest pattern) input bindings))
  
  (defn segment-pattern?
    "Is this a segment-matching pattern?"
    [pattern]
    (and (list? pattern)
         (list? (first pattern))
         (symbol? (first (first pattern)))
         (get segment-matcher-table (first (first pattern)))))
  
  (defn segment-matcher
    "Call the right function for this kind of segment pattern."
    [pattern input bindings]
    ((get segment-matcher-table (first (first pattern))) pattern input bindings))
  
  (defn pat-match
    ([pattern input] (pat-match pattern input no-bindings))
    ([pattern input bindings]
        (println pattern)
     (cond (= bindings fail) fail
           (variable? pattern) (match-variable pattern input bindings)
           (= pattern input) bindings
           (single-pattern? pattern) (single-matcher pattern input bindings)
           (segment-pattern? pattern) (segment-matcher pattern input bindings)
           (and (list? pattern) (list? input)) (pat-match
                                                (rest pattern)
                                                (rest input)
                                                (pat-match
                                                 (first pattern)
                                                 (first input)
                                                 bindings))
           :else fail)))
 
           
;; Rule based translator
;; ================================================================================
(defn call-arg-on-remaining-args ; funcall
    "Calls an argument on the optionals provided after the initial argument
     eg: (call-arg-on-remaining-args #'* 2 2 2) OUTPUT: 8"
    [arg & remaining-args]
     (apply arg remaining-args))

(def abbreviation-table
    (atom {}))

(defn expand-pat-match-abbrev
    "Expand all pattern matching abbreviations in pat"
    [pat]
    (cond
        (symbol? pat) (get @abbreviation-table pat pat)
        (empty? pat) pat
            :else (cons (expand-pat-match-abbrev (first pat))
                        (expand-pat-match-abbrev (rest pat)))))

(defn pat-match-abbrev
    "Define symbol as macro and swap for a pat-match(ed) patted"
    [sym expansion] ; symbol expansion
    (swap! abbreviation-table (fn [x] (assoc x sym expansion)))
        (expand-pat-match-abbrev expansion))

(defn rule-based-translator
    [input rules & keys ]
    (let [matcher pat-match
          action postwalk-replace]
    (some 
        (fn [rule]
            (println rule)
            (let [result (matcher (first rule) input)]
                (if (not (= result fail))
                    (action result (rest rule))))) rules)))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))
;; Begin Student 
;; ================================================================================ 


(def basic-student-rules 
  '(((?x* .)                  ?x)
    ((?x* . ?y*)          (?x ?y))
    ((?if ?x* (symbol ",") then ?y*)  (?x ?y))
    ((?if ?x* then ?y*)              (?x ?y))
    ((?if ?x* (symbol ",") ?y*)       (?x ?y))
   ; ((?x* (symbol ,) and ?y*)      (?x ?y))
    ((find ?x* and ?y*)     ((= to-find-1 ?x) (= to-find-2 ?y)))
    ((find ?x*)             (= to-find ?x))
    ((?x* equals ?y*)       (= ?x ?y))
    ((?x* same as ?y*)      (= ?x ?y))
    ((?x* = ?y*)            (= ?x ?y))
    ((?x* is equal to ?y*)  (= ?x ?y))
    ((?x* is ?y*)           (= ?x ?y))
    ((?x* - ?y*)            (- ?x ?y))
    ((?x* minus ?y*)        (- ?x ?y))
    ((difference between ?x* and ?y*)  (- ?y ?x))
    ((difference ?x* and ?y*)          (- ?y ?x))
    ((?x* + ?y*)            (+ ?x ?y))
    ((?x* plus ?y*)         (+ ?x ?y))
    ((sum ?x* and ?y*)      (+ ?x ?y))
    ((product ?x* and ?y*)  (* ?x ?y))
    ((?x* * ?y*)            (* ?x ?y))
    ((?x* times ?y*)        (* ?x ?y))
    ((?x* / ?y*)            (/ ?x ?y))
    ((?x* per ?y*)          (/ ?x ?y))
    ((?x* divided by ?y*)   (/ ?x ?y))
    ((half ?x*)             (/ ?x 2))
    ((one half ?x*)         (/ ?x 2))
    ((twice ?x*)            (* 2 ?x))
    ((square ?x*)           (* ?x ?x))))
   ; ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
   ; ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
   ; ((?x* % ?y*)            (* (/ ?x 100) ?y))))


(def ^:dynamic *student-rules* 
  (map expand-pat-match-abbrev basic-student-rules))


(defn translate-to-expression [word]
  "Translate the problem state in 'words' to and equation of expression"
  ())

  (defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (println "Hello Clojure"))
  