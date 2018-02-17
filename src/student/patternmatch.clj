; Created by: Erik Whipp 
; &copy; 2018
; Ain't got no license, doe muhfucka


; FUNCTIONS THAT MUST BE IMPLEMENT
; -- segments
; ---- multiply            DONE
; ---- questions           DONE
; ---- addition            DONE
; ---- if                  DONE
; -- singles
; ---- is                   DONE
; ---- or                   DONE
; ---- and                  DONE        
; ---- not                  DONE
; -- helpers
; ---- expand-pat-match
; ---- abbreviate the table
; ---- variable-p           DONE
; ---- pattern-matcher      DONE
; ---- segment-pattern-p    DONE
; ---- single-pattern-p     DONE
; ---- single-match-fn      DONE
; ---- match-with-variable  DONE
; ---- single-matcher       DONE 
; ---- segment-match-fn     DONE
; -- Translators
; ---- match-abrev
; ---- pat-match-abbrev 
; ---- expand-match-abrev
; ---- rules-based-translator 
; ---- first-position-matcher DONE 
;
(use 'clojure.test)
; Forward declarations
; ==========================================================================
(declare single-function-matcher)
(declare pos-seq-satisfy)
(declare first-possible-match)
(declare segment-match)
(declare segment-match-add)
(declare segment-match?)
(declare match-if)
(declare match-is)
(declare match-or)
(declare match-not)
(declare match-and)
(declare match-with-variable)
(declare pattern-matcher-main)



; Helper functions to do random shit
; ==========================================================================
(defn reject-empty-list ; consp in common-lisp 
    "Only accept a list if it isn't empty"
    [object]
    (and (list? object) (not (empty? object))))

(defn call-arg-on-remaining-args ; funcall
    "Calls an argument on the optionals provided after the initial argument
     eg: (call-arg-on-remaining-args #'* 2 2 2) OUTPUT: 8"
    [arg & remaining-args]
     (apply arg remaining-args)) ; apply doesn't work because it needs to end with a list

(defn find-duplicate-numbers 
    "Finds duplicate numbers in a list"
    [numbers]
        (->> numbers
            (frequencies)
            (filter (fn [[k v]] (> v 1)))
            (keys)))

(defn find-non-duplicate-numbers
    "Finds duplicate numbers in a list"
    [numbers]
        (->> numbers
            (frequencies)
            (filter (fn [[k v]] (= v 1)))
            (keys)))

(defn rest-between-two-indexes
    "Return the all values between two indexes
     Similar to substrings but for lists instead"
    [x-list x-start x-end]
     (let [reg-x x-list
           rev-x (reverse x-list)
           startnew x-start
           endnew x-end]
           (drop startnew reg-x)
           (drop (- endnew startnew) rev-x)
           (take (- endnew startnew) (find-duplicate-numbers (concat reg-x rev-x)))))

; Table of available bindings
; ==========================================================================
(def binding-table
    {:segment-match {'?* segment-match '?+ segment-match-add '?? segment-match? '?if match-if}
     :single-match  {'?is match-is '?or match-or '?and match-and '?not match-not}})

; Binding functions
; ==========================================================================
; Segments

(defn segment-match-add
    "Match one or more elements of input."
    [pattern input-var bindings]
    (segment-match pattern input-var bindings 1))

(defn segment-match 
    "Match against ?* pattern"
    ([pattern input-var bindings]
        (segment-match pattern input-var bindings 0))
    ([pattern input-var bindings start]
    (let [var (second (first pattern))  
          in-pattern (rest pattern)]
          (if (nil? in-pattern) (match-with-variable var input-var bindings)
          (let [pos (first-possible-match (first pattern) input-var start)]
            (if (nil? pos) (println "failure to find match")
                (let [try-again (pattern-matcher-main in-pattern (nthrest input-var pos)
                      (match-with-variable var (rest-between-two-indexes input-var 0 pos) bindings))] 
                ; Failure handling
                (if (nil? try-again) (println "Failure, brah try again.") 
                    (segment-match pattern input-var bindings (inc pos))) try-again)))))))

(defn segment-match?
    "Match zero or one element of input"
    [pattern input-var bindings]
    (let [var (second (first pattern))
          in-pattern (rest pattern)]
          (or (pattern-matcher-main (cons var in-pattern) input-var bindings)
              (pattern-matcher-main in-pattern input-var bindings))))

(defn matches-segment-pattern?  ; segment-pattern? 
    "Does this match (?+, ?-) "
    [pattern]
    (and (seq? pattern)
    (seq? (first pattern))
    (symbol? (ffirst pattern))))

(defn match-if
    "Tests for the pattern (?if expre) rest of sentence"
    [pattern input-var bindings]
    ([[v pred] pattern new-bindings (pattern-matcher-main v input-var bindings)]
        (if (or (= new-binding nil) (not ((resolve pred) input-var)))
        (println "Failed to find if var")
        new-bindings)))

; Singles
(defn match-is
    "Succeed and bind the var if the input is satisfied
     var is the list"
     [var input-var bindings]
     (let* [var-list (first var)
            pred (second var)
            find-new-binding (pattern-matcher-main var-list input-var bindings)]
        (if (or (nil? find-new-binding) (not (call-arg-on-remaining-args pred input-var)))
            (println "The binding has failed")
            find-new-binding)))

(defn match-or
    "If any one of the inputs match something in the list"
    [pattern input-var bindings]
    (if (nil? pattern) (println "There are no values that match")
    (let [new-binding (pattern-matcher-main (first pattern) input-var bindings)]
        (if (nil? new-binding)
            (match-or (rest pattern) input-var bindings) new-binding))))

(defn match-and
    "Succeed if all patterns match the input"
    [pattern input-var bindings]
    (cond
        (nil? bindings) (println "All inputs do not match the given patterns")
    :else
        (true? (match-and (rest pattern) input-var (pattern-matcher-main (first pattern) input-var bindings)))))

(defn match-not
    "No patterns match the input -- AKA nothing will ever bind using this function"
    [pattern input-var bindings]
    (if (match-or pattern input-var bindings) (println "Nothing matches, fam. Best of luck next time.")
        bindings))

; Segment matcher to binding-table
; ==========================================================================
(defn first-possible-match ; first-match-pos
    [first-pattern input start]
    "Find the first position that first-pattern can match with input,
     start = starting position. If first-pattern isn't a constant, we
     return the starting position" 
     (cond 
        (and (atom first-pattern) (not (is-variable first-pattern))) ; atoms are essential to this https://clojure.org/reference/atoms
        (pos-seq-satisfy (partial = first-pattern) input start)
        (<= start ( count input )) start    
            :else nil))         

(defn pos-seq-satisfy ; position
    "Find the first possibility where the sequence
     is satisified by the test"
     ([test column]
        (pos-seq-satisfy test column 0))
        ([test column start]
            (pos-seq-satisfy test (nthnext column start) start start))
            ([test column start current-position]
                (cond
                    (empty? column) nil
                    (test (first column)) current-position
                    :else (recur test (rest column) start (inc current-position)))))

;  Translators
; ==========================================================================
(defn rules-based-translator
    [input rules &key ])

; Pattern matcher 
; ==========================================================================
(defn is-variable ; variable-p
    [x]
    "Is x a variable that begins with ?"
    (and (symbol? x) (= \? (get (str x) 0))))

(defn segment-pattern ; segment-pattern-p
    [pattern]
    "Does this match ?* var :pattern"
    (and (reject-empty-list pattern) (reject-empty-list (first pattern))
    (symbol? (first (first pattern)))
    (single-function-matcher (first (first pattern)))))

(defn is-a-singular-pattern ; single-pattern-p
    [pattern]
    "Is this pattern a match with a single pattern"
    (and (reject-empty-list pattern)) (single-function-matcher (first pattern)))

(defn segment-matcher ; segment-matcher
    [pattern input-seg bindings]
    "Calls the correct function that corresponds to the input pattern"
    (call-arg-on-remaining-args (segment-function-matcher (ffirst pattern)))
        pattern input-seg bindings)

(defn single-function-pat-match ; single-matcher
    [pattern input bindings]
    "Call the correct function for a single pattern -- eg is, or etc"
    (call-arg-on-remaining-args (single-function-matcher (first pattern)))
        (rest pattern) input bindings)

(defn segment-function-matcher ; segment-match-fn
    [var]
    "When a symbol is input, find the corresponding segment/function for it"
    (when (symbol? var) (get var :segment-match)))

(defn single-function-matcher ; single-match-fn
    [x]
    "Call the right function for a single pattern"
    (when (symbol? x) (get x :single-match)))


(defn match-with-variable ; match-variable
    "Does the input-var match with the input? Returns ult-binding"
    [input-var input ult-binding]
    (let [binded-value  (ult-binding input-var)]
        (cond
            (nil? binded-value) (assoc ult-binding input-var input) ; if nil --> associate input-var with input so Nil
            (= input-var binded-value) ult-binding
                :else  ; if input-var equals end-binding --> return the binding
                (println "Failure to match-with-var"))))  

(defn pattern-matcher-main ; pat-match
    ([pattern binded-input & [optional other-bindings]]
    "match the patterns with input through a binding"
    (cond
        (= binded-input false) (println "failure to find a match")
        (is-variable pattern) (match-with-variable pattern binded-input other-bindings)
        (= pattern binded-input) other-bindings
        (segment-pattern pattern) (segment-matcher pattern binded-input other-bindings)
        (is-a-singular-pattern pattern) (single-function-pat-match pattern binded-input other-bindings)
        (and (reject-empty-list pattern) (reject-empty-list binded-input))
            (pattern-matcher-main (rest pattern) (rest binded-input)
                (pattern-matcher-main (first pattern) (first binded-input) other-bindings)))))

(pattern-matcher-main '(= x (?is ?n int)) '(= x 34))
