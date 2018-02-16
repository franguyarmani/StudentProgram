w; Created by: Erik Whipp 
; &copy; 2018
; Ain't got no license, doe muhfucka


; FUNCTIONS THAT MUST BE IMPLEMENT
; -- segments
; ---- multiply
; ---- questions
; ---- addition
; ---- if
; -- singles
; ---- is
; ---- or
; ---- and
; ---- not
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

(find-duplicate-numbers (concat '(1 2 3 4 5) '(1 2 3)))

(find-non-duplicate-numbers
    "Finds duplicate numbers in a list"
    [numbers]
        (->> numbers
            (frequencies)
            (filter (fn [[k v]] (= v 1)))
            (keys))))

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

           (rest-between-two-indexes '(1 2 3 4 5) 1 4)
; Table of available bindings
; ==========================================================================
(def binding-table
    {:segment-match {'?* segment-match '?+ segment-match-add '?? segment-match? '?if match-if}
     :single-match  {'?is match-is '?or match-or '?and match-and '?not match-not}})

; Binding functions
; ==========================================================================
; Segments
(defn segment-match ; Need pattern match
    "Match against ?* pattern"
    [pattern input-var bindings & optionals (start 0)]
    (let [var (second (first pattern))
          in-pattern (rest pattern)]
          (if (nil? in-pattern) (match-with-variable var input-var bindings)
          (let [pos (first-possible-match (first pattern) input-var start)]
            (if (nil? pos) (println "failure to find match")
                (let [try-again (pattern-matcher-main in-pattern (nthrest input-var pos)
                      (match-with-variable var (nthrest input )) )] ))))))

(defn segment-match-add)
    
(defn segment-match?)

(defn matches-segment-pattern?  ; segment-pattern? 
    "Does this match (?+, ?-) "
    [pattern]
    (and (seq? pattern)
    (seq? (first pattern))
    (symbol? (ffirst pattern))))


(defn match-if)

; Singles
(defn match-is)

(defn match-or)

(defn match-and)

(defn match-not)

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
; Pattern matcher 
; ==========================================================================
(defn is-variable ; variable-p
    [x]
    "Is x a variable that begins with ?"
    (and (symbol? x) (= \? (first (name x)))))

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