
(deftype Exp [op lhs rhs])  ; probably will not use

(defmulti get-lhs class)
(defmethod get-lhs Exp [expre]
    (.lhs expre))
(defmethod get-lhs clojure.lang.PersistentList [expre]
    (if (> (count expre) 3)
        fail
        (second expre)))


(defmulti get-op class)
(defmethod get-op Exp [expre]  ; probably will not use
    (.op expre))
(defmethod get-op clojure.lang.PersistentList [expre] 
    (if (> (count expre) 3)
        fail
        (first expre)))

(defmulti get-rhs class)
(defmethod get-rhs Exp [expre]  ; probably will not use
    (.rhs expre))
(defmethod get-rhs clojure.lang.PersistentList [expre] 
    (if (> (count expre) 3)
        fail
        (nth expre 2))

(defn construct [lst]
    (Exp. (second lst) (first lst) (nth lst 2)))

;----------
(defn no-unknown-var
"Returns true if all variables in expression are now known"
  [expre]
  (cond (unknown-parameter ) nil
    (not (seq? expre)) true
    (no-unknown-var (exp-lhs exp)) (no-unknown-var (exp-rhs exp))
    :else nil))

(defn one-unknown-var
"Returns the single unkown expression if only one exists"
  [expre]
  (cond (unknown-p exp) nil
    (not (seq? expre)) nil
    (no-unknown (exp-lhs exp))(one-unknown (exp-rhs exp))
    (no-unknown (exp-rhs exp))(one-unknown (exp-rhs exp))
    :else nil))

(defn unknown-parameter ;
  "Is the argument an unknown variable?"
  [expre]
  (cond 
    (= expre nil) true
    (symbol? expre) true
    (keyword? expre) true
    (= '() expre) true
    :else nil))

(defn print-equation ; Not working just use printf instead of formatting the bullshit
"Format and print the equation so we can
 see the student work"
  [header equation]
    (apply str (pre))) ; Complete prefix-to-infix-notation and this is complete

(defn solve
"Solve a system of equations by constraint propagation"
[equations known]
(or
  (some (fn [equation]
    (let [x (one-unknown-var equation)]
            
            (when x
              (let [answer  (solve-arithmetic
                            (isolate equation x))
                    action postwalk-replace]
                   
              (solve (action (get-lhs answer) (get-rhs answer)
                                        ; idk if the line below this is right, we'll see.
                                        (remove equation equations))
                      (cons answer known))))))
        equations)
  (do (println "foo")) ))