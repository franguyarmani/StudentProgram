
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