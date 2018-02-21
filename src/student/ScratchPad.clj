
(deftype Exp [op lhs rhs])

(defmulti get-lhs class)
(defmethod get-lhs Exp [expre]
    (.lhs expre))
(defmethod get-lhs clojure.lang.PersistentList [expre]
    (second expre))

(defmulti get-op class)
(defmethod get-op Exp [expre]
    (.op expre))
(defmethod get-phs clojure.lang.PersistentList [expre]
    (first expre))

(defmulti get-rhs class)
(defmethod get-rhs Exp [expre]
    (.rhs expre))
(defmethod get-lhs clojure.lang.PersistentList [expre]
    (nth expre 2))

(defn construct [lst]
    (Exp. (second lst) (first lst) (nth lst 2)))