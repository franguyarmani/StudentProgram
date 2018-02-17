(defn is-variable ; variable-p
    [x]
    "Is x a variable that begins with ?"
    (and (symbol? x) (= \? (get (str x) 0))))


(defn pat-match-simple [pattern input]
    (cond (= pattern input) true
        (or (is-variable (first pattern))(= (first pattern)(first input))) 
                (pat-match-simple (rest pattern) (rest input))
        :else false))


