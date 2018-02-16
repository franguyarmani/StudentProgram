(ns uk.co.fatvat.student
    (:use clojure.walk)
    (:use clojure.test))

(def basic-student-rules
  `[
    ~['(?x* .) '?x]
    ~['(?x* . ?y*) '(?x ?y)]
    ~[(list 'if '?x* (symbol ",") 'then '?y*)  '(?x ?y)]
    ~['(if ?x* then ?y*)      '(?x ?y)]
    ~[(list 'if '?x* (symbol ",") '?y*)       '(?x ?y)]
    ~['(?x* and ?y*)      '(?x ?y)]
    ~['(find ?x* and ?y*)     '((= to-find-1 ?x) (= to-find-2 ?y))]
    ~['(find ?x*)             '(= to-find ?x)]
    ~['(?x* equals ?y*)       '(= ?x ?y)]
    ~['(?x* same as ?y*)      '(= ?x ?y)]
    ~['(?x* = ?y*)            '(= ?x ?y)]
    ~['(?x* is equal to ?y*)  '(= ?x ?y)]
    ~['(?x* is ?y*)           '(= ?x ?y)]
    ~['(?x* - ?y*)            '(- ?x ?y)]
    ~['(?x* minus ?y*)        '(- ?x ?y)]
    ~['(difference between ?x* and ?y*)  '(- ?y ?x)]
    ~['(difference ?x* and ?y*)          '(- ?y ?x)]
    ~['(?x* + ?y*)            '(+ ?x ?y)]
    ~['(?x* plus ?y*)         '(+ ?x ?y)]
    ~['(sum ?x* and ?y*)      '(+ ?x ?y)]
    ~['(product ?x* and ?y*)  '(* ?x ?y)]
    ~['(?x* * ?y*)            '(* ?x ?y)]
    ~['(?x* times ?y*)        '(* ?x ?y)]
    ~['(?x* / ?y*)            '(/ ?x ?y)]
    ~['(?x* per ?y*)          '(/ ?x ?y)]
    ~['(?x* divided by ?y*)   '(/ ?x ?y)]
    ~['(half ?x*)             '(/ ?x 2)]
    ~['(one half ?x*)         '(/ ?x 2)]
    ~['(twice ?x*)            '(* 2 ?x)]
    ~['(square ?x*)           '(* ?x ?x)]
    ~['(?x* % less than ?y*)  '(* ?y (/ (- 100 ?x) 100))]
    ~['(?x* % more than ?y*)  '(* ?y (/ (+ 100 ?x) 100))]
    ~['(?x* % ?y*)            '(* (/ ?x 100) ?y)]])
  