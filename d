[1mdiff --git a/src/student/student.clj b/src/student/student.clj[m
[1mindex 15ec090..2ea704f 100644[m
[1m--- a/src/student/student.clj[m
[1m+++ b/src/student/student.clj[m
[36m@@ -6,6 +6,8 @@[m
 [m
 ;; Begin Pattern Matching[m
 ;; ================================================================================[m
[32m+[m[32m(declare unknown-parameter)[m
[32m+[m
 (def comma[m
   (symbol ","))[m
 [m
[36m@@ -17,8 +19,7 @@[m
    (let [idx (.indexOf (nthrest seq start) item)][m
      (if (>= idx 0)[m
        (+ start idx)[m
[31m-       -1)))[m
[31m-  ([seq item] (.indexOf seq item)))[m
[32m+[m[32m       -1))))[m
 [m
 [m
 (def fail[m
[36m@@ -59,7 +60,9 @@[m
 [m
 (defn variable?[m
   [x][m
[31m-  (and (symbol? x) (= \? (get (str x) 0))))[m
[32m+[m[32m  (or (and[m[41m [m
[32m+[m[32m    (unknown-parameter x) (= \? (get (str x) 0)))[m
[32m+[m[32m  )[m
 [m
 (defn get-binding[m
   "Find a variable->value binding in the given binding."[m
[36m@@ -132,7 +135,7 @@[m
    at position start. If pat1 is non-constant, then just return start,[m
    conservatively assuming it could match."[m
   [pat1 input start][m
[31m-  (cond (and (not (list? pat1))[m
[32m+[m[32m  (cond (and (not (seq? pat1))[m
              (not (variable? pat1)))[m
     (let [idx (index-in-seq input pat1 start)][m
         (if (< idx 0)[m
[36m@@ -264,7 +267,7 @@[m
 (def abbreviation-table[m
   (atom {}))[m
 [m
[31m-(declare unknown-parameter)[m
[32m+[m
 [m
 (defn expand-pat-match-abbrev[m
   "Expand all pattern matching abbreviations in pat"[m
[36m@@ -332,7 +335,7 @@[m
       (return-inverse-operation operation (rest inverse-poss) ) (second (first inverse-poss)))))[m
 [m
 (defn unknown-parameter ;[m
[31m-  "Is the argument an unknown variable?"[m
[32m+[m[32m  "Is the argument an unknown variable? (implements symbolp)"[m
   [expre][m
   (cond[m
     (= expre nil) true[m
[36m@@ -379,40 +382,43 @@[m
     ~['(?x is ?y)  '(= ?x ?y)][m
   ])[m
 [m
[31m-(def ^:dynamic *student-rules*[m
[31m-  '(((?x* .) ?x)[m
[31m-     ((?x* . ?y*)  (?x ?y))[m
[31m-     ((if ?x* \, then ?y*) (?x ?y))[m
[31m-     ((if ?x* then ?y*) (?x ?y))[m
[31m-     ((if ?x* \, ?y*) (?x ?y))[m
[31m-     ((?x* \, and ?y*) (?x ?y))[m
[31m-     ((find ?x* and ?y*) ((= to-find-1 ?x) (to-find-2 ?y)))[m
[31m-     ((find ?x*)  (to-find ?x))[m
[31m-     ((?x* equals ?y*)  (?x ?y))[m
[31m-     ((?x* same as ?y*)  (?x ?y))[m
[31m-     ((?x* = ?y*)  (?x ?y))[m
[31m-     ((?x* is equal to ?y*)  (?x ?y))[m
[31m-     ((?x* is ?y*) (?x ?y))[m
[31m-     ((?x* - ?y*) (- ?x ?y))[m
[31m-     ((?x* minus ?y*) (- ?x ?y))[m
[31m-     ((difference between ?x* and ?y*) (- ?y ?x))[m
[31m-     ((difference ?x* and ?y*) (- ?y ?x))[m
[31m-     ((?x* + ?y*) (+ ?x ?y))[m
[31m-     ((?x* plus ?y*) (+ ?x ?y))[m
[31m-     ((sum ?x* and ?y*) (+ ?x ?y))[m
[31m-     ((product ?x* and ?y*) (* ?x ?y))[m
[31m-     ((?x* * ?y*) (* ?x ?y))[m
[31m-     ((?x* times ?y*) (* ?x ?y))[m
[31m-     ((?x* / ?y*) (/ ?x ?y))[m
[31m-     ((?x* per ?y*) (/ ?x ?y))[m
[31m-     ((?x* divided by ?y*) (/ ?x ?y))[m
[31m-     ((half ?x*) (/ ?x 2))[m
[31m-     ((one half ?x*) (/ ?x 2))[m
[31m-     ((twice ?x*) (* 2 ?x))[m
[31m-     ((square ?x*) (* ?x ?x))[m
[31m-     ((?x* % less than ?y*) (* ?y (/ (- 100 ?x) 100)))[m
[31m-     ((?x* % more than ?y*) (* ?y (/ (+ 100 ?x) 100)))[m
[31m-     ((?x* % ?y*) (* (/ ?x 100) ?y))))[m
[32m+[m[32m  (def ^:dynamic *basic-student-rules*[m[41m [m
[32m+[m[32m    `[[m
[32m+[m[32m      ~['(?x* .)                            '?x][m
[32m+[m[32m      ~['(?x* . ?y*)                   '(?x ?y)][m
[32m+[m[32m      ;~[(list 'if '?x* comma 'then '?y*)  '(?x ?y)][m
[32m+[m[32m      ~['(if ?x* then ?y*)            '(?x ?y)][m
[32m+[m[32m      ;~[(list 'if '?x* comma '?y*)    '(?x ?y)][m
[32m+[m[32m      ;~[(list '?x* comma 'and '?y*)      '(?x ?y)][m
[32m+[m[32m      ~['(find ?x* and ?y*)     '((= to-find-1 ?x) (= to-find-2 ?y))][m
[32m+[m[32m      ~['(find ?x*)             '(= to-find ?x)][m
[32m+[m[32m      ~['(?x* equals ?y*)       '(= ?x ?y)][m
[32m+[m[32m      ~['(?x* same as ?y*)      '(= ?x ?y)][m
[32m+[m[32m      ~['(?x* = ?y*)            '(= ?x ?y)][m
[32m+[m[32m      ~['(?x* is equal to ?y*)  '(= ?x ?y)][m
[32m+[m[41m    [m
[32m+[m[32m      ~['(?x* is ?y*)           '(= ?x ?y)][m
[32m+[m[32m      ~['(?x* - ?y*)            '(- ?x ?y)][m
[32m+[m[32m      ~['(?x* minus ?y*)        '(- ?x ?y)][m
[32m+[m[32m      ~['(difference between ?x* and ?y*)  '(- ?y ?x)][m
[32m+[m[32m      ~['(difference ?x* and ?y*)          '(- ?y ?x)][m
[32m+[m[32m      ~['(?x* + ?y*)            '(+ ?x ?y)][m
[32m+[m[32m      ~['(?x* plus ?y*)         '(+ ?x ?y)][m
[32m+[m[32m      ~['(sum ?x* and ?y*)      '(+ ?x ?y)][m
[32m+[m[32m      ~['(product ?x* and ?y*)  '(* ?x ?y)][m
[32m+[m[32m      ~['(?x* * ?y*)            '(* ?x ?y)][m
[32m+[m[32m      ~['(?x* times ?y*)        '(* ?x ?y)][m
[32m+[m[32m      ~['(?x* / ?y*)            '(/ ?x ?y)][m
[32m+[m[32m      ~['(?x* per ?y*)          '(/ ?x ?y)][m
[32m+[m[32m      ~['(?x* divided by ?y*)   '(/ ?x ?y)][m
[32m+[m[32m      ~['(half ?x*)             '(/ ?x 2)][m
[32m+[m[32m      ~['(one half ?x*)         '(/ ?x 2)][m
[32m+[m[32m      ~['(twice ?x*)            '(* 2 ?x)][m
[32m+[m[32m      ~['(square ?x*)           '(* ?x ?x)][m
[32m+[m[32m      ~['(?x* % less than ?y*)  '(* ?y (/ (- 100 ?x) 100))][m
[32m+[m[32m      ~['(?x* % more than ?y*)  '(* ?y (/ (+ 100 ?x) 100))][m
[32m+[m[32m      ~['(?x* % ?y*)            '(* (/ ?x 100) ?y)][m
[32m+[m[32m    ])[m
 [m
 (defn map-expand-to-rules[m
   "Expand all the rules to allow us to actually match/translate"[m
