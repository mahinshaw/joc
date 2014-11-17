(ns joc.macros
  "Joy of Clojure Ch. 8: Macros"
  (:require [clojure.walk :as walk]))

(defmacro do-until [& clauses]
  "This macro is similar to 'cond', except that it does actions until false."
  (when clauses
    (list 'clojure.core/when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                    "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

(do-until
 (even? 2) (println "Even")
 (odd? 3) (println "Odd")
 (zero? 1) (println "You never see me")
 :lollipop (println "Truthy thing"))

(macroexpand-1 '(do-until true (prn 1) false (prn 2)))

(walk/macroexpand-all '(do-until true (prn 1) false (prn 2)))

(defmacro unless [condition & body]
  `(if (not ~condition)
     (do ~@body)))

(unless (even? 2) (prn 1))

(macroexpand-1 '(unless (even? 2) (prn 1)))
