(ns joc.macros
  "Joy of Clojure Ch. 8: Macros"
  (:require [clojure.walk :as walk])
  (:import (java.io BufferedReader InputStreamReader)
           (java.net URL)))

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

;; 8.4 domain example
(defmacro domain [name & body]
  `{:tag :domain
    :attrs {:name (str '~name)}
    :content [~@body]})

(declare handle-things)

(defmacro grouping [name & body]
  `{:tag :grouping
    :attrs {:name (str '~name)}
    :content [~@(handle-things body)]})

(declare grok-attrs grok-props)

(defn handle-things [things]
  (for [t things]
    {:tag :thing
     :attrs (grok-attrs (take-while (comp not vector?) t))
     :content (if-let [c (grok-props (drop-while (comp not vector?) t))]
                [c]
                [])}))

(defn grok-attrs [attrs]
  (into {:name (str (first attrs))}
        (for [a (rest attrs)]
          (cond
           (list? a) [:isa (str (second a))]
           (string? a) [:comment a]))))

(defn grok-props [props]
  (when props
    {:tag :properties
     :attrs nil
     :content (apply vector (for [p props]
                              {:tag :property
                               :attrs {:name (str (first p))}
                               :content nil}))}))

(def d
  (domain man-vs-monster
          (grouping people
                    (Human "A stock Human")

                    (Man (isa Human)
                         "A man, baby"
                         [name]
                         [has-beard?]))
          (grouping monsters
                    (Chupacabra
                     "A fierce, yet elusive creature"
                     [eats-goats?]))))

(:tag d)
(:tag (first (:content d)))

(clojure.xml/emit d)

;; 8.5
(defmacro resolution [] `x)

(macroexpand '(resolution))

(def 9 x)
(let [x 109] (resolution)) ;;=> 9

(defmacro awhen [expr & body]
  `(let [~'it ~expr]
     (if ~'it
       (do ~@body))))

(macroexpand '(awhen [1 2 3] (it 2)))
(awhen [1 2 3] (it 2))

(macroexpand '(awhen 1 (awhen 2 [it])))
(awhen 1 (awhen 2 [it]))

(defn joc-www []
  (-> "http://www.joyofclojure.com/hello" URL.
      .openStream
      InputStreamReader.
      BufferedReader.))

(let [stream (joc-www)]
  (with-open [page stream]
    (println (.readLine page))
    (print "The stream will now close ..."))
  (println "But lets read from it anyway.")
  (.readLine stream))

(defmacro with-resource [binding close-fn & body]
  `(let ~binding
     (try
       (do ~@body)
       (finally
         (~close-fn ~(binding 0))))))

(let [stream (joc-www)]
  (with-resource [page stream]
    #(.close %)
    (.readLine page)))

;; 8.7
(declare collect-bodies)
(defmacro contract [name & forms]
  (list* `fn name (collect-bodies forms)))

(declare build-contract)
(defn collect-bodies [forms]
  (for [form (partition 3 forms)]
    (build-contract form)))

(defn build-contract [c]
  (let [args (first c)]
    (list
     (into '[f] args)
     (apply merge
            (for [con (rest c)]
              (cond (= (first con) 'require)
                    (assoc {} :pre (vec (rest con)))
                    (= (first con) 'ensure)
                    (assoc {} :post (vec (rest con)))
                    :else (throw (Exception.
                                  (str "Unknown tag "
                                       (first con)))))))
     (list* 'f args))))

(def doubler-contract
  (contract doubler
            [x]
            (require
             (pos? x))
            (ensure
             (= (* 2 x) %))))

(def times2 (partial doubler-contract #(* 2 %)))
(times2 9)

(def times3 (partial doubler-contract #(* 3 %)))
(times3 9)
