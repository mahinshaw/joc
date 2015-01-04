(ns joc.concurrency)

(defn manipulate-memoize
  "Alter the core memoize to apply the atom to the calling function.
  10.4.2"
  [function]
  (let [cache (atom {})]
    (with-meta
      (fn [& args]
        (or (second (find @cache args))
            (let [ret (apply function args)]
              (swap! cache assoc args ret)
              ret)))
      {:cache cache})))

(def slowly (fn [x] (Thread/sleep 1000) x))

(time [(slowly 9) (slowly 9)])

(def sometimes-slowly (manipulate-memoize slowly))

(time [(sometimes-slowly 108) (sometimes-slowly 108)])

(meta sometimes-slowly)

(let [cache (:cache (meta sometimes-slowly))]
  (swap! cache dissoc '(108)))
