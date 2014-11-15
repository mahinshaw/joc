(ns joc.core
  (:use clojure.test))

;; CH 6
;; lazy quicksort
(defn rand-ints [n]
  "Get 'n' random integers from 0 to n inclusive"
  (take n (repeatedly #(rand-int n))))

(defn sort-parts [work]
  (lazy-seq                                ;; Returns a lazy sequence
   (loop [[part & parts] work]             ;; loop -> destruct work into head (part) and tail (parts)
     (if-let [[pivot & xs] (seq part)]     ;; create a seq out of part and destruct, if we dont have a seq in part, the continue.
       (let [smaller? #(< % pivot)]        ;; let smaller? = the function that compares a value with the pivot.
         (recur (list*                     ;; list* takes n lists and packs them into the last passed list.
                 (filter smaller? xs)      ;; put the smaller parts of xs into a list
                 pivot                     ;; put the pivot into a list
                 (remove smaller? xs)      ;; put the larger parts into a list
                 parts)))                  ;; put all the previous lists into the parts list
       (when-let [[x & parts] parts]       ;; when we can split up parts
         (cons x (sort-parts parts)))))))  ;; cons the head into the sorted tail.

(defn qsort [xs]
  "Create a list out of xs, and sort it lazily."
  (sort-parts (list xs)))

;; CH 7
;; wrapping fnth in with-test macro is analogous to :test metadata
(with-test
  (defn fnth [n]
    (apply comp (cons first (take (dec n) (repeat rest)))))
  (is (= 4 ((fnth 2) [1 4 5 6])))
  (is (= 5 ((fnth 3) [1 4 5 6]))))

;; Using test meta data.
(defn join
  {:test (fn []
           (is ;; could use assert, but clojure.test/is has better output.
            (= (join "," [1 2 3]) "1,2,3")))}
  [sep s]
  (apply str (interpose sep s)))

;;; metadata-> 
(defn sum
  {:private true
   :dynamic true}
  [nums] (map + nums))

;; or
(defn ^:private ^:dynamic sum [nums] (map + nums))

;; or
(defn ^{:private true :dynamic true} sum [nums] (map + nums))

;; or
(defn sum ([nums] (map + nums)) {:private true :dynamic true})

;;; Pure Functions
(def plays [{:band "Burial" :plays 979 :loved 9}
            {:band "Eno" :plays 2333 :loved 15}
            {:band "Bill Evans" :plays 979 :loved 9}
            {:band "Magma" :plays 2665 :loved 31}])

(defn columns [column-names]
  (fn [row]
    (vec (map #(row %) column-names))))

;; Sort the columns based on an ordering of the maps then by the plays
;; (sort-by (columns [:plays :loved :band]) plays)

(defn keys-apply [f ks m]
  {:doc "Takes a function to map to the values of m,then reapplies the select keys to the new values."}
  (let [only (select-keys m ks)]
    (zipmap (keys only)
            (map f (vals only)))))

(defn manip-map [f ks m]
  (merge m (keys-apply f ks m)))

;; not referentially transparent like keys-apply and manip-map
(defn mega-love! [ks]
  (map (partial manip-map #(int (* % 1000)) ks) plays))

;;; Named Arguments
;; Destructuring with & allows for named params that are defined via maps.
(defn slope
  {:doc "Calculate slope of tuples p1 and p2."}
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

;;; Constraining functions with pre and post conditions
;; to turn off pre and post in a file, (set! *assert* false) somewhere near the top.
(defn slope [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)] ;; before we start we assert that p1 and p2 are not equal, then we asser that p1 and p2 are vectors.
   :post [(float? %)]} ;; after we assert that the return value is a float.
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))


;;; Recursion
(def simple-metric {:meter 1
                    :km 1000
                    :cm 1/100
                    :mm [1/10 :cm]})

(defn convert [context descriptor]
  (reduce (fn [result [mag unit]]
            (+ result
               (let [val (get context unit)]
                 (if (vector? val)
                   (* mag (convert context val))
                   (* mag val)))))
          0
          (partition 2 descriptor)))

(float (convert simple-metric [3 :km 10 :meter 80 :cm 10 :mm]))
