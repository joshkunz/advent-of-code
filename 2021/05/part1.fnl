(local fun (require :fun))
(local fennel (require :fennel))

(lambda parse-range [line]
 (let [(x1 y1 x2 y2) (string.match line "(%d+),(%d+) %-> (%d+),(%d+)")]
  {:from {:x (tonumber x1) :y (tonumber y1)}
   :to   {:x (tonumber x2) :y (tonumber y2)}}))

(lambda horizontal? [{:from {:y y1} :to {:y y2}}]
 (= y1 y2))

(lambda vertical? [{:from {:x x1} :to {:x x2}}]
 (= x1 x2))

(lambda valid? [r]
 (or (horizontal? r) (vertical? r)))

(lambda horizontal-points [{:from {:x x1 : y} :to {:x x2}}]
 (icollect [v (fun.range x1 x2)]
  {:x v : y}))

(lambda vertical-points [{:from {: x :y y1} :to {:y y2}}]
 (icollect [v (fun.range y1 y2)]
  {: x :y v}))

(lambda points [r]
 (if
  (horizontal? r) (horizontal-points r)
  (vertical? r) (vertical-points r)))

(lambda ranges-points [rs]
 (icollect [_ r (ipairs rs)]
  (let [ps (points r)]
   ps)))

(lambda combine-points [pss]
 (var combined {})
 (each [_ ps (ipairs pss)]
  (each [_ {: x : y} (ipairs ps)]
   (let [cur (?. combined x y)
         val (if cur (+ cur 1) 1)]
    (if
     (. combined x) (tset (. combined x) y val)
                    (tset combined x {y val})))))
 combined)

(lambda count-overlap [combined]
 (var sum 0)
 (each [_ ys (pairs combined)]
  (each [_ count (pairs ys)]
   (when (> count 1)
    (set sum (+ sum 1)))))
 sum)

(->
 (icollect [l (io.stdin:lines)]
  (let [r (parse-range l)]
   (when (valid? r) r)))
 (ranges-points)
 (combine-points)
 (count-overlap)
 (fennel.view)
 (print))
