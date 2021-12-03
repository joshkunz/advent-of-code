(local fun (require :fun))
(local fennel (require :fennel))

(lambda oxygen-generator [{:0 zero :1 one}]
 (if (> zero one) :0 :1))

(lambda co2-scrubber [{:0 zero :1 one}]
 (if (< one zero) :1 :0))

(lambda freq-table [size]
 {:freqs (icollect [_ (fun.range size)]
          {:0 0 :1 0})
  :inc-digit (lambda [tbl position value]
              (let [cur (. tbl :freqs position value)]
               (tset (. tbl :freqs position) value (+ cur 1))))
  :update (lambda [tbl input]
           (each [pos digit (fun.iter input)]
            (tbl:inc-digit pos digit)))
  :view (lambda [tbl]
         (fennel.view tbl.freqs))
  :freq (lambda [tbl position rule]
         (rule (. tbl :freqs position)))})

(lambda build-table [in]
 (let [digits (length (. in 1))
       freqs  (freq-table digits)]
  (each [_ v (ipairs in)]
   (freqs:update v))
  freqs))

(lambda rating [in rule]
 (let [digits (length (. in 1))]
  (->
   (accumulate [possible in
                position (fun.range digits)
                :until (= (length possible) 1)]
    (let [t (build-table possible)
          check (string.byte (t:freq position rule))]
     (icollect [_ v (ipairs possible)]
      (when (= (string.byte v position) check) v))))
   (#(. $1 1)))))

(lambda ratings [vs]
 {:oxygen-generator (rating vs oxygen-generator)
  :co2-scrubber     (rating vs co2-scrubber)})

(lambda solve [{: oxygen-generator : co2-scrubber }]
 (* (tonumber oxygen-generator 2) (tonumber co2-scrubber 2)))

(lambda trace [v]
 (print (fennel.view v))
 v)

(->
 (icollect [l (io.stdin:lines)] l)
 (ratings)
 (trace)
 (solve)
 (print))
