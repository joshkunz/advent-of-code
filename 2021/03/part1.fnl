(local fun (require :fun))
(local fennel (require :fennel))

(lambda gamma [{:0 zero :1 one}]
 (if (< zero one) :1 :0))

(lambda epsilon [{:0 zero :1 one}]
 (if (< zero one) :0 :1))

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
  :rate (lambda [tbl f]
         (->
          (icollect [_ v (ipairs tbl.freqs)]
           (f v))
          (table.concat "")
          (tonumber 2)))})

(lambda build-table [in]
 (let [digits (length (. in 1))
       freqs  (freq-table digits)]
  (each [_ v (ipairs in)]
   (freqs:update v))
  freqs))

(lambda solve [freqs]
 (* (freqs:rate gamma) (freqs:rate epsilon)))

(lambda info [t]
 (print "rates:" (t:view))
 (print "gamma:" (t:rate gamma))
 (print "epsilon:" (t:rate epsilon))
 t)
  
(->
 (icollect [l (io.stdin:lines)] l)
 (build-table)
 (info)
 (solve)
 (print))
