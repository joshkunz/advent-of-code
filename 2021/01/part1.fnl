(local fun (require :fun))
(local fennel (require :fennel))

(lambda solve [pts]
 ; solve-one returns 0 on equal/decrement, and 1 on increment
 (let [solve-one (lambda solve-one [prev current]
                  (if 
                   (> current prev) 1
                                    0))
      ; increments is an iterator over the 1/0 increments for each point.
      increments  (lambda increments []
                   (fun.map #(solve-one $1 $2) (fun.zip pts (fun.cdr pts))))]
  (if
   ; If we have less than 2 points, there can be no increment
   (< (length pts) 2) 0
   ; Otherwise, sum all the increments.
   (accumulate [sum 0
                _ v (increments)]
    (+ sum v)))))

(->
 (icollect [v (io.stdin:lines)]
  (tonumber v))
 (solve)
 (print))

