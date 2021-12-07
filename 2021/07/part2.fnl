(local fun (require :fun))
(local fennel (require :fennel))

(lambda parse-input [line]
 (icollect [v (string.gmatch line "%d+")]
  (tonumber v)))

(lambda trace [v ...]
 (print ... (fennel.view v))
 v)

(lambda fuel-to [positions position]
 (lambda fuel-for [n-moves]
  ; https://en.wikipedia.org/wiki/1_%2B_2_%2B_3_%2B_4_%2B_⋯
  (/ (* n-moves (+ n-moves 1)) 2))
 ; Calculate the amount of fuel required to move all the ships
 ; to the given position.
 (let [move-cost #(-> $1 (- position) (math.abs) (fuel-for))]
  (fun.sum (fun.map move-cost positions))))

(lambda cheapest-position [ships]
 (let [min-position (math.min (table.unpack ships))
       max-position (math.max (table.unpack ships))]
  (accumulate [cheapest {:position -1
                         :cost math.huge}
               _ position (fun.range min-position max-position)]
   (let [cost (fuel-to ships position)]
    (if (< cost cheapest.cost)
     {: position : cost}
     cheapest)))))

(->
 ((io.stdin:lines))
 (parse-input)
 (trace "Input:")
 (cheapest-position)
 (trace "Minimization:")
 (#$1.cost)
 (trace "Solution:"))

