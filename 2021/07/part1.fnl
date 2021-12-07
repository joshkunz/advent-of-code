(local fun (require :fun))
(local fennel (require :fennel))

(lambda parse-input [line]
 (icollect [v (string.gmatch line "%d+")]
  (tonumber v)))

(lambda trace [v ...]
 (print ... (fennel.view v))
 v)

(lambda fuel-to [positions position]
 ; Calculate the amount of fuel required to move all the ships
 ; to the given position.
 (fun.sum (fun.map #(math.abs (- $1 position)) positions)))

(lambda cheapest-position [ships]
 (accumulate [cheapest {:position -1
                        :cost math.huge}
              _ position (ipairs ships)]
  (let [cost (fuel-to ships position)]
   (if (< cost cheapest.cost)
    {: position : cost}
    cheapest))))

(->
 ((io.stdin:lines))
 (parse-input)
 (trace "Input:")
 (cheapest-position)
 (trace "Minimization:")
 (#$1.cost)
 (trace "Solution:"))

