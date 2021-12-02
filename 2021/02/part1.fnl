(local fennel (require :fennel))

(lambda parse [command]
 (let [(op value) (string.match command "(%l+)%s(%d+)")]
  {:operation op :value (tonumber value)}))

(lambda saturating-sub [a b]
 (let [result (- a b)]
  (if (< result 0) 0 result)))

(lambda update [{: depth : position} {: operation : value}]
 (match operation 
  "forward" {:depth depth                        :position (+ position value)}
  "down"    {:depth (+ depth value)              :position position}
  "up"      {:depth (saturating-sub depth value) :position position}))

(lambda answer [{: depth : position}] (* depth position))

(local initial-state {:depth 0 :position 0})

(->
 (accumulate [state initial-state
              line (io.stdin:lines)]
  (->> line
   (parse)
   (update state)))
 (answer)
 (fennel.view)
 (print))
