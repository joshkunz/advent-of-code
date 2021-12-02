(local fennel (require :fennel))

(lambda parse [command]
 (let [(op value) (string.match command "(%l+)%s(%d+)")]
  {:operation op :value (tonumber value)}))

(lambda update [{: aim : depth : position} {: operation : value}]
 (match operation 
  "forward" {:aim aim
             ; Saturate our new depth in-case our aim is negative.
             :depth (let [new-depth (+ depth (* aim value))]
                     (if (< new-depth 0) 0 new-depth))
             :position (+ position value)}
  "down"    {:aim (+ aim value)
             :depth depth
             :position position}
  "up"      {:aim (- aim value)
             :depth depth
             :position position}))

(lambda answer [{: depth : position}] (* depth position))

(local initial-state {:aim 0 :depth 0 :position 0})

(->
 (accumulate [state initial-state
              line (io.stdin:lines)]
  (->> line
   (parse)
   (update state)))
 (answer)
 (fennel.view)
 (print))
