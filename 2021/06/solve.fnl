(local fun (require :fun))
(local fennel (require :fennel))

; +1 because we also count 0 as a valid timer.
(local *max-timer* (+ 8 1))

(lambda parse-timers [l]
 (let [counts (icollect [_ (fun.range *max-timer*)] 0)]
  (each [n (string.gmatch l "%d+")]
   (let [n-index (+ n 1)
         cur (. counts n-index)]
    (tset counts n-index (+ cur 1))))
  counts))

(lambda step [timers]
 (let [
  [ zero one two three four five six seven eight ] timers]
  ; Shift every lifecycle forward by 1 day
  [ one
    two
    three
    four
    five
    six
    ; Except for stage six, which is composed of both newly "reset"
    ; lanternfish that just produced new fish.
    (+ seven zero)
    eight
    ; And except for 8 which is composed entirely of newly created fish
    ; from the stage 0 group.
    zero ]))

(lambda simulate [timers days]
 (accumulate [state timers
              _ (fun.range days)]
  (step state)))

(lambda count-fish [timers]
 (fun.sum timers))

(lambda trace [v]
 (print (fennel.view v))
 v)

(lambda part [name {: days} timers]
 (-> timers
  (simulate days)
  (count-fish)
  (#(print (.. name ":")  $1)))
 timers)

(lambda part2 [timers]
 (print "part2:" (simulate timers 256)
 timers))

(->
 ((io.stdin:lines))
 (parse-timers)
 (trace)
 ((partial part "part1" {:days 80}))
 ((partial part "part2" {:days 256})))
