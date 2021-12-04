(local fun (require :fun))
(local fennel (require :fennel))

; Assuming
;  * Every value is unique in a board (no two positions share a value)
;  * Every board is square
;  * Every board is the same size
; Can maybe assume 
;  * Every value exists on every board

(fn nil? [v] (= v nil))

(lambda make-set []
 {:size 0
  :values {}
  :add (lambda [tbl v]
        (when (nil? (. tbl.values v))
         (tset tbl.values v true)
         (tset tbl :size (+ tbl.size 1))))})
         

(lambda make-play [board]
 {:board board
  ; We use these tables as sets to track the marks in each column / row
  :tracks {:by-row (icollect [_ (fun.range board.dimension)] (make-set))
           :by-col (icollect [_ (fun.range board.dimension)] (make-set))}
  :marks {}
  :view (lambda [tbl] (fennel.view {:board {:dimension tbl.board.dimension
                                            :cells tbl.board.cells}
                                    :tracks tbl.tracks}))
  :mark   (lambda mark [tbl value]
           ; mark marks the given value if it exists in the table.
           (let [(exists {: row : col}) (tbl.board:locate value)]
            (when exists
             (tset tbl.marks value true)
             (-> tbl.tracks.by-row (. row) (: :add value))
             (-> tbl.tracks.by-col (. col) (: :add value)))))
  :unmarked (lambda unmarked [tbl]
             (icollect [_ v (ipairs tbl.board.values)]
              (when (not (. tbl.marks v)) v)))
  :bingo? (lambda bingo? [tbl] 
           (let [check-marks (lambda check-marks [tracks]
                              (accumulate [found false
                                           _ marks (ipairs tracks)
                                           :until found]
                               (= marks.size board.dimension)))]
            (or
             ; Bingo if we have board.dimension marks in any
             ; column or row table tracking table.
             (check-marks tbl.tracks.by-row)
             (check-marks tbl.tracks.by-col))))})

(lambda make-board [m]
 ; m is a row-major index of the parsed nxn board.
 {:dimension (length m)
  :cells m
  :values (let [t []]
           (each [_ row (ipairs m)]
            (each [_ v (ipairs row)]
             (table.insert t v)))
           t)
  ; locations maps values to their location in the board, for O(1) lookup.
  :locations (let [l {}]
              (for [row 1 (length m)]
               (for [col 1 (length m)]
                (tset l (. m row col) {:row row :col col})))
              l)
  :view (lambda [tbl] (fennel.view tbl.cells))
  :locate (lambda locate [tbl v]
           (let [found (. tbl.locations v)]
            (values
             (not (nil? found))
             (if
              (nil? found) {:row nil :col nil}
              found))))})

(lambda parse-board [raws]
 ; Parse a list of strings, where each string is a line of numbers into a board.
 (-> 
  (icollect [_ raw (ipairs raws)]
   (icollect [v (string.gmatch raw "%d+")]
    (tonumber v)))
  (make-board)))

(lambda read-puzzle []
 (let [lines (icollect [l (io.stdin:lines)] l)]
  {:draws (icollect [v (string.gmatch (. lines 1) "%d+")]
           (tonumber v))
   :boards (->
            (accumulate [{: boards : current} {:boards [] :current []}
                         _ line (fun.drop 2 lines)]
             (if
              (= line "") (do
                           (->> current
                            (parse-board)
                            (table.insert boards))
                           {:boards boards :current []})
              (do
               (table.insert current line)
               {:boards boards :current current})))
            ; Then cleanup the form, and parse the final board.
            ((lambda [{: boards : current}]
              (->> current
               (parse-board)
               (table.insert boards))
              boards)))
   :view (lambda [tbl]
          (print "Draws:" (fennel.view tbl.draws))
          (each [_ board (ipairs tbl.boards)]
           (print "Board:" (board:view))))}))

(lambda run [game]
 (let [plays (icollect [_ board (ipairs game.boards)]
              (make-play board))]
  (accumulate [winner nil
               _ draw (ipairs game.draws)
               :until (not (nil? winner))]
   (accumulate [winner nil
                idx play (ipairs plays)
                :until (not (nil? winner))]
    (do
     (play:mark draw)
     (when (play:bingo?)
      (print "Bingo on" idx "at" draw)
      {:play play :draw draw}))))))

(lambda score [{: draw : play}]
 (* (fun.sum (play:unmarked)) draw))

(->
 (read-puzzle)
 (run)
 (score)
 (print))
