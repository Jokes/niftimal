#lang racket/gui

; digit-construction things

(define palette-lookup
  (make-hash
   (list
    (list "Ice" (make-color 236 245 255) (make-color 213 226 252) (make-color 189 206 247)
          (make-color 164 187 243) (make-color 148 159 225) (make-color 134 133 207)
          (make-color 119 109 188))
    (list "Sunset" (make-color 255 253 228) (make-color 249 230 192) (make-color 237 186 146)
          (make-color 220 134 97) (make-color 201 110 103) (make-color 179 88 110)
          (make-color 154 65 118))
    (list "Love" (make-color 255 245 236) (make-color 252 226 213) (make-color 247 206 189)
          (make-color 243 187 164) (make-color 225 159 148) (make-color 207 133 134)
          (make-color 188 109 119)))))
(define palette-choices (sort (hash-keys palette-lookup) string<?))

(define palette
  (hash-ref palette-lookup (first palette-choices)))
(define (get-fill-col n)
  (list-ref palette (modulo n (length palette))))

(define (zero-path x y s)
  (let* ([p (new dc-path%)]
         [hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send p move-to x (- y s))         ; 12:00
    (send p line-to (+ x ds) (- y hs)) ;  2:00
    (send p line-to (+ x ds) (+ y hs)) ;  4:00
    (send p line-to x (+ y s))         ;  6:00
    (send p line-to (- x ds) (+ y hs)) ;  8:00
    (send p line-to (- x ds) (- y hs)) ; 10:00
    (send p close)
    p))

(define (draw-zero dc x y s) ; the hexagon
  (send dc draw-path (zero-path x y s)))

(define (draw-one dc x y s) ; vertical central line
  (send dc draw-line x (- y s) x (+ y s)))

(define (draw-two dc x y s) ; low X
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line (- x ds) (- y hs) (+ x ds) (+ y hs)) ; 10 to 4
    (send dc draw-line (+ x ds) (- y hs) (- x ds) (+ y hs)) ; 2 to 8
    ))

(define (draw-three dc x y s)
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line x y x (- y s)) ; 12
    (send dc draw-line x y (+ x ds) (+ y hs)) ; 4
    (send dc draw-line x y (- x ds) (+ y hs)) ; 8
    ))

(define (draw-four dc x y s) ; branching rune
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line x (- y s) x (+ y s)) ; vertical
    (send dc draw-line x y (- x ds) (- y hs)) ; 10
    (send dc draw-line x y (+ x ds) (- y hs)) ; 2
    ))

(define (draw-five dc x y s) ; partial star
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line (- x ds) (- y hs) (+ x ds) (+ y hs)) ; 10 to 4
    (send dc draw-line (+ x ds) (- y hs) (- x ds) (+ y hs)) ; 2 to 8
    (send dc draw-line x y x (- y s)) ; 12
    ))

(define (draw-6 dc x y s) ; top bar
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line (- x ds) (- y hs) (+ x ds) (- y hs)) ; 10 to 2
    ))

(define (draw-12 dc x y s) ; parallels
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line (- x ds) (- y hs) (+ x ds) (- y hs)) ; 10 to 2
    (send dc draw-line (- x ds) (+ y hs) (+ x ds) (+ y hs)) ; 8 to 4
    ))

(define (draw-18 dc x y s) ; triangle
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line (- x ds) (- y hs) (+ x ds) (- y hs)) ; 10 to 2
    (send dc draw-line (- x ds) (- y hs) x (+ y s)) ; 10 to 6
    (send dc draw-line x (+ y s) (+ x ds) (- y hs)) ; 6 to 2
    ))

(define (draw-24 dc x y s) ; crossing lines
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line x (- y s) (- x ds) (+ y hs)) ; 12 to 8
    (send dc draw-line x (- y s) (+ x ds) (+ y hs)) ; 12 to 4
    (send dc draw-line (- x ds) (- y hs) x (+ y s)) ; 10 to 6
    (send dc draw-line x (+ y s) (+ x ds) (- y hs)) ; 6 to 2
    ))

(define (draw-30 dc x y s) ; crossing lines with top bar
  (let* ([hs (/ s 2)]
         [ds (* (sqrt 3) hs)])
    (send dc draw-line (- x ds) (- y hs) (+ x ds) (- y hs)) ; 10 to 2
    (send dc draw-line x (- y s) (- x ds) (+ y hs)) ; 12 to 8
    (send dc draw-line x (- y s) (+ x ds) (+ y hs)) ; 12 to 4
    (send dc draw-line (- x ds) (- y hs) x (+ y s)) ; 10 to 6
    (send dc draw-line x (+ y s) (+ x ds) (- y hs)) ; 6 to 2
    ))

; and put it all together...
(define (draw-digit d dc x y s)
  (draw-zero dc x y s)
  (let* ([ones (modulo d 6)]
         [sixes (- d ones)])
    (case ones
      [(1) (draw-one dc x y s)]
      [(2) (draw-two dc x y s)]
      [(3) (draw-three dc x y s)]
      [(4) (draw-four dc x y s)]
      [(5) (draw-five dc x y s)])
    (case sixes
      [(6) (draw-6 dc x y s)]
      [(12) (draw-12 dc x y s)]
      [(18) (draw-18 dc x y s)]
      [(24) (draw-24 dc x y s)]
      [(30) (draw-30 dc x y s)])
    ))

(define (draw-number n dc x y s)
  (send dc set-pen "black" 1 'solid)
  (if (< n 36)
      (draw-digit n dc x y s)
      (let* ([hs (/ s 2)]
             [ths (* 3/2 hs)]
             [dhs (* (sqrt 3) (/ hs 2))]
             [d1 (modulo n 36)]
             [d1a (- n d1)]
             [d2 (modulo (/ d1a 36) 36)]
             [d2a (- d1a (* 36 d2))]
             [d3 (modulo (/ d2a 36 36) 36)]
             [d3a (- d2a (* 36 36 d3))]
             [d4 (modulo (/ d3a 36 36 36) 36)]
             [d4a (- d3a (* 36 36 36 d4))]
             [d5 (modulo (/ d4a 36 36 36 36) 36)]
             [d5a (- d4a (* 36 36 36 36 d5))]
             [d6 (modulo (/ d5a 36 36 36 36 36) 36)]
             [d6a (- d5a (* 36 36 36 36 36 d6))]
             [d7 (modulo (/ d6a 36 36 36 36 36 36) 36)]
             [nd (if (< 0 d7) 7 (if (< 0 d6) 6 (if (< 0 d5) 5 (if (< 0 d4) 4 (if (< 0 d3) 3 2)))))])
        (draw-digit d1 dc x y hs) ; central
        (draw-number-base d2 dc (- x (* 2 dhs)) y hs)                   ;  9:00
        (when (> nd 2) (draw-number-base d3 dc (- x dhs) (- y ths) hs)) ; 11:00
        (when (> nd 3) (draw-number-base d4 dc (+ x dhs) (- y ths) hs)) ;  1:00
        (when (> nd 4) (draw-number-base d5 dc (+ x (* 2 dhs)) y hs))   ;  3:00
        (when (> nd 5) (draw-number-base d6 dc (+ x dhs) (+ y ths) hs)) ;  5:00
        (when (> nd 6) (draw-number-base d7 dc (- x dhs) (+ y ths) hs)) ;  7:00
        )))

(define (draw-number-base n dc x y s)
  (send dc set-brush (get-fill-col n) 'solid)
  (send dc draw-path (zero-path x y s))
  (send dc set-brush "black" 'transparent)
  (draw-number n dc x y s))

; canvasing things
(define all-digits
  (build-vector 6 (λ (n) (build-vector 6 (λ (m) (+ (* n 6) m))))))
(define the-grid all-digits)
(define (grid-arrange l)
  (let* ([len (length l)]
         [sq (floor (sqrt len))]
         [rt (sqrt len)]
         [qt (+ (inexact->exact sq) (if (equal? sq rt) 0 1))]
         [qr (ceiling (/ len qt))])
    (build-vector qr (λ (n) (build-vector qt (λ (m)
                                               (let ([pos (+ (* n qt) m)])
                                                 (if (< pos (length l))
                                                     (list-ref l pos)
                                                     0))))))))

(define (draw-grid dc)
  (send dc set-smoothing 'smoothed)
  ; draw the grid
  ; first, get dimensions and place the center
  (let*-values ([(w h) (send dc get-size)]
                [(cx) (/ w 2)]
                [(cy) (/ h 2)]
                [(magic) (* (sqrt 3) 3/4)]
                [(row-length) (vector-length (vector-ref the-grid 0))]
                [(column-length) (vector-length the-grid)]
                [(row-length-real) (* (+ row-length 1) magic)]
                [(column-length-real) (* (+ (/ column-length 2) 1/2) (sqrt 3) magic)]
                [(row-scale) (/ w row-length-real)]
                [(column-scale) (/ h column-length-real)]
                [(true-scale) (min row-scale column-scale)]
                [(start-x) (- cx (* (- row-length 3/2) magic true-scale 1/2))]
                [(start-y) (- cy (* (- column-length 1) (* magic (sqrt 3)) true-scale 1/4))])
    (send dc draw-rectangle 0 0 w h)
    (for([i (in-range column-length)])
      (for ([j (in-range row-length)])
        (send dc set-pen "black" 2 'solid)
        (draw-number-base
         (vector-ref (vector-ref the-grid i) j) dc
         (if (odd? i)
             (+ start-x (* magic true-scale j))
             (+ start-x (* magic true-scale (- j 1/2))))
         (+ start-y (* true-scale i (* magic (sqrt 3)) 1/2)) (* true-scale 1/2))
        ))
    ))

; export
(define (export-grid)
  (let*-values ([(w h) (send (send main-canvas get-dc) get-size)]
                [(bmp) (make-bitmap w h)]
                [(bdc) (new bitmap-dc% [bitmap bmp])])
    (draw-grid bdc)
    (send bmp save-file
          (string-append
           (string-join (map number->string (apply append (vector->list (vector-map vector->list the-grid)))))
           ".png") 'png)))

; windowing things
(define frame (new frame% [label "Tiles"] [height 700] [width 800]))
(define canvas-p (new vertical-panel% [parent frame] [alignment '(center center)]))
(define menu-p (new horizontal-panel% [parent canvas-p] [alignment '(center center)]
                    [stretchable-height #f]))

(define digits-b 
  (new button% [parent menu-p] [label "Display All Digits"]
       [callback (λ (b e) (set! the-grid all-digits) (send main-canvas refresh))]))

(define input-t
  (new text-field% [parent menu-p] [label ""]))

(define input-b
  (new button% [parent menu-p] [label "Draw Numbers"]
       [callback
        (λ (b e)
          (set! the-grid
                (let ([spl (string-split (send input-t get-value))])
                  (if (string->number (first spl))
                      (grid-arrange (map string->number spl))
                      all-digits)))
          (send main-canvas refresh))]))

(define pal-drop
  (new choice% [label "Palette: "]
       [parent menu-p]
       [callback
        (λ (c e)
          (set! palette (hash-ref palette-lookup (list-ref palette-choices (send c get-selection))))
          (send main-canvas refresh))]
       [choices palette-choices]))

(define export-b 
  (new button% [parent menu-p] [label "Export"] [callback (λ (b e) (export-grid))]))

(define main-canvas
  (new canvas% [parent canvas-p]
       [paint-callback
        (λ (canvas dc)
          (draw-grid dc))]))

(send frame show #t)