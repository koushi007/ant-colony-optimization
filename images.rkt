#lang racket
(provide (all-defined-out))
(require 2htdp/image)
(define france (bitmap/file "france.jpg"))
(define us2 (bitmap/file "us2.jpg"))
(define germany (bitmap/file "map_of_germany.jpg"))
(define spain (bitmap/file "spain.png"))
(define (scale-appropriate! image)
  (define new-scale
    (let*([delx (image-width image)]
          [dely (image-height image)]
          [scale-by-x (/ 880 delx)]
          [scale-by-y (/ 620 dely)])
      (cond
        [(> 620 (* scale-by-x dely)) scale-by-y]
        [(> 880 (* scale-by-y delx)) scale-by-x]
        [else (error "both scales inappropriate")])))
  (scale/xy new-scale new-scale image))
(define button1 (bitmap/file "anmol_1.png"))
(define button2 (bitmap/file "anmol_2.png"))
(define button3 (bitmap/file "anmol_3.png"))
(define button4 (bitmap/file "anmol_4.png"))
(define tspsolver (bitmap/file "anmol_heading.png"))
(define solvewithaco (bitmap/file "anmol_aco.png"))
(define solvewithbruteforce (bitmap/file "anmol_brute.png"))
(define solveonmap (bitmap/file "anmol_map.png"))
(define solved% (bitmap/file "anmol_solved.png"))
(define trace (bitmap/file "anmol_trace.png"))
(define backtomm (bitmap/file "anmol_main.png"))
(define bestsofar (bitmap/file "anmol_best_dist_so.png"))
(define next (bitmap/file "anmol_next.png"))
(set! us2 (scale-appropriate! us2))
(set! france (scale-appropriate! france))
(set! germany (scale-appropriate! germany))
(set! spain (scale-appropriate! spain))
(define (in-circle x y x0 y0 r)
  (<= (+ (sqr (- x x0)) (sqr (- y y0))) (sqr r)))


