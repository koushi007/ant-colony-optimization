#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require "images.rkt")

;;;;;;;;;;;;;;;;;;;;;;Classes;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generate-random-point)
  (vector-ref points-vector (random (vector-length points-vector))))

(define (generate-random-points-vector)
  (list->vector (shuffle (vector->list points-vector))))

(define ant-factor 0.75)

(define posn-vec (make-vector 0))

(define (point%-posn point)
  (make-posn (get-field x point) (get-field y point)))
 
(define (posn-point% pos)
  (new point% [x (posn-x pos)] [y (posn-y pos)]))


(define (all-edges-vector points-vec)
  (list->vector
   (for*/list ([i (in-range (vector-length points-vec))]
               [j (in-range (+ i 1) (vector-length points-vec))])
     (make-object graph-edge% (cons (vector-ref points-vec i)
                                    (vector-ref points-vec j))))))
(define edge-vector (make-vector 0))
(define points-vector (build-vector (vector-length posn-vec) (lambda (x) (posn-point% x))))




(define loop%
  (class object%
    
    (init-field loop-size)
    (field [vector-of-edges (make-vector loop-size 0)])
    (field [total-length 9999999999])   
    
    (super-new)

    (define/public (update best-edges-vector)
      (begin
        (set! vector-of-edges best-edges-vector)
        (set-field! total-length
                    this
                    (apply + (for/list ([an-edge best-edges-vector])
                               (get-field length an-edge))))))))


(define point%
  (class object%
    
    (init-field x)
    (init-field y)
    
    (super-new)
    
    (define/public (distance-to second-point) (sqrt (+ (sqr (- x (get-field x  second-point))) (sqr (- y (get-field y  second-point))))))))


(define graph-edge%
  (class object%
    
    (init-field point-pair)
    (define basic-pheremone 100) (define rho 0.5)
    (define (edge-length edge) (send (car (get-field  point-pair edge)) distance-to (cdr (get-field  point-pair edge))))
    (field [pheromone-intensity basic-pheremone])
    (field [length (edge-length this)])
    
    (super-new)
    
    (define/public (edge-of? pair-of-points-to-check)
      (or (equal? point-pair pair-of-points-to-check)
          (equal? (cons (cdr point-pair) (car point-pair)) pair-of-points-to-check)))
    (define/public (increment-pheromone amount) (set! pheromone-intensity (+ pheromone-intensity amount)))))





(define ant%
  (class object%
    
    (init-field size-of-edges-vector)                                   
    (init-field init-point)
    (field [color "yellow"])
    (field [curr-point init-point])
    (field [edges-covered-vector (make-vector size-of-edges-vector 0)])
    (field [iters 0])
    (define Q 500000) (define alpha 1) (define beta 5) (define iter-limit 200)
    
    (super-new)
    
    (define probability-of-moving-to (lambda (next-edge)
                                       (begin
                                         (define (single-contribution e)
                                           (/ (expt (get-field pheromone-intensity e) alpha) (expt (get-field length e) beta)))
                                         (/ (single-contribution next-edge)
                                            (apply + (for/list ([an-edge (possible-edges-vector)])
                                                       (single-contribution an-edge)))))))
    (define/public (update-pheromone)
      (begin
        (define length-sum 0)
        (for ([edge edges-covered-vector])
          (set! length-sum (+ length-sum (get-field length edge))))
        (for ([edge edges-covered-vector])
          (send edge increment-pheromone (* (/ Q length-sum) (get-field length edge))))))
    (define (update-covered-edges new-edge)
      (vector-set! edges-covered-vector (index-of (vector->list edges-covered-vector) 0) new-edge))
   (define (generate-random-point)
      (vector-ref points-vector (random (vector-length points-vector))))
    (define/public (make-random-decision)
      (define possible-edges (possible-edges-vector));so that it can be stored and we don't need to call it again
      (define weights-list
        (vector->list (vector-map (lambda(x) (probability-of-moving-to x)) possible-edges)))
      (define points-list
        (vector->list (vector-map (lambda(x) (let([pair-of-points (get-field point-pair x)])
                                               (if(equal? curr-point (car pair-of-points))
                                                  (cdr pair-of-points)
                                                  (car pair-of-points))))
                                  possible-edges)))
      (define index (weighted-random weights-list))
      (begin  (set! curr-point (list-ref points-list index))
              (update-covered-edges (vector-ref possible-edges index))
              curr-point))
    (define/public (possible-edges-vector)
      (if (equal? (- (vector-length points-vector) 1) (index-of (vector->list edges-covered-vector) 0))
          (let ([find-initial-point
                 (let ([pair1 (get-field point-pair (vector-ref edges-covered-vector 0))]
                       [pair2 (get-field point-pair (vector-ref edges-covered-vector 1))])
           (cond ((or (equal? (car pair1) (cdr pair2)) (equal? (car pair1) (car pair2))) (cdr pair1))
                 (else (car pair1))))])
            (list->vector (list (make-object graph-edge% (cons find-initial-point curr-point)))))
          (let ([covered-points-list
             (remove curr-point (remove-duplicates (append*
                                                    (map
                                                     (lambda (x) (list (car (get-field point-pair x)) (cdr (get-field point-pair x))))
                                                     (filter (lambda (x) (not (equal? x 0))) (vector->list  edges-covered-vector))))))])
            (vector-filter
             (lambda(x) (and (not (let ([point-pair-of-the-edge-x (get-field point-pair x)])
                           (or (member (car point-pair-of-the-edge-x) covered-points-list)
                               (member (cdr point-pair-of-the-edge-x) covered-points-list))))  ;changed
                             
                             (let([point-pair-of-the-edge-x (get-field point-pair x)])
                               (or (equal? curr-point (car point-pair-of-the-edge-x))
                                   (equal? curr-point (cdr point-pair-of-the-edge-x))))))
       edge-vector))))
    (define/public (reset) (begin (let ([total-distance-of-loop (total-distance edges-covered-vector 0 0)])
                                     (if (< total-distance-of-loop (get-field total-length best-so-far)) (send best-so-far update edges-covered-vector)
                                        (void)))
                                  (set! edges-covered-vector (make-vector size-of-edges-vector 0))
                                  (set! curr-point (generate-random-point))))
    (define/public (do-a-loop)
      (if (= iter-limit iters)
          (begin
            ;close the ant object display
            (void))             
          (begin (for ([i (in-range size-of-edges-vector)])
                   (make-random-decision))
                 (set! iters (+ iters 1))
                 (update-pheromone)
                 (reset)
                 (do-a-loop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Definitions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define image '())
(define solved%pos (make-posn 200 940))
(define tracepos (make-posn 200 940))
(define nextpos (make-posn 500 940))
(define backtommpos (make-posn 800 940))
(define bestsofarpos (make-posn 210 50))
(define side-rect-pos (make-posn 960 500))
(define imagepos (make-posn 500 500))
(define side-color (make-color 50 50 50 50))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;graphics code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (weighted-random weights-list);input for this function should be the list of weights,it returns an index
  (define sum (apply + weights-list))
  (define rnd (* sum (random)))
  (define count 0)
  (for([weight (stop-before weights-list (lambda(x) (< rnd x)))])
    (set! count (+ count 1))
    (set! rnd (- rnd weight)))
  count)

;answer is zero initially (""tail-recursion"")
;num should be given value "0"
(define (total-distance vector-of-edges num answer)
  (if (= num (vector-length vector-of-edges)) answer
      (total-distance vector-of-edges (+ num 1)
                      (+ answer (get-field length (vector-ref vector-of-edges num))))))

(define (total-distance-from-points vectorofpoints)
  (define full (vector-append vectorofpoints (vector (vector-ref vectorofpoints 0))))
  (apply + (for/list ([i (in-range (vector-length vectorofpoints))])
             (send (vector-ref full i)  distance-to  (vector-ref full (+ 1 i))))))

(define best-vector-till-now 0)

(define best-so-far
  (make-object loop% (vector-length points-vector)))

(define (best-so-far-call) best-so-far)

(define (brute-force-method-2 index minimum-distance);initial 0, 99999999
  (define (remove-reverse-lists l answer)
    (cond ((null? l) '())
          ((null? (cdr l)) l)
          (else (if (member (reverse (car l)) (cdr l))
                    (remove-reverse-lists (cdr l) answer)
                    (remove-reverse-lists (cdr l) (cons (car l) answer))))))
 (let
 ([all-possible-loops-vector (list->vector (map (lambda(x) (list->vector x))
            (remove-reverse-lists (permutations (vector->list points-vector)) '())))]) 
    (begin
      (define (iter)
        (if (= index (vector-length all-possible-loops-vector)) minimum-distance
            (let* ([test-loop (vector-ref all-possible-loops-vector index)]
                   [distance (total-distance-from-points test-loop)]
                   [full (vector-append test-loop (vector (vector-ref test-loop 0)))])
              (if (< distance  minimum-distance)
                  
                  (begin (set-field! vector-of-edges best-so-far
             (for/vector ([i (in-range (vector-length test-loop))])
              (make-object graph-edge% (cons (vector-ref full i) (vector-ref full (+ 1 i))))))
                         
                         (set! minimum-distance distance)
                         (set! index (+ index 1))
                         (iter))
                  (begin
                    (set! index (+ index 1))
                (iter))))))
      (iter))))
(define (brute-force-method-1 number minimum-distance);initial number = total tries
  (if (= number 0) minimum-distance
      (let* ([random-vector (generate-random-points-vector)]
             [distance (total-distance-from-points random-vector)]
             [full (vector-append random-vector (vector (vector-ref random-vector 0)))])
        (if (< distance  minimum-distance)
            (begin (set-field! vector-of-edges best-so-far      
                    (for/vector ([i (in-range (vector-length random-vector))])
              (make-object graph-edge% (cons (vector-ref full i) (vector-ref full (+ 1 i))))))
                   (set! minimum-distance distance)
                   (brute-force-method-1 (- number 1) distance))
            (brute-force-method-1 (- number 1) minimum-distance)))))

(define number-of-ants (exact-round (* ant-factor (vector-length points-vector))))
(define vector-of-ants (build-vector number-of-ants (lambda(x) (make-object ant% (vector-length points-vector) (generate-random-point)))))

(define (do-aco)
  (begin
    (for ([i (in-range 0 number-of-ants)])
      (send (vector-ref vector-of-ants i) do-a-loop))))

;(brute-force-method-1 0 9999999)
(define minimum-distance 9999999)
(define (color1 a) (make-color 100 100 100 a))
(define (pen1 a) (make-pen (color1 a) 7 "solid" "round" "bevel"))
(define (closer? a b) (cond [(or (< (abs (- (posn-x a) (posn-x b))) 0.1)
                                 (< (abs (- (posn-y a) (posn-y b))) 0.1)) #t]
                            [else #f]))

(define state (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))

(big-bang state
  (on-mouse (lambda (w x y b) (cond
                                [(and (= 0 (vector-ref w 2)) (equal? b "button-down");aco
                                      (< x 700) (> x 300) (< y 450) (> y 350)) (begin (vector-set! w  2 1) w)]
                                [(and (= 0 (vector-ref w 2)) (equal? b "button-down");aco
                                      (< x 700) (> x 300) (< y 650) (> y 550)) (begin (vector-set! w  2 4) w)]
                                [(and (= 0 (vector-ref w 2)) (equal? b "button-down");init of map aco
                                      (< x 700) (> x 300) (< y 850) (> y 750)) (begin (vector-set! w 2 2) w)]
                                
                                [(and (= 1 (vector-ref w 2)) (equal? b "button-down");aco-main-menu
                                      (< x 900) (> x 700) (< y 980) (> y 900)) (begin (vector-set! w 2 0)
                                                                                      (set! posn-vec (make-vector 0))
                                                                                      (set! points-vector (make-vector 0))
                                                                                      (set! edge-vector (make-vector 0))
                                                                                      (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                                      w)]
                                [(and (= 3 (vector-ref w 2)) (equal? b "button-down");map-aco-main-menu
                                      (< x 900) (> x 700) (< y 980) (> y 900)) (begin (vector-set! w 2 0)
                                                                                      (set! posn-vec (make-vector 0))
                                                                                      (set! points-vector (make-vector 0))
                                                                                      (set! edge-vector (make-vector 0))
                                                                                      (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                                      w)]
                                [(and (= 4 (vector-ref w 2)) (equal? b "button-down");brute-force-main-menu
                                      (< x 900) (> x 700) (< y 980) (> y 900)) (begin (vector-set! w 2 0)
                                                                                      (set! posn-vec (make-vector 0))
                                                                                      (set! points-vector (make-vector 0))
                                                                                      (set! edge-vector (make-vector 0))
                                                                                      (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                                      w)]
                                [(and (= 5 (vector-ref w 2)) (equal? b "button-down");map-brute-force-main-menu
                                      (< x 900) (> x 700) (< y 980) (> y 900)) (begin (vector-set! w 2 0)
                                                                                      (set! posn-vec (make-vector 0))
                                                                                      (set! points-vector (make-vector 0))
                                                                                      (set! edge-vector (make-vector 0))
                                                                                      (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                                      w)]
                                
                                [(and (= 1 (vector-ref w 2)) (equal? b "button-down");aco-button
                                      (< x 600) (> x 400) (< y 980) (> y 900)) (begin (vector-set! w 3 1) w)]
                                [(and (= 3 (vector-ref w 2)) (equal? b "button-down");map-aco-button
                                      (< x 600) (> x 400) (< y 980) (> y 900)) (begin (vector-set! w 3 1) w)]
                                [(and (= 4 (vector-ref w 2)) (equal? b "button-down");brute-force-button
                                      (< x 600) (> x 400) (< y 980) (> y 900)) (begin (vector-set! w 3 1) w)]
                                [(and (= 5 (vector-ref w 2)) (equal? b "button-down");map-brute-force-button
                                      (< x 600) (> x 400) (< y 980) (> y 900)) (begin (vector-set! w 3 1) w)]

                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-button1
                                      (in-circle x y 200 50 25)) (begin (set! image us2) w)]
                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-button2
                                      (in-circle x y 400 50 25)) (begin (set! image germany) w)]
                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-button3
                                      (in-circle x y 600 50 25)) (begin (set! image france) w)]
                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-button4
                                      (in-circle x y 800 50 25)) (begin (set! image spain) w)]

                                [(and (= 1 (vector-ref w 2)) (equal? b "move");aco-side-button
                                      (> x 980) (< x 1000)) (begin (vector-set! w 3 6) w)]
                                [(and (= 3 (vector-ref w 2)) (equal? b "move");map-aco-side-button
                                      (> x 980) (< x 1000)) (begin (vector-set! w 3 6) w)]
                                [(and (= 4 (vector-ref w 2)) (equal? b "move");brute-force-side-button
                                      (> x 980) (< x 1000)) (begin (vector-set! w 3 6) w)]
                                [(and (= 5 (vector-ref w 2)) (equal? b "move");map-brute-force-side-button
                                      (> x 980) (< x 1000)) (begin (vector-set! w 3 6) w)]

                                [( and (or  (= 1 (vector-ref w 2))
                                              (= 4 (vector-ref w 2))
                                              (= 3 (vector-ref w 2))
                                            (= 5 (vector-ref w 2))) (= 6 (vector-ref w 3)) (equal? b "button-down");aco-side-window-select
                                     (< x 600) (> x 400) (< y 980) (> y 900))
                                                                 (begin 
                                                                   (vector-set! w 3 1)  w)]

                                [( and (= 1 (vector-ref w 2)) (= 6 (vector-ref w 3)) (equal? b "button-down");aco-side-window-select
                                      (> x 920) (< x 1000)) (begin 
                                                                   (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                   (vector-set! w 2 4)  w)]
                                [(and (= 3 (vector-ref w 2)) (= 6 (vector-ref w 3)) (equal? b "button-down");map-aco-side-window-select
                                      (> x 920) (< x 1000)) (begin 
                                                                   (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                   (vector-set! w 2 5) w)]
                                [(and (= 4 (vector-ref w 2)) (= 6 (vector-ref w 3)) (equal? b "button-down");brute-force-side-window-select
                                      (> x 920) (< x 1000)) (begin 
                                                                   (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                   (vector-set! w 2 1) w)]
                                [(and (= 5 (vector-ref w 2)) (= 6 (vector-ref w 3)) (equal? b "button-down");map-brute-force-side-window-select
                                      (> x 920) (< x 1000)) (begin 
                                                                   (set! w (vector (make-vector 0) (make-vector 0) 0 0 '() '() '() 0 '() 100 10000 999999 0))
                                                                   (vector-set! w 2 3)  w)]
                                
                                [(and (= 1 (vector-ref w 2)) (equal? b "button-down");aco-ant-trace
                                      (< x 300) (> x 100) (< y 980) (> y 900)) (begin (if (= (vector-ref w 12) 0) (vector-set! w 12 1)	
                                                                                                                  (vector-set! w 12 0)) w)]
                                [(and (= 3 (vector-ref w 2)) (equal? b "button-down");map-aco-ant-trace
                                      (< x 300) (> x 100) (< y 980) (> y 900)) (begin (if (= (vector-ref w 12) 0) (vector-set! w 12 1)	
                                                                                                                  (vector-set! w 12 0)) w)]
                                
                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-aco
                                      (< x 800) (> x 200) (< y 300) (> y 100)) (begin
                                                                                 (vector-set! w 2 3) w)]
                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-bf
                                      (< x 800) (> x 200) (< y 600) (> y 400)) (begin
                                                                                 (vector-set! w 2 5) w)]
                                [(and (= 2 (vector-ref w 2)) (equal? b "button-down");map-options-page-back
                                      (< x 800) (> x 200) (< y 900) (> y 700)) (begin
                                                                                 (vector-set! w 2 0) w)]
                                
                                [(and  (equal? b "button-down") (and (= 0 (vector-ref w 3)) (or;point-selection 
                                                                                            (= 1 (vector-ref w 2))
                                                                                            (= 4 (vector-ref w 2))
                                                                                            (= 3 (vector-ref w 2))
                                                                                            (= 5 (vector-ref w 2)))))
                                                        (begin (set! posn-vec
                                                                    (vector-append posn-vec (make-vector 1 (make-posn x y))))
                                                               (set! points-vector
                                                                    (vector-append points-vector (make-vector 1
                                                                                                 (posn-point% (make-posn x y)))))
                                                               (set! edge-vector (all-edges-vector points-vector))
                                                               w)]
                                
                                [else w])))
  
 (on-tick (lambda (w) (cond [(or (= 3 (vector-ref w 2)) (= 1 (vector-ref w 2)))
                              (begin
                          (vector-map (lambda (x) (cond [(> (get-field pheromone-intensity x) 25)
                                                         (set-field! pheromone-intensity x (- (get-field pheromone-intensity x) 1))])) edge-vector)
                         (cond[(= (vector-ref w 3) 3) (begin (cond [(or (<= 0 (- (posn-x (car (vector-ref w 4))) (posn-x (car (vector-ref w 6)))))
                                                         (<= 0 (- (posn-y (car (vector-ref w 4))) (posn-y (car (vector-ref w 6))))))
                                                            (begin (vector-set! w 3 2) (vector-set! w 4 (vector-ref w 6)))]
                                                       [else (vector-set! w 4 (build-list (length (vector-ref w 4))
                                                                                 (lambda (x) (make-posn (+ (posn-x (list-ref (vector-ref w 4) x))
                                                                                                         (/ (car (list-ref (vector-ref w 5) x)) 10))
                                                                                                        (+ (posn-y (list-ref (vector-ref w 4) x))
                                                                                                         (/ (cdr (list-ref (vector-ref w 5) x)) 10))))))]
                                                       ) w) ]
                                [(and (>= (vector-ref w 9) 0)  (= (vector-ref w 3) 2))
                                                (begin
                                                 
                                                 (cond [(not (member 0 (vector->list (get-field edges-covered-vector (vector-ref vector-of-ants 0)))))
                                                        (begin
                                                          (vector-set! w 9 (- (vector-ref w 9) 1))
                                                         
                                                          (vector-set! w 7 (argmin (lambda (y) (total-distance y 0 0))
                                                                                   (map  (lambda (x) (get-field edges-covered-vector x))
                                                                                   (vector->list vector-of-ants))))
                                                           
                                                          (cond
                                                           [(< (total-distance (vector-ref w 7) 0 0) (vector-ref w 11))
                                                            (begin
                                                              (vector-set! w 11 (total-distance (vector-ref w 7) 0 0))
                                                              (vector-set! w 8 (vector-ref w 7)))])
                                                          
                                                           (append*
                                                            (map (lambda (x)
                                                                (vector->list (get-field edges-covered-vector x)))
                                                               (vector->list vector-of-ants)))






                                                          
                                                          ;(display (get-field edges-covered-vector (vector-ref vector-of-ants 0)))
                                                         
                                                                (set! vector-of-ants
                                                          (build-vector number-of-ants (lambda (x) (make-object ant% (vector-length points-vector) (generate-random-point))))) 
                                                               )])
                                                 
                                                               
                                                 (vector-set! w 3 3)
                                                 (vector-set! w 4  (map (lambda (p) (point%-posn (get-field curr-point p)))
                                                  (vector->list vector-of-ants)))
                                                 
                                                  (map (lambda (p) (send p make-random-decision)) (vector->list vector-of-ants))
                                                  (vector-set! w 6  (map (lambda (p) (point%-posn (get-field curr-point p)))
                                                  (vector->list vector-of-ants)))
                                                  (vector-set! w 5 (map (lambda (a b) (cons (- (posn-x b) (posn-x a))
                                                                                            (- (posn-y b) (posn-y a))))
                                                                            (vector-ref w 4) (vector-ref w 6)))
                                                  w)]
                                [else w]))]

                                
                         
                             [(or (= 4 (vector-ref w 2)) (= 5 (vector-ref w 2)))
                              (cond [(and (> (vector-ref w 10) 0) (= (vector-ref w 3) 1))
                                                             (begin
                                                                    (vector-set! w 10 (- (vector-ref w 10) 1))
                                                                    (vector-set! w 4 (generate-random-points-vector))
                                                                    (vector-set! w 5 (total-distance-from-points (vector-ref w 4)))
                                                                    (vector-set! w 6 (vector-append (vector-ref w 4) (vector (vector-ref (vector-ref w 4) 0))))
                                                                    (vector-set! w 7
                                                                          (for/vector ([i (in-range (vector-length points-vector))])
                                                                      (make-object graph-edge% (cons (vector-ref (vector-ref w 6) i)
                                                                                                     (vector-ref (vector-ref w 6) (+ i 1))))))
                                                                     (cond [(< (vector-ref w 5) (vector-ref w 11))
                                                                        (begin
                                                                          (vector-set! w 11 (vector-ref w 5))
                                                                          (vector-set! w 8 (vector-ref w 7)))])
                                                                     w)]
                                                         
                                                            [else w])]
                             [else w])) (/ 1 120))
  

  (to-draw (lambda (w)
             (place-images
              (append
               
               (if (and (or (= 1 (vector-ref w 2))
                                      (= 3 (vector-ref w 2))) (>= (vector-ref w 3) 1)) (begin
                                             (set! number-of-ants (exact-round (* ant-factor (vector-length points-vector))))
                                             (if (= (vector-ref w 12) 1)
                                                 (append (list (circle 10 255 "yellow"))
                                                         (make-list (- number-of-ants 1) (circle 5 255 "red")))
                                                         
                                             (make-list number-of-ants (circle 5 255 "red")))) '())
                              
               (make-list (vector-length posn-vec) (circle 13 200 'blue))
               
               (map (lambda (x) (let ([p1 (car (get-field point-pair x))]
                                      [p2 (cdr (get-field point-pair x))])
                                  (line (- (get-field x p2) (get-field x p1))
                                        (- (get-field y p2) (get-field y p1))
                                        (pen1 (get-field pheromone-intensity x)))))
                    (vector->list edge-vector))
               
               (cond [(and (or (= (vector-ref w 2) 5) (= (vector-ref w 2) 4)) (= 1 (vector-ref w 3)) (and (vector? (vector-ref w 8))
                                                                                                          (vector? (vector-ref w 7))))
                      (append 
               (map (lambda (x) (let ([p1 (car (get-field point-pair x))]
                                      [p2 (cdr (get-field point-pair x))])
                                  (line (- (get-field x p2) (get-field x p1))
                                        (- (get-field y p2) (get-field y p1))
                                        (make-pen "yellow" 7 "solid" "round" "bevel"))))
                    (vector->list (vector-ref w 7)))
               (map (lambda (x) (let ([p1 (car (get-field point-pair x))]
                                      [p2 (cdr (get-field point-pair x))])
                                  (line (- (get-field x p2) (get-field x p1))
                                        (- (get-field y p2) (get-field y p1))
                                        (make-pen "red" 15 "solid" "round" "bevel"))))
                    (vector->list (vector-ref w 8))))]
                     [else '()])
               (cond [(and (or (= (vector-ref w 2) 1) (= (vector-ref w 2) 3)) (< 1 (vector-ref w 3)) (vector? (vector-ref w 8)))
                   
               (map (lambda (x) (let ([p1 (car (get-field point-pair x))]
                                      [p2 (cdr (get-field point-pair x))])
                                  (line (- (get-field x p2) (get-field x p1))
                                        (- (get-field y p2) (get-field y p1))
                                        (make-pen "red" 7 "solid" "round" "bevel"))))
                    (vector->list (vector-ref w 8)))]
                     [else '()])

               (cond [(= 0 (vector-ref w 2)) (list tspsolver solvewithaco solvewithbruteforce solveonmap)]
                     [(and
                       (= 1 (vector-ref w 2)) (= 6 (vector-ref w 3)))
                      (list bestsofar trace next backtomm
                            (rectangle 80 1000 "solid" side-color)
                            (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                            (text/font (number->string (vector-ref w 9)) 24 "indigo" #f "roman" 'normal 'bold #f))]   
                     [(and
                       (= 3 (vector-ref w 2)) (= 6 (vector-ref w 3)))
                      (list bestsofar trace next backtomm
                            (rectangle 80 1000 "solid" side-color)
                            image
                            (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                            (text/font (number->string (vector-ref w 9)) 24 "indigo" #f "roman" 'normal 'bold #f))]
                     [(and
                       (= 5 (vector-ref w 2)) (= 6 (vector-ref w 3)))
                      (list bestsofar trace next backtomm
                            (rectangle 80 1000 "solid" side-color)
                            image
                            solved%
                            (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                            (text/font (number->string (vector-ref w 10)) 24 "indigo" #f "roman" 'normal 'bold #f))]
                     [(and
                       (= 4 (vector-ref w 2)) (= 6 (vector-ref w 3)))
                      (list bestsofar next backtomm
                            (rectangle 80 1000 "solid" side-color)
                            solved%
                            (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                            (text/font (number->string (vector-ref w 10)) 24 "indigo" #f "roman" 'normal 'bold #f))]
                     [(= 1 (vector-ref w 2))
                      (list bestsofar trace next backtomm
                            (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                            (text/font (number->string (vector-ref w 9)) 24 "indigo" #f "roman" 'normal 'bold #f))]
                     [(= 2 (vector-ref w 2))
                      (list button1 button2 button3 button4 solvewithaco solvewithbruteforce backtomm)]
                     [(= 3 (vector-ref w 2)) (list bestsofar trace next backtomm image
                                                   (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                                                   (text/font (number->string (vector-ref w 9)) 24 "indigo" #f "roman" 'normal 'bold #f))]
                     [(= 4 (vector-ref w 2)) (list bestsofar solved% next backtomm
                                                   (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                                                   (text/font (number->string (vector-ref w 10)) 24 "indigo" #f "roman" 'normal 'bold #f))]
                     [(= 5 (vector-ref w 2)) (list bestsofar solved% next backtomm image
                                                   (text/font (number->string (vector-ref w 11)) 24 "indigo" #f "roman" 'normal 'bold #f)
                                                   (text/font (number->string (vector-ref w 10)) 24 "indigo" #f "roman" 'normal 'bold #f))]))

              (append
               
               (cond [(and (or (= 1 (vector-ref w 2))
                                      (= 3 (vector-ref w 2))) (= 1 (vector-ref w 3))) (begin
                                               (set! number-of-ants (exact-round (* ant-factor (vector-length points-vector))))
                                               (set! vector-of-ants
                                                          (build-vector number-of-ants (lambda (x) (make-object ant% (vector-length points-vector) (generate-random-point))))) 
                                                   (vector-set! w 3 2)
                                                   (map (lambda (p) (point%-posn (get-field curr-point p)))
                                                  (vector->list vector-of-ants)))]
                     [(and (or (= 1 (vector-ref w 2))
                                      (= 3 (vector-ref w 2))) (>=  (vector-ref w 3) 2)) (vector-ref w 4)]
                                                   
                     [else '()])

               (vector->list posn-vec)
               (map (lambda (x) (let ([p1 (car (get-field point-pair x))]
                                      [p2 (cdr (get-field point-pair x))])
                                  (make-posn (/ (+ (get-field x p2) (get-field x p1)) 2)
                                             (/ (+ (get-field y p2) (get-field y p1)) 2))))
                     (cond [(and (or (= (vector-ref w 2) 5) (= (vector-ref w 2) 4))  (and (vector? (vector-ref w 8)) (vector? (vector-ref w 7))) (= 1 (vector-ref w 3)))
                     (append (vector->list edge-vector) (vector->list (vector-ref w 7)) (vector->list (vector-ref w 8)))]
                           [(and (or (= (vector-ref w 2) 1) (= (vector-ref w 2) 3))  (vector? (vector-ref w 8)) (< 1 (vector-ref w 3)))
                     (append (vector->list edge-vector)  (vector->list (vector-ref w 8)))]
                           [else
                     (vector->list edge-vector)]))
                    
               
               (cond [(= 0 (vector-ref w 2))
                      (list (make-posn 500 175) (make-posn 500 400) (make-posn 500 600) (make-posn 500 800))]
                     [(and (= 6 (vector-ref w 3)) (= 1 (vector-ref w 2)))
                      (list bestsofarpos tracepos nextpos backtommpos side-rect-pos (make-posn 700 50) (make-posn 900 50))]
                     [(and (= 6 (vector-ref w 3)) (= 3 (vector-ref w 2)))
                      (list bestsofarpos tracepos nextpos backtommpos side-rect-pos imagepos (make-posn 700 50) (make-posn 900 50))]
                     [(and (= 6 (vector-ref w 3)) (= 5 (vector-ref w 2)))
                      (list bestsofarpos tracepos nextpos backtommpos side-rect-pos imagepos solved%pos (make-posn 700 50) (make-posn 900 50))]
                     [(and (= 6 (vector-ref w 3)) (= 4 (vector-ref w 2)))
                      (list bestsofarpos nextpos backtommpos side-rect-pos solved%pos (make-posn 700 50) (make-posn 900 50))]
                     [(= 1 (vector-ref w 2)) (list bestsofarpos tracepos nextpos backtommpos (make-posn 700 50) (make-posn 900 50))]
                     [(= 2 (vector-ref w 2)) (list (make-posn 200 50) (make-posn 400 50) (make-posn 600 50) (make-posn 800 50) (make-posn 500 200) (make-posn 500 500) (make-posn 500 800))]
                     [(= 3 (vector-ref w 2)) (list bestsofarpos tracepos nextpos backtommpos imagepos (make-posn 700 50) (make-posn 900 50))]
                     [(= 4 (vector-ref w 2)) (list bestsofarpos solved%pos nextpos backtommpos (make-posn 700 50) (make-posn 900 50))]
                     [(= 5 (vector-ref w 2)) (list bestsofarpos solved%pos nextpos backtommpos imagepos (make-posn 700 50) (make-posn 900 50))]))

              (rectangle 1000 1000 "outline" "black")))))

