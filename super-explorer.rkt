#lang racket
(require 2htdp/universe 2htdp/image 2htdp/planetcute)
(require (prefix-in gui: racket/gui) racket/serialize)           ; for save
(require "fstruct.rkt")

(fstruct now (x y world-x world-y world avatar tile))



(fstruct tile (image walkable? offset))

(define tiles `#hash((grass . ,(tile grass-block true 0))
                     (water . ,(tile water-block false 0))
                     (chest . ,(tile chest-closed false 100))
                     (dirt . ,(tile dirt-block true 0))
                     (tree . ,(tile (overlay tree-tall grass-block) false 0))
                     (stone . ,(tile stone-block true 0))
                     (stone-tall . ,(tile stone-block-tall false 0))
                     (wall . ,(tile wall-block false 0))
                     (wall-tall . ,(tile wall-block-tall false 0))
                     (wood . ,(tile wood-block true 0))
                     (door . ,(tile door-tall-closed false 0))
                     (door-open . ,(tile door-tall-open true 0))))



(define (draw-row row)
  (apply beside (for/list ([tile-name row])
                  (tile-image (hash-ref tiles tile-name)))))

(define (place-avatar row-image row avatar x y)
  (if (= y row)
      (overlay/xy avatar (* (- 0 x) (image-width avatar)) 0 row-image)
      row-image))

(define (room-for now)
  (vector-ref (vector-ref (now 'world) (now 'world-y)) (now 'world-x)))

(define (draw now)
  (let ([x (now 'x)] [y (now 'y)]
        [room (room-for now)] [avatar (now 'avatar)])
    (for/fold ([scene (empty-scene 505 505)])
              ([row (range (vector-length room))])
      (place-image (place-avatar (draw-row (vector-ref room row))
                                 row avatar x y)
                   252 (+ 101 (* 80 row)) scene))))



(define (edit-mode? now)
  (equal? selector (now 'avatar)))

(define (new-place now a-key)
     (cond [(key=? a-key "left") (now 'x (sub1 (now 'x)))]
           [(key=? a-key "right") (now 'x (add1 (now 'x)))]
           [(key=? a-key "up") (now 'y (sub1 (now 'y)))]
           [(key=? a-key "down") (now 'y (add1 (now 'y)))]
           [else now]))

(define (off-edge? room x y)
  (or (not (<= 0 x (sub1 (vector-length (vector-ref room 0)))))
      (not (<= 0 y (sub1 (vector-length room))))))

(define (allowed? room x y)
  (and (not (off-edge? room x y))
       (tile-walkable? (hash-ref tiles (vector-ref (vector-ref room y) x)))))

(define (wrap now)
  (let ([x-bound (sub1 (vector-length (vector-ref (now 'world) 0)))]
        [y-bound (sub1 (vector-length (now 'world)))])
    (cond [(negative? (now 'world-x)) (now 'world-x x-bound)]
          [(negative? (now 'world-y)) (now 'world-y y-bound)]
          [(> (now 'world-x) x-bound) (now 'world-x 0)]
          [(> (now 'world-y) y-bound) (now 'world-y 0)]
          [else now])))

(define (new-room now a-key)
  (let ([x-bound (sub1 (vector-length (vector-ref (room-for now) 0)))]
        [y-bound (sub1 (vector-length (room-for now)))])
    (cond [(key=? a-key "left") ((wrap (now 'world-x (sub1 (now 'world-x)))) 'x x-bound)]
          [(key=? a-key "right") ((wrap (now 'world-x (add1 (now 'world-x)))) 'x 0)]
          [(key=? a-key "up") ((wrap (now 'world-y (sub1 (now 'world-y)))) 'y y-bound)]
          [(key=? a-key "down") ((wrap (now 'world-y (add1 (now 'world-y)))) 'y 0)]
          [else now])))

(define (move now a-key)
  (let ([new (new-place now a-key)])
    (cond [(allowed? (room-for now) (new 'x) (new 'y)) new]
          [(off-edge? (room-for new) (new 'x) (new 'y))
           (let ([new (new-room now a-key)])
             (if (or (edit-mode? now)
                     (allowed? (room-for new) (new 'x) (new 'y)))
                 new
                 now))]
          [(edit-mode? now) new]
          [else now])))



(define (switch-mode now)
  (now 'avatar (if (edit-mode? now)
                   character-princess-girl
                   selector)))

(define (place now)
  (when (edit-mode? now)
    (vector-set! (vector-ref (room-for now) (now 'y)) (now 'x)
                 (list-ref (hash-keys tiles) (now 'tile))))
  now)

(define (next-tile now)
  (now 'tile (modulo (add1 (now 'tile)) (length (hash-keys tiles)))))

(define (prev-tile now)
  (now 'tile (modulo (sub1 (now 'tile)) (length (hash-keys tiles)))))

(define (save now)
  (let ([filename (gui:put-file "Save to:")])
    (when filename
      (when (file-exists? filename)
        (delete-file filename))
      (call-with-output-file filename
        (λ (port)
          (write (now 'world) port)))))
  now)

(define (handle-key now a-key)
  (cond [(key=? a-key " ") (place now)] ; edit keys
        [(key=? a-key "[") (next-tile now)]
        [(key=? a-key "]") (prev-tile now)]
        [(key=? a-key "0") (now 'tile 0)]
        [(key=? a-key "\t") (switch-mode now)]
        [(key=? a-key "s") (save now)]
        [else (move now a-key)]))

(module+ main
  (big-bang (now 2 2 0 0 (call-with-input-file "world.rktd" read)
                 character-princess-girl 0)
            (on-draw draw)
            (on-key handle-key)))
