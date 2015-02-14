#lang racket
(require 2htdp/universe 2htdp/image 2htdp/planetcute racket/vector)
(require (prefix-in gui: racket/gui) racket/serialize)           ; for save
(require "fstruct.rkt")

(fstruct now (x y world-x world-y world avatar tile))

(define (index-of l x)
  (for/or ([y l] [i (in-naturals)] #:when (equal? x y)) i))

(define (vector-nested-set v indexes new) ; booooo no persistent vectors
  (let ([v (vector-copy v)])
    (vector-set! v (first indexes)
                 (if (empty? (rest indexes))
                     new
                     (vector-nested-set (vector-ref v (first indexes))
                                        (rest indexes) new)))
    v))



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

(define world-size-x 5)
(define world-size-y 5)

(define room-size-x 5)
(define room-size-y 5)



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

(define (walkable? room x y)
  (tile-walkable? (hash-ref tiles (vector-ref (vector-ref room y) x))))

(define (new-room now a-key)
  (cond [(key=? a-key "left") (if (zero? (now 'world-x))
                                  now
                                  (dict-update (now 'x (sub1 room-size-x))
                                               'world-x sub1))]
        [(key=? a-key "right") (if (= (now 'world-x) (sub1 world-size-x))
                                   now
                                   (dict-update (now 'x 0) 'world-x add1))]
        [(key=? a-key "up") (if (zero? (now 'world-y))
                                now
                                (dict-update (now 'y (sub1 room-size-y))
                                             'world-y sub1))]
        [(key=? a-key "down") (if (= (now 'world-y) (sub1 world-size-y))
                                  now
                                  (dict-update (now 'y 0) 'world-y add1))]
        [else now]))

(define (move now a-key)
  (let ([new (new-place now a-key)])
    (cond [(off-edge? (room-for new) (new 'x) (new 'y))
           (let ([new (new-room now a-key)])
             (if (or (edit-mode? now)
                     (walkable? (room-for new) (new 'x) (new 'y)))
                 new
                 now))]
          [(walkable? (room-for now) (new 'x) (new 'y)) new]
          [(edit-mode? now) new]
          [else now])))



(define (switch-mode now)
  (now 'avatar (if (edit-mode? now)
                   character-princess-girl
                   selector)))

(define (place now)
  (if (edit-mode? now)
    (now 'world (vector-nested-set (now 'world) (map now '(world-y world-x y x))
                                   (list-ref (hash-keys tiles) (now 'tile))))
    now))

(define (next-tile now)
  (now 'tile (modulo (add1 (now 'tile)) (length (hash-keys tiles)))))

(define (prev-tile now)
  (now 'tile (modulo (sub1 (now 'tile)) (length (hash-keys tiles)))))

(define (tile-of now)
  (now 'tile (index-of (hash-keys tiles)
                       (vector-ref (vector-ref (room-for now) (now 'y))
                                   (now 'x)))))

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
        [(key=? a-key "\b") (tile-of now)]
        [(key=? a-key "\t") (switch-mode now)]
        [(key=? a-key "s") (save now)]
        [else (move now a-key)]))

(module+ main
  (big-bang (now 2 2 0 0 (call-with-input-file "world.rktd" read)
                 character-princess-girl 0)
            (on-draw draw)
            (on-key handle-key)))
