#lang racket
(require 2htdp/universe 2htdp/image 2htdp/planetcute)
(require "fstruct.rkt")

(fstruct now (x y room character))



(fstruct tile (image walkable? offset))

(define grass (tile grass-block true 0))
(define water (tile water-block false 0))
(define chest (tile chest-closed false 100))
(define dirt (tile dirt-block true 0))
(define tree (tile (overlay tree-tall grass-block) false 0))

(define stone (tile stone-block true 0))
(define stone-tall (tile stone-block-tall false 0))
(define wall (tile wall-block false 0))
(define wall-tall (tile wall-block-tall false 0))
(define wood (tile wall-block wood-block 0))
(define door (tile door-tall-open true 0))



(define room1 (list (list water grass grass grass tree)
                    (list water water grass grass grass)
                    (list stone stone grass grass grass)
                    (list grass grass grass chest grass)
                    (list stone-tall stone grass grass grass)))

(define room2 (list (list wall-tall wall-tall door wall-tall wall-tall)
                    (list grass grass dirt grass grass)
                    (list grass grass stone stone stone)
                    (list grass grass grass grass grass)
                    (list grass grass grass grass grass)))



(define (draw-row row)
  (apply beside (map tile-image row)))

(define (place-character row-image row character x y)
  (if (= y row)
      (overlay/xy character (* (- 0 x) (image-width character)) 0 row-image)
      row-image))

(define (draw now)
  (let ([x (now 'x)] [y (now 'y)]
        [room (now 'room)] [character (now 'character)])
    (for/fold ([scene (empty-scene 505 505)])
              ([row (range (length room))])
      (place-image (place-character (draw-row (list-ref room row))
                                    row character x y)
                   252 (+ 101 (* 80 row)) scene))))



(define (new-place now a-key)
  (cond [(key=? a-key "left") (now 'x (sub1 (now 'x)))]
        [(key=? a-key "right") (now 'x (add1 (now 'x)))]
        [(key=? a-key "up") (now 'y (sub1 (now 'y)))]
        [(key=? a-key "down") (now 'y (add1 (now 'y)))]
        [else now]))

(define (off-edge? room x y)
  (or (not (<= 0 x (sub1 (length (first room)))))
      (not (<= 0 y (sub1 (length room))))))

(define (allowed? room x y)
  (and (not (off-edge? room x y))
       (tile-walkable? (list-ref (list-ref room y) x))))

(define (new-room now a-key)
  (if (or (key=? a-key "left") (key=? a-key "right"))
      ((now 'room (if (equal? (now 'room) room1) room2 room1))
       'x (if (key=? a-key "left") 4 0))
      now))

(define (move now a-key)
  (let ([new (new-place now a-key)])
    (cond [(allowed? (new 'room) (new 'x) (new 'y)) new]
          [(off-edge? (new 'room) (new 'x) (new 'y))
           (let ([new (new-room now a-key)])
             (if (allowed? (new 'room) (new 'x) (new 'y))
                 new
                 now))]
          [else now])))

(module+ main
  (big-bang (now 2 2 room1 character-boy)
            (on-draw draw)
            (on-key move)))
