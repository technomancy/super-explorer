#lang racket
(require 2htdp/universe 2htdp/image 2htdp/planetcute racket/vector)
(require (prefix-in gui: racket/gui) racket/serialize)           ; for save
(require "fstruct.rkt")

(fstruct now (player world tile items))

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

(define (dict-update-in d ks f . args)
  (if (empty? (rest ks))
      (dict-update d (first ks) (λ (x) (apply f x args)))
      (dict-set d (first ks) (apply dict-update-in (dict-ref d (first ks))
                                    (rest ks) f args))))



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

(define world-size-x 25)
(define world-size-y 25)

(define room-size-x 5)
(define room-size-y 5)

(fstruct item (image x y pushable? pick-up?))



(define (draw-row row-tiles items)
  (for/fold ([image (apply beside (for/list ([tile-name row-tiles])
                                    (tile-image (hash-ref tiles tile-name))))])
            ([item items])
    (overlay/xy (item 'image) (* (- 0 (modulo (item 'x) room-size-x)) (image-width (item 'image)))
                0 image)))

(define (truncate-to x y)
  (* (quotient x y) y))

(define (room-for world item)
  (vector-map (lambda (row) (vector-take (vector-drop row (truncate-to (item 'x) room-size-x)) room-size-x))
              (vector-take (vector-drop world (truncate-to (item 'y) room-size-y)) room-size-y)))

(define (same-room? item1 item2)
  (and (= (quotient (item1 'x) room-size-x) (quotient (item2 'x) room-size-x))
       (= (quotient (item1 'y) room-size-y) (quotient (item2 'y) room-size-y))))

(define (draw now)
  (let* ([room (room-for (now 'world) (now 'player))]
         [items (filter (curry same-room? (now 'player)) (now 'items))])
    (for*/fold ([scene (empty-scene 505 505)])
               ([row (range (vector-length room))])
      (place-image (draw-row (vector-ref room row)
                             (filter (lambda (item) (= row (modulo (item 'y) room-size-x)))
                                     (cons (now 'player) items)))
                   252 (+ 101 (* 80 row)) scene))))



(define (edit-mode? now)
  (equal? selector (now '(player image))))

(define (out-of-bounds? item)
  (or (> (item 'x) world-size-x) (> (item 'y) world-size-y)))

(define (new-place item a-key)
  (cond [(key=? a-key "left") (dict-update item 'x sub1)]
        [(key=? a-key "right") (dict-update item 'x add1)]
        [(key=? a-key "up") (dict-update item 'y sub1)]
        [(key=? a-key "down") (dict-update item 'y add1)]
        [else item]))

(define (walkable? world x y)
  (tile-walkable? (hash-ref tiles (vector-ref (vector-ref world y) x))))

(define (new-room now a-key)
  (cond [(key=? a-key "left") (if (zero? (now '(player x)))
                                  now
                                  (dict-update-in now '(player x) sub1))]
        [(key=? a-key "right") (if (= (now '(player x)) (sub1 world-size-x))
                                   now
                                   (dict-update-in now '(player x) add1))]
        [(key=? a-key "up") (if (zero? (now '(player y)))
                                now
                                (dict-update-in now '(player y) sub1))]
        [(key=? a-key "down") (if (= (now '(player y)) (sub1 world-size-y))
                                  now
                                  (dict-update-in now '(player y) add1))]
        [else now]))

(define (item-at now x y)
  (let ([is (filter (lambda (i) (and (= x (i 'x)) (= y (i 'y))))
                    (now 'items))])
    (if (empty? is)
        false
        (first is))))

(define (move-push-try item now old a-key)
  (let ([new-item (new-place item a-key)])
    (if (and (walkable? (now 'world) (new-item 'x) (new-item 'y))
             (not (item-at now (new-item 'x) (new-item 'y))))
        (now 'items (cons new-item (remove item (now 'items))))
        old)))

(define (move-to-item now old a-key item)
  (cond [(not item) now]
        [(item-pushable? item) (move-push-try item now old a-key)]
        [else now]))

(define (move now a-key)
  (let ([new (now 'player (new-place (now 'player) a-key))])
    (cond [(out-of-bounds? (new 'player)) now]
          [(edit-mode? now) new]
          [(not (walkable? (now 'world) (new '(player x)) (new '(player y))))
           now]
          [else (move-to-item new now a-key (item-at new
                                                     (new '(player x)) 
                                                     (new '(player y))))])))



(define (switch-mode now)
  (now '(player image)
       (if (edit-mode? now)
           character-princess-girl
           selector)))

(define (place now)
  (if (edit-mode? now)
      (now 'world (vector-nested-set (now 'world) (map (now 'player) '(y x))
                                     (list-ref (hash-keys tiles) (now 'tile))))
      now))

(define (next-tile now)
  (now 'tile (modulo (add1 (now 'tile)) (length (hash-keys tiles)))))

(define (prev-tile now)
  (now 'tile (modulo (sub1 (now 'tile)) (length (hash-keys tiles)))))

(define (tile-of now)
  (now 'tile (index-of (hash-keys tiles)
                       (vector-ref (vector-ref (now 'world)
                                               (now '(player y))) 
                                   (now '(player x))))))

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

(define player (item character-princess-girl 2 2 false false))

(module+ main
  (big-bang (now player (call-with-input-file "world.rktd" read) 0
                 (list (item yellow-star 4 2 true false)))
            (on-draw draw)
            (on-key handle-key)))
