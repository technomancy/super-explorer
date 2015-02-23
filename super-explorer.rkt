#lang racket
(require 2htdp/universe 2htdp/image 2htdp/planetcute racket/vector)
(require (prefix-in gui: racket/gui) racket/serialize)           ; for save
(require "fstruct.rkt")

(fstruct now (player grid tile items triggers))

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
                     (dirt . ,(tile dirt-block true 0))
                     (tree . ,(tile (overlay tree-tall grass-block) false 0))
                     (stone . ,(tile stone-block true 0))
                     (stone-tall . ,(tile stone-block-tall false 0))
                     (wall . ,(tile wall-block false 0))
                     (wall-tall . ,(tile wall-block-tall false 0))
                     (wood . ,(tile wood-block true 0))
                     (plain . ,(tile plain-block true 0))
                     (door . ,(tile door-tall-closed false 0))
                     (door-open . ,(tile door-tall-open true 0))))

(define grid-size-x 20)
(define grid-size-y 20)

(define room-size-x 10)
(define room-size-y 10)

(fstruct item (image x y pushable? combine))

(define (in-chest item-in chest)
  (item chest-closed (chest 'x) (chest 'y) false false))

;; TODO: come up with a non-eval way to map image name -> image
;; super gnarly because of reasons, but http://pasterack.org/pastes/45527
(define get-image eval)



(define (draw-row row-tiles items)
  (for/fold ([image (apply beside (for/list ([tile-name row-tiles])
                                    (tile-image (hash-ref tiles tile-name))))])
            ([item items])
    (overlay/xy (get-image (item 'image))
                (* (- 0 (modulo (item 'x) room-size-x))
                   (image-width (get-image (item 'image))))
                0 image)))

(define (truncate-to x y)
  (* (quotient x y) y))

(define (room-for grid item)
  (vector-map (lambda (row) (vector-take (vector-drop row (truncate-to (item 'x) room-size-x)) room-size-x))
              (vector-take (vector-drop grid (truncate-to (item 'y) room-size-y)) room-size-y)))

(define (same-room? item1 item2)
  (and (= (quotient (item1 'x) room-size-x) (quotient (item2 'x) room-size-x))
       (= (quotient (item1 'y) room-size-y) (quotient (item2 'y) room-size-y))))

(define (draw now)
  (let ([room (room-for (now 'grid) (now 'player))]
        [items (filter (curry same-room? (now 'player)) (now 'items))])
    (for/fold ([scene (empty-scene (* 101 room-size-x) (* 101 room-size-y))])
              ([row (range (vector-length room))])
      (place-image (draw-row (vector-ref room row)
                             (filter (lambda (item)
                                       (= row (modulo (item 'y) room-size-y)))
                                     (cons (now 'player) items)))
                   (/ (* 101 room-size-x) 2) (+ 101 (* 80 row)) scene))))



(define (edit-mode? now)
  (equal? 'selector (now '(player image))))

(define (out-of-bounds? item)
  (or (> (item 'x) grid-size-x) (> (item 'y) grid-size-y)))

(define (new-place item a-key)
  (cond [(key=? a-key "left") (dict-update item 'x sub1)]
        [(key=? a-key "right") (dict-update item 'x add1)]
        [(key=? a-key "up") (dict-update item 'y sub1)]
        [(key=? a-key "down") (dict-update item 'y add1)]
        [else item]))

(define (walkable? grid x y)
  (tile-walkable? (hash-ref tiles (vector-ref (vector-ref grid y) x))))

(define (item-at now x y)
  (let ([is (filter (lambda (i) (and (= x (i 'x)) (= y (i 'y)))) (now 'items))])
    (and (not (empty? is)) (first is))))

(define (move-push-try item now old a-key)
  (let* ([new-item (new-place item a-key)]
         [item-at-new (item-at now (new-item 'x) (new-item 'y))])
    (cond [(and (walkable? (now 'grid) (new-item 'x) (new-item 'y))
                (not item-at-new))
           (now 'items (cons new-item (remove item (now 'items))))]
          [(and item-at-new (item-combine item-at-new)
                ((eval (item-combine item-at-new)) item item-at-new))
           (now 'items (cons ((eval (item-combine item-at-new))
                              item item-at-new)
                             (remove item-at-new (remove item (now 'items)))))]
          [else old])))

(define (move-to-item now old a-key item)
  (cond [(item-pushable? item) (move-push-try item now old a-key)]
        [else old]))

(define (move now a-key)
  (let* ([new (now 'player (new-place (now 'player) a-key))]
         [item-at-new (item-at new (new '(player x)) (new '(player y)))])
    (cond [(out-of-bounds? (new 'player)) now]
          [(edit-mode? now) new]
          [(not (walkable? (now 'grid) (new '(player x)) (new '(player y)))) now]
          [(not item-at-new) new]
          [else (move-to-item new now a-key item-at-new)])))

(define (trigger-at now x y)
  (dict-ref (now 'triggers) (list x y) false))

(define (teleport x y now)
  ((now '(player x) x) '(player y) y))

(define (grassify x y now)
  (now 'grid (vector-nested-set (now 'grid) (list y x) 'grass)))

(define (move-and-trigger now a-key)
  (let* ([moved (move now a-key)]
         [trigger (trigger-at moved (moved '(player x)) (moved '(player y)))])
    (if trigger
        ((eval trigger) moved)
        moved)))



(define (switch-mode now)
  (now '(player image)
       (if (edit-mode? now)
           'character-princess-girl
           'selector)))

(define (place now)
  (if (edit-mode? now)
      (now 'grid (vector-nested-set (now 'grid) (map (now 'player) '(y x))
                                    (list-ref (hash-keys tiles) (now 'tile))))
      now))

(define (place-item now item)
  (cond [(not (edit-mode? now)) now]
        [(item-at now (now '(player x)) (now '(player y))) now]
        [(not (walkable? (now 'grid) (now '(player x)) (now '(player y)))) now]
        [else (dict-update now 'items (curry cons item))]))

(define (delete-item now)
  (if (edit-mode? now)
      (let ([item (item-at now (now '(player x)) (now '(player y)))])
        (now 'items (remove item (now 'items))))
      now))

(define (next-tile now amt)
  (now 'tile (modulo (+ amt (now 'tile)) (length (hash-keys tiles)))))

(define (tile-of now)
  (now 'tile (index-of (hash-keys tiles)
                       (vector-ref (vector-ref (now 'grid)
                                               (now '(player y)))
                                   (now '(player x))))))

(define (save now)
  (let ([filename (gui:put-file "Save to:")])
    (when filename
      (when (file-exists? filename) (delete-file filename))
      (call-with-output-file filename (curry write (serialize now)))))
  now)

(define (restore old)
  (let ([filename (gui:get-file "Restore:")])
    (if filename
        (call-with-input-file "world.rktd" (compose deserialize read))
        old)))

(define (handle-key now a-key)
  (let ([x (now '(player x))] [y (now '(player y))])
    (cond [(key=? a-key " ") (place now)] ; edit keys
          [(key=? a-key "1") (place-item now (item gem-blue x y true false))]
          [(key=? a-key "2") (place-item now (item chest-open x y #f in-chest))]
          [(key=? a-key "\b") (delete-item now)]
          [(key=? a-key "[") (next-tile now 1)]
          [(key=? a-key "]") (next-tile now -1)]
          [(key=? a-key "`") (tile-of now)]
          [(key=? a-key "\t") (switch-mode now)]
          [(key=? a-key "s") (save now)]
          [(key=? a-key "r") (restore now)]
          [else (move-and-trigger now a-key)])))

(define (undoable-big-bang init draw key-handler)
  (let ([max-undo-size 10] [undo-key "z"])
    (big-bang (list init)
              (to-draw (lambda (states)
                         (with-handlers ([exn:fail?
                                          (λ (e)
                                            (printf "draw error ~s~n" e)
                                            (rectangle (* 101 room-size-x)
                                                       (* 101 room-size-y)
                                                       "solid" "red"))])
                           (draw (first states)))))
              (on-key
               (lambda (states a-key)
                 (with-handlers ([exn:fail?
                                  (λ (e)
                                    (printf "error ~s~n" e)
                                    states)])
                   (if (and (key=? a-key undo-key) (> (length states) 1))
                       (rest states)
                       (cons (key-handler (first states) a-key)
                             (take states (min max-undo-size
                                               (length states)))))))))))

(module+ main
  (undoable-big-bang (call-with-input-file "world.rktd"
                       (compose deserialize read)) draw handle-key))
