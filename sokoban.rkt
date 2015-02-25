#lang racket
(require 2htdp/universe 2htdp/planetcute)
(require "super-explorer.rkt")

(define tile-codes #hash((#\H . stone-tall)
                         (#\# . stone-tall)
                         (#\_ . wood)
                         (#\space . wood)
                         (#\S . wood)
                         (#\. . plain)
                         (#\+ . plain)
                         (#\* . plain)))

(define player-codes '(#\A #\x #\+ #\@))

(define gem-codes '(#\* #\$))

(define (get-grid lines)
  (let ([width (apply max (map string-length lines))]
        [grid (make-vector (length lines))])
    (for ([line lines] [row (length lines)])
      (vector-set! grid row (make-vector width 'wood))
      (for ([tile line] [col width])
        (vector-set! (vector-ref grid row) col
                     (dict-ref tile-codes tile 'wood))))
    grid))

(define (find-gems lines)
  (for/fold ([gems '()]) ([line lines] [row (in-naturals)])
    (for/fold ([gems gems]) ([code line] [col (in-naturals)])
      (if (member code gem-codes)
          (cons (item 'gem-blue col row true false) gems)
          gems))))

(define (find-player lines)
  (for/or ([line lines] [row (in-naturals)])
    (for/or ([code player-codes])
      (let ([col (index-of line code)])
        (and col (item 'character-princess-girl col row false false))))))

(define (world-from-code level)
  (let* ([lines (string-split level "\n")])
    (now (find-player lines) (get-grid lines) 0 (find-gems lines) '())))

(define (done? now)
  (for/and ([item (now 'items)])
    (eq? 'plain (vector-ref (vector-ref (now 'grid) (item 'y)) (item 'x)))))

(define (run levels)
  (when (not (empty? levels))
    (undoable-big-bang (world-from-code (first levels)) draw handle-key done?)
    ;; TODO: some way of quitting?
    (run (rest levels))))

(module+ main
  (run (string-split (file->string "microban.mf8") "\n\n")))
