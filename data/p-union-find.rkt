#lang racket
(require (only-in rnrs/base-6 assert))
(require (only-in data/p-array
                  [p-array-ref get] 
                  [p-array-set set]
                  [make-p-array create]
                  [p-array-resize extend]))
(provide make-initial-u-f union find allocate-new-index)

;; Jason Hemann and Dan Friedman
;; Modification of Conchon & Filiatre's Persistent Union-Find, ML 2007. 


(define make-u-f (lambda (r p nvi l) `((,p ,nvi . ,l) . ,r)))
(define union-make-u-f (lambda (r p str) (cons (cons p (cdar str)) r)))
(define u-f-rank cdr)
(define u-f-parent caar)
(define u-f-nvi cadar)
(define u-f-length cddar)

(define make-initial-u-f
  (lambda (size)
    (make-u-f (hasheqv)
              (create size (curry +))
              0
              (box size))))

(define find-aux
  (lambda (t i)
    (let ((rep* (get t i)))
      (cond
        ((= rep* i) rep*)
        (else
         (let ((rep* (find-aux t rep*)))
           (set t i rep*)
           rep*))))))

(define find
  (lambda (u-f x)
    (let ((t (u-f-parent u-f)))
      (assert (<= x (u-f-nvi u-f)))
      (let ((rep* (find-aux t x)))
        (set t x rep*)
        rep*))))

(define union
  (lambda (u-f x y)
    (let ((rep-x (find u-f x))
          (rep-y (find u-f y)))
      (cond
        ((= rep-x rep-y) u-f)
        (else 
         (let ((rank (u-f-rank u-f))
               (parent (u-f-parent u-f)))
           (let ((rank-x (hash-ref rank rep-x 0))
                 (rank-y (hash-ref rank rep-y 0)))
             (let ((new-rank (add1 (+ rank-x rank-y))))
               (cond
                 ((< rank-y rank-x)
                  (let ((rank (hash-set rank rep-x new-rank))
                        (parent (set parent rep-y rep-x)))
                    (union-make-u-f rank parent u-f)))
                 (else
                  (let ((rank (hash-set rank rep-y new-rank))
                        (parent (set parent rep-x rep-y)))
                    (union-make-u-f rank parent u-f))))))))))))

(define allocate-new-index
  (lambda (pnl)
    (let ((nvi (cadr pnl))
          (lengthb (cddr pnl)))
      (let ((length (unbox lengthb)))
        (cond
          ((= nvi length)
           (set-box! lengthb (* 2 length))
           (cons (extend (car pnl)) (cons (add1 nvi) lengthb)))
          (else (cons (car pnl) (cons (add1 nvi) lengthb))))))))
