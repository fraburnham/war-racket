#lang typed/racket

(require typed/racket/random
         (prefix-in redcode: "redcode.rkt"))

(: instruction (-> redcode:Opcode redcode:instruction))
(define (instruction op)
  (let ((core-size : Exact-Positive-Integer 8000))
    (redcode:instruction op
                         (redcode:operand (first (random-sample redcode:Operand-Modifier-options 1))
                                          (random (add1 core-size)))
                         (redcode:operand (first (random-sample redcode:Operand-Modifier-options 1))
                                          (random (add1 core-size))))))

(: warrior (-> Exact-Positive-Integer redcode:Warrior))
(define (warrior max-instruction-count)
  (map instruction (random-sample redcode:Opcode-options (random (add1 max-instruction-count)))))

;; may need to return some error type if the save fails?
;; oh no that's right this will throw ick
;; so I should wrap the error and return a Bool
(: save-warrior (-> redcode:Warrior String Void))
(define (save-warrior warrior id)
  (with-output-to-file (string-append id ".red")
    (lambda ()
      ;; this should output more metadata eventually
      (displayln (string-append ";name " id))
      (displayln (redcode:render-warrior warrior)))))

(: warriors (-> Exact-Positive-Integer Exact-Positive-Integer (Listof redcode:Warrior)))
(define (warriors number-of-warriors max-instruction-count)
  (letrec ((rec : (-> Integer (Listof redcode:Warrior) (Listof redcode:Warrior))
                (lambda (c warriors)
                  (if (> c 0)
                      (rec (sub1 c)
                           (cons (warrior max-instruction-count) warriors))
                      warriors))))
    (rec number-of-warriors '())))

