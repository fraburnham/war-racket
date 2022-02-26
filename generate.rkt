#lang typed/racket

;; start with random generation of warrirors in here YAY
;; then work on a fitness func - How to parse the output from pmars?
;; then breeding

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

;; now some tools to render out to a file
