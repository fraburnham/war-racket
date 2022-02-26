#lang typed/racket

;; only handling the '88 standard for now can expand later
;; https://corewar.co.uk/standards/icws88.txt

(provide render-warrior
         Opcode
         Opcode-options
         Operand-Modifier
         Operand-Modifier-options
         Warrior
         (struct-out operand)
         (struct-out instruction))

(define-syntax (define-union stx)
  (syntax-case stx ()
    [(_ name union) (with-syntax ((list-name (datum->syntax stx (string->symbol (format "~a-options" (syntax->datum #'name)))))
                                  (list-contents (cons 'list (syntax->datum #'union)))
                                  (union (cons 'U (syntax->datum #'union))))
                      #'(begin
                          (define-type name union)
                          (: list-name (Listof name))
                          (define list-name list-contents)))]))

#|
Warning:
        Missing ';assert'. Warrior may not work with the current setting
Error in line 4: 'MOV #5798, #2406'
        Invalid '88 format. Proper format: 'MOV [#$@<] [$@<]'
Error in line 5: 'SUB #646, #3777'
        Invalid '88 format. Proper format: 'SUB [#$@<] [$@<]'
Error in line 6: 'DJN #4627, #3149'
        Invalid '88 format. Proper format: 'DJN [$@<] [#$@<]'

clearly the modifiers can't be used all willy nilly in the '88 spec
(but that warrior did work for the '94 spec. I wonder if it will be easier to embiggen or restrict...
 I also wonder if I can use types? I'm guessing not since it's a runtime thing... Maybe if I get creative)
|#

(define-union Operand-Modifier ('immediate 'direct 'b-indirect 'b-indirect-predecrement))
(define-union Opcode ('dat 'mov 'add 'sub 'jmp 'jmz 'jmn 'cmp 'slt 'djn 'spl))

;; need to determine the actual range of values for value & set up a better type refinement for it
(struct operand ((modifier : Operand-Modifier) (value : (U Exact-Positive-Integer Zero))))
(struct instruction ((opcode : Opcode) (a-field : operand) (b-field : operand)))

(define-type Warrior (Listof instruction))

(: render-modifier (-> Operand-Modifier String))
(define (render-modifier mod)
  (cond ((equal? mod 'immediate) "#")
        ((equal? mod 'direct) "$")
        ((equal? mod 'b-indirect) "@")
        ((equal? mod 'b-indirect-predecrement) "<")))

(: render-operand (-> operand String))
(define (render-operand op)
  (string-append (render-modifier (operand-modifier op))
                 (number->string (operand-value op))))

(: render-opcode (-> Opcode String))
(define (render-opcode op)
  ;; I don't think upcase is required. Keeping it for style.
  (string-upcase (symbol->string op)))

(: render-instruction (-> instruction String))
(define (render-instruction ins)
  (let ((opcode : Opcode (instruction-opcode ins))
        (a-field : operand (instruction-a-field ins))
        (b-field : operand (instruction-b-field ins)))
    (format "~a ~a, ~a"
            (render-opcode opcode)
            (render-operand a-field)
            (render-operand b-field))))

(: render-warrior (-> Warrior String))
(define (render-warrior warrior)
  (string-join (map render-instruction warrior) "\n"))

;; should add a parser so I can feed winners into the GA...
;; should be fairly simple to parse due to the constrained syntax (and since I'm not compiling anything, just making an intermediate representation)
