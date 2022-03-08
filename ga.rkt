#lang typed/racket

(require (prefix-in gen: "generate.rkt")
         (prefix-in red: "redcode.rkt")
         (prefix-in pmars: "pmars.rkt"))

(struct citizen ((warrior : red:Warrior)
                 (name : String)
                 (filename : String)
                 (points : pmars:Points)))

(: tribe-count Natural)
(define tribe-count 10)

(: tribe-winners Natural)
(define tribe-winners 3)

(: tribe-size Natural)
(define tribe-size 40)

(: fitness (-> (Listof red:Warrior) (Listof citizen)))
(define (fitness population)
  (let* ((population : (Listof citizen) (map (lambda ((c : red:Warrior) (i : Integer)) : citizen
                                               (let ((i : String (number->string i)))
                                                 (citizen c i (gen:save-warrior c i) #f)))
                                             population
                                             (range (length population)))))
    (sort
     (map (lambda ((c : citizen) (p : pmars:Points)) : citizen
            (struct-copy citizen c (points p)))
          population
          (pmars:parse-result (pmars:run (map citizen-filename population))))
     (lambda ((a : citizen) (b : citizen))
       (let* ((a-points : pmars:Points (citizen-points a))
              (a-points : Integer (if (integer? a-points) a-points 0))
              (b-points : pmars:Points (citizen-points b))
              (b-points : Integer (if (integer? b-points) b-points 0)))
         (> a-points b-points))))))

(: mate (-> red:Warrior (-> red:Warrior red:Warrior)))
(define (mate a)
  (lambda ((b : red:Warrior)) : red:Warrior
    (let ((rest : (-> red:Warrior red:Warrior)
                (lambda (a)
                  (if (empty? a) '() (rest a)))))
      (letrec ((rec : (-> red:Warrior red:Warrior red:Warrior)
                    (lambda (a b)
                      (if (and (empty? a) (empty? b))
                          '()
                          (if (> (random) 0.5)
                              (if (empty? a)
                                  (rec a (rest b))
                                  (cons (first a) (rec (rest a) (rest b))))
                              (if (empty? b)
                                  (rec (rest a) b)
                                  (cons (first b) (rec (rest a) (rest b)))))))))
        (rec a b)))))

(: breed (-> (Listof red:Warrior) (Listof red:Warrior)))
(define (breed warriors)
  (letrec ((rec : (-> (Listof red:Warrior) (Listof red:Warrior) (Listof red:Warrior))
                (lambda (warriors children)
                  (if (= 1 (length warriors))
                      children
                      (let ((rest (rest warriors))
                            (parent (first warriors)))
                        (rec rest (append (map (mate parent) rest) children)))))))
    (rec warriors '())))

;; so the whole flow will be (probably in some main.rkt file)
;; make generation -> write files out -> get fitness ->
;; save winners to disk -> create next generation

(define-type Tribe (Listof Warrior))
(: next-world (-> (Listof Tribe) (Listof Tribe)))
(define (next-world t)
  t)
