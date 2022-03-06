#lang typed/racket

(require (prefix-in gen: "generate.rkt")
         (prefix-in red: "redcode.rkt")
         (prefix-in pmars: "pmars.rkt"))

(struct citizen ((warrior : red:Warrior) (name : String) (filename : String) (points : pmars:Points)))

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

;; now come up with a breeding strategy and gene selection plan!
