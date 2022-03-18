#lang typed/racket

(require typed/racket/async-channel
         typed/racket/random
         (prefix-in gen: "generate.rkt")
         (prefix-in red: "redcode.rkt")
         (prefix-in pmars: "pmars.rkt"))

(define-type Tribe (Listof red:Warrior))
(define-type World (Listof Tribe))

(struct citizen ((warrior : red:Warrior)
                 (name : String)
                 (filename : String)
                 (points : pmars:Points)))

(: tribe-count Positive-Integer)
(define tribe-count 10)

(: tribe-winners Natural)
(define tribe-winners 3)

(: tribe-size Positive-Integer)
(define tribe-size 35) ; TODO!: update the other stuff so the breeding program is still sensible

;; I think there is a logic failure in how fitness is being calculated...
(: fitness (-> (Listof red:Warrior) (#:tribe-id-offset Integer) (Listof citizen)))
(define (fitness population #:tribe-id-offset (offset 1))
  (let* ((population : (Listof citizen) (map (lambda ((c : red:Warrior) (i : Integer)) : citizen
                                               ;; this could be much better. I could be using new directories I think...
                                               (let ((i : String (number->string (+ i (* 1000 offset)))))
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

(: new-world (-> World))
(define (new-world)
  (map (lambda (_) (gen:warriors tribe-size 100)) (range tribe-count)))

;; TODO: should also take the very worst performer for genetic diversity
(: world-step (-> World World))
(define (world-step tribes)
  (let* ((chan : (Async-Channelof (Listof citizen)) (make-async-channel tribe-count))
         (threads (map (lambda ((tribe : (Listof red:Warrior)) (offset : Integer))
                         (thread (lambda ()
                                   (async-channel-put chan (fitness tribe #:tribe-id-offset offset)))))
                       tribes
                       (range tribe-count))))
    (for ((t threads))
      (thread-wait t))
    (let* ((tribes : (Listof (Listof citizen)) (map (lambda (x) (async-channel-get chan)) (range tribe-count)))
           (best : (Listof citizen) (apply append (map (lambda ((citizens : (Listof citizen))) : (Listof citizen)
                                                         (take citizens tribe-winners))
                                                       tribes)))
           (offspring : (Listof red:Warrior) (breed (map citizen-warrior best))))
      (for ((c best))
        (gen:save-warrior (citizen-warrior c)
                          (string-append "warriors/" ; format string would probably be more clear...
                                         (number->string (or (citizen-points c) 0))
                                         "-"
                                         (citizen-name c)
                                         "-"
                                         (number->string (random 1 100)))))
      (letrec ((partition : (-> (Listof red:Warrior) World)
                          (lambda (warriors)
                            (if (empty? warriors)
                                '()
                                (cons (take warriors tribe-size) (partition (drop warriors tribe-size)))))))
        (partition (random-sample offspring (* tribe-size tribe-count)))))))

(: evolution (-> Natural World))
(define (evolution number-of-generations)
  (if (zero? number-of-generations)
      (new-world)
      (world-step (evolution (sub1 number-of-generations)))))
