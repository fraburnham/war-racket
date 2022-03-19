#lang typed/racket #:with-refinements

(require typed/racket/async-channel
         typed/racket/random
         (prefix-in gen: "generate.rkt")
         (prefix-in red: "redcode.rkt")
         (prefix-in pmars: "pmars.rkt"))

(require/typed racket/vector
  (vector-sort (All (a) (-> (Vectorof a)
                            (-> a a Boolean)
                            (Vectorof a)))))

(struct citizen ((warrior : red:Warrior)
                 (name : String)
                 (filename : String)
                 (points : pmars:Points))
  #:transparent)

(: tribe-winners (Refine (winners : Natural) (= winners 3)))
(define tribe-winners 3)

(: tribe-count (Refine (count : Natural) (= count 10)))
(define tribe-count 10)

(: tribe-size (Refine (size : Natural) (= size 35)))
(define tribe-size 35) ; TODO!: update the other stuff so the breeding program is still sensible

(define-type Warriors (Refine (warriors : (Vectorof red:Warrior))
                              (= 35 (vector-length warriors))))

(define-type Tribe (Refine (tribe : (Vectorof citizen))
                           (= 35 (vector-length tribe))))

#;
(define-type World (Refine (world : (Vectorof Tribe))
                           (= 10 (vector-length world))))
;; It seems like the refinement for Tribe is lost along the way
;; not sure why yet
(define-type World (Listof Tribe))

(: list->warriors (-> (Listof red:Warrior) Warriors))
(define (list->warriors w)
  (let ((warriors (make-vector tribe-size (ann '() red:Warrior)))
        (w (list->vector w)))
    (vector-copy! warriors 0 w 0 tribe-size)
    warriors))

(: vector->warriors (-> (Vectorof red:Warrior) Warriors))
(define (vector->warriors w)
  (let ((warriors (make-vector tribe-size (ann '() red:Warrior))))
    (vector-copy! warriors 0 w 0 tribe-size)
    warriors))

(: warriors->tribe (-> Warriors Tribe))
(define (warriors->tribe warriors)
  (let ((tribe (make-vector tribe-size (citizen '() "" "" 0))))
    (: rec (-> Natural Void))
    (define (rec i)
      (when (< i tribe-size)
        (let ((c : red:Warrior (vector-ref warriors i)))
          (vector-set! tribe i (citizen c "" "" #f))
          (rec (add1 i)))))
    (rec 0)
    tribe))

(: vector->tribe (-> (Vectorof citizen) Tribe))
(define (vector->tribe v)
  (let ((tribe (make-vector tribe-size (citizen '() "" "" 0))))
    (vector-copy! tribe 0 v 0 tribe-size)
    tribe))

;; this can become an update only action! It'll take a tribe in and update them
(: save-tribe (-> Tribe Natural Tribe))
(define (save-tribe tribe offset)
  (: rec (-> Natural Void))
  (define (rec i)
    (when (< i tribe-size)
      (let ((c : citizen (vector-ref tribe i))
            (name : String (number->string (+ i (* 1000 offset)))))
        (vector-set! tribe
                     i
                     (struct-copy citizen
                                  c
                                  (name name)
                                  (filename (gen:save-warrior (citizen-warrior c) name)))))
      (rec (add1 i))))
  (rec 0)
  tribe)

(: tribe-sort (-> Tribe (-> citizen citizen Boolean) Tribe))
(define (tribe-sort tribe proc)
  (let ((sorted (make-vector tribe-size (citizen '() "" "" 0))))
    (vector-copy! sorted 0 (vector-sort tribe proc) 0 tribe-size)
    sorted))

;; I think there is a logic failure in how fitness is being calculated...
;; maybe the issue is related to how scoring is only per tribe so a 4k score may only mean
;; the rest of the tribe was SUPER weak. That makes me think I should have a round of fitness
;; where the winners compete against each other so a champion can be found...
;; also may not be worth writing out the intermediate winners instead of just the final ones...
;; also should consider letting the champion(s) outbreed the rest of the population...
(: fitness (-> Tribe (#:tribe-id-offset Natural) Tribe))
(define (fitness tribe #:tribe-id-offset (offset 1))
  (let* ((tribe : Tribe (save-tribe tribe offset)))
    (vector-copy! ; pop the output back in tribe so the checker can tell it's still a tribe
     tribe
     0
     ;; pull this fn out as attach-fitness or something
     (vector-map (lambda ((c : citizen) (p : pmars:Points)) : citizen
                   (struct-copy citizen c (points p)))
                 tribe
                 (list->vector (pmars:parse-result (pmars:run (vector->list (vector-map citizen-filename tribe))))))
     0
     tribe-size)
    (tribe-sort
     tribe
     (lambda ((a : citizen) (b : citizen)) : Boolean
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
      (: rec (-> red:Warrior red:Warrior red:Warrior))
      (define (rec a b)
        (if (and (empty? a) (empty? b))
            '()
            (if (> (random) 0.5)
                (if (empty? a)
                    (rec a (rest b))
                    (cons (first a) (rec (rest a) (rest b))))
                (if (empty? b)
                    (rec (rest a) b)
                    (cons (first b) (rec (rest a) (rest b)))))))
      (rec a b))))

;; TODO: Refine me!
(: breed (-> (Vectorof red:Warrior) (Vectorof red:Warrior)))
(define (breed warriors)
  ;; this could use list->warriors and an intermediate list I think
  (letrec ((rec : (-> (Vectorof red:Warrior) (Vectorof red:Warrior) (Vectorof red:Warrior))
                (lambda (warriors children)
                  (if (= 1 (vector-length warriors))
                      children
                      (let ((rest (vector-drop warriors 1))
                            (parent : red:Warrior (vector-ref warriors 0)))
                        (rec rest (vector-append
                                   (vector-map (mate parent) rest)
                                   children)))))))
    (rec warriors '#())))

(: new-world (-> World))
(define (new-world)
  (map (lambda (_) : Tribe
         (warriors->tribe (list->warriors (gen:warriors tribe-size 100))))
       (range tribe-count)))

;; this name is terrible
(: tribe-thread (-> (Async-Channelof Tribe) Tribe Natural Thread))
(define (tribe-thread result-chan tribe offset)
  (thread
   (thunk
    (async-channel-put result-chan (fitness tribe #:tribe-id-offset offset)))))

;; still a terrible name...
(: world-threads (-> (Async-Channelof Tribe) World (Listof Thread)))
(define (world-threads result-chan world)
  (if (empty? world)
      '()
      (cons
       (tribe-thread result-chan (first world) (length world))
       (world-threads result-chan (rest world)))))

;; a bad name, too. maybe `select-breeders`?
(: world-best (-> World (Vectorof citizen)))
(define (world-best world)
  ;; TODO: should also take the very worst performer for genetic diversity
  (apply vector-append
         (map (lambda ((tribe : Tribe)) : (Vectorof citizen)
                (vector-take tribe tribe-winners))
              world)))

(: save-citizens (-> (Vectorof citizen) Void))
(define (save-citizens citizens)
  (for ((c citizens))
    (gen:save-warrior (citizen-warrior c)
                      (string-append "warriors/" ; format string would probably be more clear...
                                     (number->string (or (citizen-points c) 0))
                                     "-"
                                     (citizen-name c)
                                     "-"
                                     (number->string (random 1 100))))))

;; there should be a way to convince the checker that the offspring vector
;; is larger than the chunk size...
#;
(: vector-chunk (All (a) (-> ((v : (Vectorof a))
                              (n : (v) (Refine (chunk-size : Natural)
                                               (< chunk-size (vector-length v)))))
                             (Vectorof (Vectorof a)))))
(: vector-chunk (All (a) (-> (Vectorof a) Natural (Vectorof (Vectorof a)))))
;; NB: Chunks < chunk-size are dropped
(define (vector-chunk v chunk-size)
  (if (> chunk-size (vector-length v))
      '#()
      (vector-append
       (vector (vector-take v chunk-size))
       (vector-chunk (vector-drop v chunk-size) chunk-size))))

(: offspring->world (-> (Vectorof red:Warrior) World))
(define (offspring->world offspring)
  (map
   (lambda ((pseudo-warriors : (Vectorof red:Warrior))) : Tribe
     (warriors->tribe (vector->warriors pseudo-warriors)))
   (vector->list (vector-chunk offspring tribe-size))))

(: world-step (-> World World))
(define (world-step world)
  ;; TODO: champion phase where the bests compete so that champions can breed more aggressively?
  ;; TODO: I want to be able to tell which generation the saved warriors are from
  (let* ((chan : (Async-Channelof Tribe) (make-async-channel (ann tribe-count Exact-Positive-Integer)))
         (threads (world-threads chan world)))
    (for ((t threads))
      (thread-wait t))
    (let* ((tribes : World (map (lambda (x) (async-channel-get chan)) (range tribe-count))) ; make this `world` if it doesn't shadow anything important
           (best : (Vectorof citizen) (world-best tribes))
           (offspring : (Vectorof red:Warrior) (breed (vector-map citizen-warrior best))))
      (save-citizens best)
      (take (offspring->world offspring) tribe-count))))

(: evolution (-> Natural World))
(define (evolution number-of-generations)
  (if (zero? number-of-generations)
      (new-world)
      (world-step (evolution (sub1 number-of-generations)))))

;; so now I think the final step may need to involve competing against intentionally built warriors
;; to weed out high scores caused by ties (maybe higher round counts are the answer?)
