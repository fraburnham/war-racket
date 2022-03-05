#lang typed/racket

(struct run-result ((output : String) (exit-code : Exact-Number) (error? : Boolean) (error-output : String)))
(struct score ((warrior-file : String) (warrior-name : String) (score : Exact-Number)))

(define-predicate exact-number? Exact-Number)

(: run (-> (Listof String) run-result))
(define (run warrior-files)
  (let* ((out : Output-Port (open-output-string))
         (err : Output-Port (open-output-string))
         (plist (apply process*/ports out #f err "./pmars/pmars-0.8.6/pmars" "-b" "-r" "10" warrior-files))
         (control (last plist)))
    (if (procedure? control)
        (begin
          (control 'wait)
          (let* ((exit-code (control 'exit-code))
                 (exit-code : Exact-Number (if (number? exit-code) exit-code -1)))
            (run-result (get-output-string out)
                        exit-code
                        (not (= 0 exit-code))
                        (get-output-string err))))
        (run-result "" -1 #t "Failed to run process"))))

(: parse-score (-> String String score))
(define (parse-score warrior-file score-line)
  (let* ((split : (Listof String) (string-split score-line ": "))
         (name : String (first split))
         (points (string->number (second split)))
         (points : Exact-Number (if (exact-number? points) points -1)))
    (score warrior-file name points)))

(: parse-result (-> (Listof String) run-result (Listof score)))
(define (parse-result warrior-files result)
  (let ((trimmed (second (string-split (run-result-output result) "\r\r"))))
    (map parse-score warrior-files (string-split trimmed "\n"))))
