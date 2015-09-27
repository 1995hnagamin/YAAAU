(define-module aoj
  (use rfc.http)
  (use sxml.ssax)
  (export aoj-submit aoj-status-log aoj-submission-result))

(select-module aoj)

(define-macro (aif pred . then-els)
  `(let1 it ,pred
     (if ,pred ,@then-els)))

(define-syntax parse-http-responce
  (syntax-rules ()
    ((_ proc responce)
     (guard (exc (else exc))
      (receive (status-code header body) responce
        (parse-responce proc status-code header body))))))

(define-macro (make-alist . properties)
  `(list ,@(map (lambda (prop)
                  `(list (quote ,prop) ,prop))
                properties)))

(define (wait-for-value proc limit second default-proc)
  (let loop ((limit limit))
       (cond
         ((zero? limit) (default-proc))
         ((proc) => identity)
         (else (begin
                 (sys-sleep second)
                 (loop (- limit 1)))))))

(define (assoc-chain-ref aalist keys :optional (default #f) (eq-fn eq?))
  (cond
    ((null? keys) aalist)
    ((assoc-ref aalist (car keys)) => (cut assoc-chain-ref <> (cdr keys) default eq-fn))
    (else #f)))

(define (http-error-description code)
  (case (string-ref code 0)
    ((#\4) (string-append "HTTP Client Error: " code))
    ((#\5) (string-append "HTTP Server Error: " code))
    (else  (stirng-append "unknown HTTP status code: " code))))

(define success-code? (cut string=? <> "200"))

(define (parse-responce proc status-code header body)
  (cond
    ((success-code? status-code)
     (proc (ssax:xml->sxml (open-input-string body) '())))
    (else
      (print status-code header body)
      (http-error-description status-code))))

(define (valid-properties alist) (filter cadr alist))

(define (build-uri path alist)
  (let1 prop (valid-properties alist)
    (if (null? prop)
      path
      `(,path ,@prop))))

(define (aoj-http-post request-uri)
  (http-post "judge.u-aizu.ac.jp" request-uri ""))
(define (aoj-http-get request-uri)
  (http-get "judge.u-aizu.ac.jp" request-uri))

(define (aoj-status-log :key (user_id #f) 
                                 (problem_id #f)
                                 (start #f)
                                 (limit #f))
  (parse-http-responce cdaddr
    (aoj-http-get 
      (build-uri
        "/onlinejudge/webservice/status_log"
        (make-alist user_id problem_id start limit)))))

(define (aoj-submit userID sourceCode problemNO language password)
  (parse-http-responce identity
    (aoj-http-post 
      (build-uri
        "/onlinejudge/webservice/submit"
        (make-alist userID sourceCode problemNO language password)))))

(define (aoj-latest-status user-id)
  (aoj-status-log :user_id user-id :limit 1))

(define (aoj-submission-result user-id source-code problem-no language password
                               :optional (attempt-limits 5) (waiting-time 5))
    (let1 run-id (car (assoc-chain-ref (aoj-latest-status user-id)
                                                      '(status run_id)))
      (aoj-submit user-id source-code problem-no language password)
      (wait-for-value 
        (lambda ()
          (print "waiting for judge result ...")
          (let1 latest-status (aoj-latest-status user-id)
            (if (string=? run-id
                          (car (assoc-chain-ref latest-status '(status run_id))))
              #f
              latest-status)))
        attempt-limits
        waiting-time
        (cut raise "aoj-submission-result: could not get submission result"))))
