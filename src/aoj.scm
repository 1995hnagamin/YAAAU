(define-module aoj
  (use rfc.http)
  (use sxml.ssax)
  (export aoj-submit aoj-status-log))

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
