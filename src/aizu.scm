(add-load-path "." :relative)
(use aoj)
(use file.util)
(use gauche.process)
(load "util.scm")

(define *yaaau-config* (expand-path "~/.yaaau/config"))

(define (get-attr-from-file attr)
  (and-let* 
    ((port (open-input-file *yaaau-config* :if-does-not-exist #f))
     (pair (assoc attr (read port))))
    (cadr pair)))

(define (get-from-user message)
  (display message)
  (flush)
  (read-line))

(define (make-attr-getter attr thunk)
  (let1 memo #f
    (lambda ()
      (cond
        (memo memo)
        ((get-attr-from-file attr) => (lambda (obj)
                                        (set! memo obj)
                                        obj))
        (else 
          (begin
            (set! memo (thunk))
            memo))))))

(define get-userID 
  (make-attr-getter 'userID (cut get-from-user "Enter user ID: ")))

(define get-password 
  (make-attr-getter 'password (cut get-from-user "Enter password: ")))

(define get-user-defined-extension-table
  (make-attr-getter 'extensions (lambda () '())))

(define get-attempt-limit
  (make-attr-getter 'attemplt-limit (lambda () 5)))

(define get-waiting-time
  (make-attr-getter 'waiting-time (lambda () 10)))

(define (get-extension filename)
  (search-entry (path-extension filename)
                (list (get-user-defined-extension-table)
                      default-extantion-table)
                (cut throw (format "unknown filetype: ~a" filenamie))))

(define (usage program-name)
  (format (current-error-port) "usage: ~a <command> [<args>]\n" program-name))

(define (submit filename pid lang)
  (let* ((user-id (get-userID))
         (password (get-password))
         (source-code (filename->string filename))
         (lang (or lang (get-extension filename)))
         (result (aoj-submission-result user-id source-code pid lang password
                                        (get-attempt-limit)
                                        (get-waiting-time))))
    (let ((print-result (compose print remove-newline)))
      (print-result (car (assoc-chain-ref result '(status status)))))))

(define (browse-problem args)
  (if (null? args)
    (format (current-error-port) 
            "usage: browse <problem_id>\n")
    (run-process 
      `(xdg-open 
         ,(string-append "http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id="
                         (car args))))))

(define (show-help arg)
  (print help-message))

(define (show-version)
  (print version-message))

(define (main args)
  (if (null? (cdr args))
    (usage (car args))
    (let-args-with-subcommand (cdr args) sub
        ((pid     "p|problem=s" #f)
         (lang    "l|language=s" #f)
         (help    "h|help")
         . restargs)
      (case-with string=? sub
        (("browse") (browse-problem restargs))
        (("help") (show-help restargs))
        (("submit") (submit (car restargs) pid lang))
        (("version") (show-version))
        (else (error (format "unknown command: ~a" sub)))))))

