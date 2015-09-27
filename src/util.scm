(use gauche.parseopt)

(define-macro (apuntil pred? obj . body)
  `(let1 it #f
     (while (begin
              (set! it ,obj)
              (not (,pred? it)))
      ,@body)))

(define-syntax let-args-with-subcommand
  (syntax-rules ()
    ((_ args subcommand bind-specs body ...)
     (let1 subcommand (car args)
       (let-args (cdr args)
          bind-specs
          body ...)))))

(define-macro (with-gensyms names . body)
  `(let ,(map (lambda (name) (list name `(quote ,(gensym)))) names)
      ,@body))

(define-macro (case-with eql? keyword . clauses)
  (with-gensyms (key)
    `(let1 ,key ,keyword
       (cond
         ,@(map (lambda (clause)
                  (cons (if (eq? 'else (car clause))
                          'else
                          `(any (cut ,eql? <> ,key) ',(car clause))) 
                        (cdr clause)))
                clauses)))))

(define-macro (macro1 name)
  `(if ,name 'true 'false))

(define (filename->string filename)
  (call-with-input-file filename 
    (lambda (iport)
      (call-with-output-string 
        (lambda (oport)
          (apuntil eof-object? (read-line iport)
            (display it oport)
            (newline oport))
          (get-output-string oport))))))

(define (search-entry key entry default)
  (let search ((entry entry))
    (cond
      ((null? entry) (default))
      ((assoc key (car entry)) => cdr)
      (else (search (cdr entry))))))

(define default-extantion-table
  '(("c" . "C")
    ("cc" . "C++")
    ("cpp" . "C++")
    ("cxx" . "C++")
    ("java" . "Java")
    ("cs" . "C#")
    ("d" . "D")
    ("rb" . "Ruby")
    ("py" . "Python3")
    ("php" . "PHP")
    ("js" . "JavaScript")
    ("scala" . "Scala")
    ("hs" . "Haskell")
    ("ml" . "OCaml")))

(define help-message "\
  usage: aizu <command> [<args>]\n\
  \n\
  commands:\n\
  submit  Submit solution\n\
  browse  Browse problem page on web browser\n\
  help    show this help\n")
