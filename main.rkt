#lang racket/base

(require web-server/servlet web-server/servlet-env web-server/dispatch (planet jaymccarthy/mongodb)
         racket/dict racket/sequence openssl/sha1 racket/sandbox racket/match
         racket/runtime-path)

(define mongo (create-mongo))
(define db (mongo-db mongo "rktbin"))

(current-mongo-db db)

(define-mongo-struct paste "pastes"
  ([hash #:immutable]
   [content #:immutable]   
   [result #:immutable]
   [date #:immutable]
   [parent #:immutable]))

(define-runtime-path static "./")

(define styles
  `(
    (link ((type "text/css")
           (rel "stylesheet")
           (href "/assets/style.css")))
    (link ((type "text/css")
           (rel "stylesheet")
           (href "/assets/media_queries.css")))
    (link ((type "text/css")
           (rel "stylesheet")
           (href "/assets/normalize.css")))
    (link ((type "text/css")
           (rel "stylesheet")
           (href "/assets/font-awesome.css")))
    (link ((type "text/css")
           (rel "stylesheet")
           (href "http://fonts.googleapis.com/css?family=Droid+Sans+Mono")))))


(define hdr
  `(header ((role "banner"))
           (a ((href "/"))
              (h1 "paste.rkt"))))

(define footer
  `(footer (p "Made by Sam Tobin-Hochstadt, styling stolen from cljbin")))

(define default-placeholder "Racket code here")

(define (main-page req [parent-id #f])
  (define parent 
    (cond [parent-id
           (define q (sequence->list (mongo-dict-query "pastes" (hash 'hash parent-id))))
           (and (pair? q) (car q))]
          [else #f]))
  (response/xexpr
   `(html (head (title "paste.rkt") ,@styles)
          (body
           ,hdr
           (section ([id "paste"])
                    (form ((action "/new") (method "post"))
                          ,@(if parent 
                                `((input ([id "fork-of"] [type "hidden"] [value ,parent-id] [name "fork-of"])))
                                `())
                          (div ([class "code"])
                               (textarea ((id "code")
                                          (name "code")
                                          [spellcheck "false"]
                                          [autofocus "yes"]
                                          ,@(if parent
                                                `()
                                                `((placeholder ,default-placeholder))))
                                         ,(if parent (paste-content parent) ""))
                               ,@(if parent
                                     `((div ,(format-code (paste-content parent)))
                                       (ul ([class "output"])
                                           ,@(format-result (paste-result parent))))
                                     `()))
                          (ul ([class "actions"])
                              (li (button (i ([class "icon-play"]))
                                          "Paste and Run")))))
           ,footer))))

(define (format-result v)
  (match v
    [(vector s _ _)
     (for/list ([l (regexp-split "\n" s)])
       `(li (pre ,l)))]
    ;; FIXME -- reenable later
    [(vector s o e) `(div (pre ,s)
                           ,@(if o
                                 `((h3 "Standard Out")
                                   (pre ,o))
                                 null)
                           ,@(if e
                                 `((h3 "Standard Error")
                                   (pre ,e))
                                 null))]))

(define (show-paste req id)
  (define q (sequence->list (mongo-dict-query "pastes" (hash 'hash id))))
  (response/xexpr
   (cond [(null? q)
          `(html (head (title "paste.rkt") ,@styles)
                 ,hdr ,footer)]
         [else (define p (car q))
               (define parent (paste-parent p))
               (define result (paste-result p))
               (define content (paste-content p))
               (define when (paste-date p))
               `(html (head (title "paste.rkt")
                            ,@styles)
                      (body 
                       ,hdr
                       (section ([id "paste"])
                                (form ((action ,(string-append "/fork/" id)) (method "get"))
                                      (div ([class "code"]) ,(format-code content))
                                           
                                      (ul ([class "output"])
                                          ,@(format-result result))
                                      ,(if parent
                                           `(div ([id "fork-of"])
                                                 "Fork of "
                                                 (a ([href ,(->url show-paste parent)])
                                                    ,parent))
                                           `(div))
                                      (p ([class "meta"]) 
                                         ,(let* ([now (current-seconds)]
                                                 [days (quotient (- now when) (* 24 60 60))])
                                            (format "Pasted ~a day~a ago." days (if (= 1 days) "" "s"))))
                                      (ul ([class "actions"])
                                          (li (button (i ([class "icon-random"]))
                                                      "Fork")))))
                       ,footer))])))

(define (format-code c)
  (define lines
    (for/list ([l (regexp-split "\n" c)])
      `(div ([class "line"]) (code ,l))))
  `(div
    ([class "syntaxhighlighter"])
    (table 
     ([cellspacing "0"] [cellpadding "0"] [border "0"])
     (tbody
      (tr
       (td ([class "code"])
           (div ([class "container"])
                ,@lines)))))))
  
(define (run-code str)
  (define ev
    (parameterize ([sandbox-output 'string]
                   [sandbox-error-output 'string]
                   [sandbox-propagate-exceptions #f]
                   [sandbox-eval-limits (list 10 50)]
                   [sandbox-path-permissions '([exists "/"])])
       (call-with-limits 10 #f
                         (lambda () (make-evaluator '(begin (require racket)))))))
  (define res (ev str))
  (define out (get-output ev))
  (define err (get-error-output ev))
  (kill-evaluator ev)
  (list (if (void? res) "" (format "~v" res))
        (and (not (equal? out "")) out)
        (and (not (equal? err "")) err)))

(define (new-paste req)
  (define content (dict-ref (request-bindings req) 'code #f))
  (define parent-id (dict-ref (request-bindings req) 'fork-of #f))
  (define parent (and parent-id content
                      (not (null? (sequence->list
                                   (mongo-dict-query "pastes" (hash 'hash parent-id)))))
                      parent-id))
  (cond [content
         (define hash (sha1 (open-input-string 
                             (string-append (number->string (current-inexact-milliseconds))
                                            content))))
         ;; put it in the db
         (define p (make-paste #:hash hash #:content content
                               #:result (run-code content)
                               #:date (current-seconds)
                               #:parent parent))
                  
         ;; evaluation goes here
         ;; redirect to the permanent page
         (define new-url (->url show-paste hash))
         (redirect-to new-url)]
        [else (response/xexpr
               '(html (head (title "Failed") ,@styles)
                      (body (h1 "Failed"))))]))

(define-values (dispatch ->url)
  (dispatch-rules
   [("") #:method "get" main-page]
   [("new") #:method "post" new-paste]
   [() #:method "post" new-paste]
   [("paste" (string-arg)) show-paste]
   [("fork" (string-arg)) main-page]))

(serve/servlet dispatch
               #:listen-ip #f
               #:extra-files-paths (list static)
               #:servlet-regexp #rx""
               #:servlet-path "")

