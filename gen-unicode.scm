(use srfi-1)
(use srfi-11)
(use srfi-42)
(use text.unicode.ucd)
(use util.match)
(use gauche.generator)
(use gauche.parseopt)
(use gauche.record)

(use pp)

(set! (ref (pp-format-rules) 'define-module) (pp-scheme-indent 1))
(set! (ref (pp-format-rules) 'export) (pp-scheme-indent -1))

(define-record-type raw-string make-raw-string #f
  (content raw-string-content))

(define-method x->pp ((x raw-string) ctx)
  (raw-string-content x))

(define category-NA 'Cn)

(define other-properties
  '(Other_Alphabetic
    Other_Lowercase
    Other_Uppercase))

(define grapheme-break-properties
  (filter values (ucd-grapheme-break-properties)))

(define (make-table db)
  (define (add-ucs! tbl key ucs)
    (hash-table-update! tbl key
                        (lambda (val)
                          (match val
                            ((((? (cut = <> (- ucs 1))) . beg) . rest)
                             `((,ucs . ,beg) ,@rest))
                            (_
                             `((,ucs . ,ucs) ,@val))))
                        '()))
  (let ((categories (make-hash-table eq-comparator))
        (props (make-hash-table eq-comparator))
        (breaks (make-hash-table eq-comparator)))
    (do-ec (: ucs 0 #x20000)
           (let* ((ent (ucd-get-entry db ucs))
                  (break (ucd-get-break-property db ucs))
                  (cat (if ent (ucd-entry-category ent) category-NA)))
             (add-ucs! categories cat ucs)
             (when break
               (add-ucs! breaks (ucd-break-property-grapheme break) ucs))
             (when ent
               (when (and (ucd-entry-alphabetic ent)
                          (not (memq cat '(Lu Ll Lt Lm Lo Nl))))
                 (add-ucs! props 'Other_Alphabetic ucs))
               (when (and (ucd-entry-lowercase ent)
                          (not (memq cat '(Ll))))
                 (add-ucs! props 'Other_Lowercase ucs))
               (when (and (ucd-entry-uppercase ent)
                          (not (memq cat '(Lu))))
                 (add-ucs! props 'Other_Uppercase ucs)))
             ))
    (let ((ranges (ucd-get-category-ranges db)))
      (for-each (lambda (b e)
                  (let ((beg (car b))
                        (cat (or (cdr b) category-NA))
                        (end (- (car e) 1)))
                    (hash-table-push! categories cat (cons end beg))
                    ))
                ranges
                (append (cdr ranges) '((#x10ffff . #f)))))
    (values categories props breaks)))

(define (generate-module db)
  (define (make-def name ranges)
    `(define ,name
       ,(make-raw-string
         (with-output-to-string
           (lambda ()
             (display "#[")
             (for-each (match-lambda
                         ((end . beg)
                          (if (= end beg)
                              (format #t "\\x~4,'0X;" beg)
                              (format #t "\\x~4,'0X;-\\x~4,'0X;" beg end))))
                       ranges)
             (display "]"))))))
  (let*-values
      (((categories props breaks) (make-table db))
       ((defs) (append
                (map (lambda (cat)
                       (make-def (symbol-append 'char-set:unicode-category-
                                                cat)
                                 (reverse (hash-table-get categories cat '()))))
                     (ucd-general-categories))
                (map (lambda (prop)
                       (make-def (symbol-append 'char-set:unicode-property-
                                                prop)
                                 (reverse (hash-table-get props prop '()))))
                     other-properties)
                (map (lambda (break)
                       (make-def (symbol-append
                                  'char-set:unicode-grapheme-break-
                                  break)
                                 (reverse (hash-table-get breaks break '()))))
                     grapheme-break-properties)
                )))
    (pretty-print
     `(define-module srfi-115.internal.char-set
        (export ,@(map cadr defs))
        ,@defs))))

(define (usage basename)
  (display
   #"usage: ~basename -o output.scm --data /path/to/unicode-data.scm\
   \n"))

(define (main args)
  (define (show-usage status)
    (usage (car args))
    (exit status))
  (define (error-usage message)
    (with-output-to-port (current-error-port)
      (lambda ()
        (when message
          (display #"~(car args): ~message\n\n"))
        (show-usage 1))))
  (let-args (cdr args)
      ((data "data=s" #f)
       (output "o=s")
       (_help "h|help" => (cut show-usage 0))
       (else => (lambda (opt _rest _cont)
                  (error-usage #"unrecognized option: ~opt"))))
    (cond
     ((not output)
      (error-usage "-o is not specified"))
     ((not data)
      (error-usage "--data is not specified"))
     (data
      (with-output-to-file output
        (lambda ()
          (format #t ";;; Generated from ~A. DO NOT EDIT\n"
                  (sys-basename data))
          (generate-module (call-with-input-file data ucd-load-db)))))
     )
    0))
