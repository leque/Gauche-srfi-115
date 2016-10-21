;;;
;;; Test srfi-115
;;;

(use gauche.test)

(use srfi-14)
(use srfi-42)
(use text.unicode.ucd)
(use util.match)
(use gauche.version)

(test-start "srfi-115")
(test-section "srfi-115")
(use srfi-115)
(test-module 'srfi-115)

(test-section "srfi-115.sre")
(use srfi-115.sre)
(test-module 'srfi-115.sre)

(test-section "srfi-115.internal.char-set")
(use srfi-115.internal.char-set)
(test-module 'srfi-115.internal.char-set)

(define-syntax %category->char-set
  (er-macro-transformer
    (lambda (form rename compare)
      (define (gen-case cat)
        `((,(rename cat))
          ,(rename (symbol-append 'char-set:unicode-category- cat))))
      (match form
        ((_ cat)
         (quasirename rename
           (case ,cat
             ,@(map gen-case (ucd-general-categories)))))))))

(define (char-general-category->char-set cat)
  (%category->char-set cat))

(test* "char-set completeness"
       #t
       (every?-ec (: i 0 #x10ffff)
                  (if-let1 c (ucs->char i)
                    (or (char-set-contains? (char-general-category->char-set
                                             (char-general-category c))
                                            c)
                        ;; workaround for a Gauche's bug
                        ;; https://github.com/shirok/Gauche/pull/245
                        (and (version<=? (gauche-version) "0.9.5")
                             (= i #x20000)
                             (char-set-contains? char-set:unicode-category-Lo
                                                 c))
                        (errorf "unmatch: #x~x (~S)" i (char-general-category c)))
                    #t)))

(define (test-char-set-soundness cat)
  (test* #"char-set soundness ~cat"
         #t
         (char-set-every
          (lambda (c)
            (or (eq? (char-general-category c) cat)
                (error "unmatch: #x~x (in char-set:unicode-category-~S) ~S"
                       c
                       (char-general-category c)
                       cat)))
          (char-general-category->char-set cat))))

(dolist (c (ucd-general-categories))
  (test-char-set-soundness c))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
