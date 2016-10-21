;;;
;;; Copyright (c) 2016 OOHASHI Daichi <dico.leque.comicron@gmail.com>
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
(define-module srfi-115.sre
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use util.match)
  (use srfi-115.internal.char-set)
  (export regexp rx regexp->sre char-set->sre valid-sre? regexp?))

(select-module srfi-115.sre)

;; API
(define (regexp re)
  (if (regexp? re)
      re
      (regexp-compile
       (regexp-optimize
        (sre-parse re)))))

;; API
(define-syntax rx
  (syntax-rules ()
    ((_ sre ...)
     (regexp `(seq sre ...)))))

;; API
(define (regexp->sre re)
  (define-values (register-name! find-name)
    (let ((env '()))
      (define (register-name! name i)
        (set! env (alist-cons i name env)))
      (define (find-name i)
        (assv-ref env i #f))
      (values register-name! find-name)))
  (define (r* asts)
    (map-in-order recur asts))
  (define (recur ast)
    (match ast
      ((? char? c)
       c)
      ((? char-set? cs)
       (char-set->sre cs))
      (('comp . (? char-set? cs))
       `(~ ,(char-set->sre cs)))
      ('any
       '(w/unicode any))
      ('bol
       'bos)
      ('eol
       'eos)
      ('wb
       `(w/ascii (or bow eow)))
      ('nwb
       `(w/ascii nwb))
      (('seq asts ...)
       `(: ,@(r* asts)))
      (('seq-uncase asts ...)
       `(w/nocase (: ,@(r* asts))))
      (('seq-case asts ...)
       `(w/case (: ,@(r* asts))))
      (('alt asts ...)
       `(or ,@(r* asts)))
      (('rep 0 #f asts ...)
       `(* ,@(r* asts)))
      (('rep 1 #f asts ...)
       `(+ ,@(r* asts)))
      (('rep 0 1 asts ...)
       `(? ,@(r* asts)))
      (('rep n #f asts ...)
       `(>= ,n ,@(r* asts)))
      (('rep m n asts ...)
       (if (= m n)
           `(= ,m ,@(r* asts))
           `(** m n ,@(r* asts))))
      (('rep-min 0 #f asts ...)
       `(*? ,@(r* asts)))
      (('rep-min 0 1 asts ...)
       `(?? ,@(r* asts)))
      (('rep-min m n asts ...)
       `(**? m n ,@(r* asts)))
      (('rep-while . _)
       (error "no-backtrack repetion is not supported in sre" re))
      (((? integer? n) name asts ...)
       (cond ((zero? n)
              `(seq ,@(r* asts)))
             (name
              (register-name! name n)
              `(submatch-named ,name ,@(r* asts)))
             (else
              `(submatch ,name ,@(r* asts)))))
      (('cpat . _)
       (error "conditional expression is not supported in sre" re))
      (('backref . n)
       `(backref ,(or (find-name n) n)))
      (('once . _)
       (error "standalone pattern is not supported in sre" re))
      (('assert (lookbehind asts ...))
       `(look-behind ,@(r* asts)))
      (('assert asts ...)
       `(look-ahead ,@(r* asts)))
      (('nassert (lookbehind asts ...))
       `(neg-look-behind ,@(r* asts)))
      (('nassert asts ...)
       `(neg-look-ahead ,@(r* asts)))
      (ast
       (error "unknown regexp ast" ast))
      ))
  (recur (regexp-ast re)))

(define (valid-sre? sre)
  (guard (_
          (else #f))
    (sre-parse sre)
    #t))

;;; API
(define (char-set->sre cs)
  (let ((chars-port (open-output-string))
        (range-port (open-output-string)))
    (define (put start end)
      (cond ((or (not start) (not end))
             #f)
            ((char=? start end)
             (display start chars-port))
            (else
             (display #"~|start|~|end|" range-port))))
    (let loop ((cursor (char-set-cursor cs))
               (start #f)
               (prev #f))
      (cond
       ((end-of-char-set? cursor)
        (put start prev)
        (let ((range (and-let* ((s (get-output-string range-port))
                                ((not (string-null? s))))
                       `(/ ,s)))
              (chars (and-let* ((s (get-output-string chars-port))
                                ((not (string-null? s))))
                       `(,s))))
          (if (and range chars)
              `(or ,range ,chars)
              (or range chars '("")))))
       (else
        (let ((c (char-set-ref cs cursor)))
          (cond ((and prev
                      (= 1 (- (char->integer c)
                              (char->integer prev))))
                 (loop (char-set-cursor-next cs cursor) start c))
                (else
                 (put start prev)
                 (loop (char-set-cursor-next cs cursor) c c)))))))))

(define-constant eol-pat-ast
  '(alt #\return #\newline
        (seq #\return #\newline)))

(define (sre-parse sre)
  `(0 #f ,(%sre-parse sre)))

(define (%sre-parse sre)
  (define-values (new-group! new-named-group! extend-env! lookup-env)
    (let ()
      (define group-count 0)
      (define name-env '())
      (define (extend-env! name count)
        (set! name-env (alist-cons name count name-env)))
      (define (new-group!)
        (inc! group-count)
        group-count)
      (define (new-named-group! name)
        (rlet1 n (new-group!)
          (extend-env! name n)))
      (define (lookup-env name)
        (cond
         ((and (symbol? name)
               (assq name name-env))
          => cdr)
         ((<= name group-count)
          name)
         (else
          (error "unknown capturing-group:" name))))
      (values new-group! new-named-group! extend-env! lookup-env)))
  (define (nat? x)
    (and (integer? x) (>= x 0)))
  (let loop ((sre sre)
             (ascii? #f)
             (case? #t)
             (capture? #t))
    (define (recur x)
      (loop x ascii? case? capture?))
    (define (%rep sym m n sres)
      (when (and n (> m n))
        (errorf "~S: ~A should be less than or equal to ~A" sre m n))
      `(,sym ,m ,n ,@(map-in-order recur sres)))
    (define (rep m n sres)
      (%rep 'rep m n sres))
    (define (rep-min m n sres)
      (%rep 'rep-min m n sres))
    (match sre
      ((? string? s)
       (if case?
           `(seq ,@(string->list s))
           (if ascii?
               `(seq-uncase ,@(string->list s))
               `(seq ,@(map (lambda (c)
                              (char-set c (char-foldcase c)))
                            (string->list s))))))
      (((or '* 'zero-or-more) sres ...)
       (rep 0 #f sres))
      (((or '+ 'one-ore-more) sres ...)
       (rep 1 #f sres))
      (((or '? 'optional) sres ...)
       (rep 0 1 sres))
      (((or '= 'exactly) (? nat? n) sres ...)
       (rep n n sres))
      (((or '>= 'at-least) (? nat? n) sres ...)
       (rep n #f sres))
      (((or '** 'repeated) (? nat? m) (? nat? n) sres ...)
       (rep m n sres))
      (((or 'or '|\||) sres ...)
       `(alt ,@(map-in-order recur sres)))
      (((or ': 'seq) sres ...)
       `(seq ,@(map-in-order recur sres)))
      (((or '$ 'submatch) sres ...)
       (if capture?
           (let ((n (new-group!)))
             `(,n #f ,@(map-in-order recur sres)))
           `(seq ,@(map-in-order recur sres))))
      (((or '-> 'submatch-named) (? symbol? name) sres ...)
       (if capture?
           (let ((n (new-named-group! name)))
             `(,n ,name ,@(map-in-order recur sres)))
           `(seq ,@(map-in-order recur sres))))
      (('w/case sres ...)
       (loop `(seq ,@sres) ascii? #t capture?))
      (('w/nocase sres ...)
       (loop `(seq ,@sres) ascii? #f capture?))
      (('w/unicode sres ...)
       (loop `(seq ,@sres) #f case? capture?))
      (('w/ascii sres ...)
       (loop `(seq ,@sres) #t case? capture?))
      (('w/nocapture sres ...)
       (loop `(seq ,@sres) ascii? case? #f))
      ('bos
       ;; NB: Gauche's ^ matches beginning of strings only
       'bol)
      ('eos
       ;; NB: Gauche's $ matches end of strings only
       'eol)
      ('bol
       `(assert (lookbehind (alt bol ,eol-pat-ast))))
      ('eol
       `(assert (alt eol ,eol-pat-ast)))
      ('bog
       (if ascii?
           `(seq)
           (error "unsupported yet")))
      ('eog
       (if ascii?
           `(seq)
           (error "unsupported yet")))
      ('grapheme
       (if ascii?
           char-set:ascii
           (error "unsupported yet")))
      ('bow
       (let ((w (char-set:word-constituent ascii? case?)))
         `(seq (nassert (lookbehind ,w))
               (assert ,w))))
      ('eow
       (let ((w (char-set:word-constituent ascii? case?)))
         `(seq (assert (lookbehind ,w))
               (nassert ,w))))
      ('nwb
       (recur `(neg-look-ahead (or bow eow))))
      (('word sres ...)
       (recur `(seq bow ,@sres eow)))
      (('word+ cset-sre ...)
       (let ((cset (char-set-intersection
                    (char-set:word-constituent ascii? case?)
                    (cset-sre-parse `(or ,@cset-sre)
                                    ascii?
                                    case?))))
         `(seq (rep 1 #f ,cset))))
      ('word
       `(rep 1 #f ,(char-set:word-constituent ascii? case?)))
      (((or '?? 'non-greedy-optional) sres ...)
       (rep-min 0 1 sres))
      (((or '*? 'non-greedy-zero-or-more) sres ...)
       (rep-min 0 #f sres))
      (((or '**? 'non-greedy-repeated) (? nat? m) (? nat? n) sres ...)
       (rep-min m n sres))
      (('look-ahead sres ...)
       `(assert ,@(map-in-order recur sres)))
      (('look-behind sres ...)
       `(assert (lookbehind ,@(map-in-order recur sres))))
      (('neg-look-ahead sres ...)
       `(nassert ,@(map-in-order recur sres)))
      (('neg-look-behind sres ...)
       `(nassert (lookbehind ,@(map-in-order recur sres))))
      (('backref (or (? nat? n) (? symbol? n)))
       `(backref . ,(lookup-env n)))
      (cset
       (cset-sre-parse cset ascii? case?)))))

(define (char-swapcase c)
  (if (char-upper-case? c)
      (char-downcase c)
      (char-upcase c)))

(define (char-set-uncase cs)
  (char-set-fold (lambda (c res)
                   (char-set-adjoin!
                    (char-set-adjoin! res (char-swapcase c))
                    c))
                 (char-set)
                 cs))

(define (cset-sre-parse sre ascii? case?)
  (define (may-uncase cset)
    (if case?
        cset
        (char-set-uncase cset)))
  (define (cset-complement cset)
    (if ascii?
        (char-set-intersection char-set:ascii (char-set-complement cset))
        (char-set-complement cset)))
  (let recur ((sre sre))
    (match sre
      ((? char? c)
       (may-uncase (char-set c)))
      ((? string? s)
       (if (= 1 (string-length s))
           (may-uncase (char-set (string-ref s 0)))
           (error "invalid sre char-set:" s)))
      ((? char-set? cs)
       cs)
      ((or ((? string? s))
           ('char-set (? string? s)))
       (may-uncase (string->char-set s)))
      (((or '/ 'char-range) range-specs ...)
       (may-uncase
        (rlet1 cs (char-set)
          (let* ((spec (with-output-to-string
                         (lambda ()
                           (for-each
                            (lambda (x)
                              (unless (or (char? x) (string? x))
                                (error "range-spec should be a char or a string"
                                       x))
                              (display x))
                            range-specs))))
                 (len (string-length spec)))
            (unless (even? len)
              (error "length of range-spec is not even:" range-specs))
            (do ((i 0 (+ i 2)))
                ((>= i len))
              (ucs-range->char-set! (char->ucs (string-ref spec i))
                                    (+ 1 (char->ucs (string-ref spec (+ i 1))))
                                    #f
                                    cs))))))
      (((or 'or '|\||) csets ...)
       (apply char-set-union (map recur csets)))
      (((or 'and '&) csets ...)
       (apply char-set-intersection (map recur csets)))
      (((or '- 'difference) cset csets ...)
       (apply char-set-difference (recur cset) (map recur csets)))
      (((or '~ 'complement) csets ...)
       (cset-complement (recur `(or ,@csets))))
      ((? symbol? s)
       (named-cset-sre-parse s ascii? case?))
      (('w/case csets ...)
       (cset-sre-parse `(or ,@csets) ascii? #t))
      (('w/nocase csets ...)
       (cset-sre-parse `(or ,@csets) ascii? #f))
      (('w/ascii csets ...)
       (cset-sre-parse `(or ,@csets) #t case?))
      (('w/unicode csets ...)
       (cset-sre-parse `(or ,@csets) #f case?))
      (_
       (error "invalid sre:" sre)))))

(define char-set:nonl
  (char-set-complement! (char-set #\return #\newline)))

(define char-set:ascii-lower-case
  #[a-z])

(define char-set:ascii-upper-case
  #[A-Z])

(define char-set:ascii-alphabetic
  (char-set-union
   char-set:ascii-lower-case
   char-set:ascii-upper-case
   ))

(define char-set:ascii-numeric
  #[0-9])

(define char-set:ascii-alphanumeric
  (char-set-union
   char-set:ascii-alphabetic
   char-set:ascii-numeric))

(define char-set:ascii-punctuation
  (string->char-set "!\"#%&'()*,-./:;?@[\]_{}"))

(define char-set:ascii-symbol
  (string->char-set "$+<=>^`|~"))

(define char-set:ascii-graphic
  (char-set-union
   char-set:ascii-alphanumeric
   char-set:ascii-punctuation
   char-set:ascii-symbol
   ))

(define char-set:ascii-whitespace
  #[ \t\r\n\v])

(define char-set:ascii-printing
  (char-set-union
   char-set:ascii-graphic
   char-set:ascii-whitespace
   ))

(define char-set:ascii-control
  (ucs-range->char-set 0 32))

(define char-set:unicode-lower-case
  (char-set-union
   char-set:unicode-category-Ll
   char-set:unicode-property-Other_Lowercase
   ))

(define char-set:unicode-upper-case
  (char-set-union
   char-set:unicode-category-Lu
   char-set:unicode-property-Other_Uppercase
   ))

(define char-set:unicode-title-case
  char-set:unicode-category-Lt)

(define char-set:unicode-cased
  (char-set-union
   char-set:unicode-lower-case
   char-set:unicode-upper-case
   char-set:unicode-title-case
   ))

(define char-set:unicode-alphabetic
  (char-set-union
   char-set:unicode-category-Lu
   char-set:unicode-category-Ll
   char-set:unicode-category-Lt
   char-set:unicode-category-Lm
   char-set:unicode-category-Lo
   char-set:unicode-category-Nl
   char-set:unicode-property-Other_Alphabetic
   ))

(define char-set:unicode-numeric
  (char-set-union
   char-set:unicode-category-Nd
   ))

(define char-set:unicode-alphanumeric
  (char-set-union
   char-set:unicode-alphabetic
   char-set:unicode-numeric))

(define char-set:unicode-punctuation
  (char-set-union
   char-set:unicode-category-Pc
   char-set:unicode-category-Pd
   char-set:unicode-category-Ps
   char-set:unicode-category-Pe
   char-set:unicode-category-Pi
   char-set:unicode-category-Pf
   char-set:unicode-category-Po
   ))

(define char-set:unicode-symbol
  (char-set-union
   char-set:unicode-category-Sm
   char-set:unicode-category-Sc
   char-set:unicode-category-Sk
   char-set:unicode-category-So
   ))

(define char-set:unicode-graphic
  (char-set-union
   char-set:unicode-alphanumeric
   char-set:unicode-punctuation
   char-set:unicode-symbol
   ))

(define char-set:unicode-whitespace
  (char-set-union
   ;; ???
   char-set:ascii-whitespace
   char-set:unicode-category-Zs
   char-set:unicode-category-Zl
   char-set:unicode-category-Zp
   ))

(define char-set:unicode-printing
  (char-set-union
   char-set:unicode-graphic
   char-set:unicode-whitespace
   ))

(define char-set:unicode-control
  (char-set-union
   char-set:unicode-category-Cc
   char-set:unicode-category-Cf
   char-set:unicode-category-Cs
   char-set:unicode-category-Co
   char-set:unicode-category-Cn
   ))

(define char-set:word-constituent
  (let ()
    (define char-set:ascii-word-constinuent
      (char-set-adjoin char-set:ascii-alphanumeric
                       #\_))
    (define char-set:ascii-word-constinuent/nocase
      (char-set-uncase char-set:ascii-word-constinuent))
    (define char-set:unicode-word-constinuent
      (char-set-adjoin char-set:unicode-alphanumeric
                       #\_))
    (define char-set:unicode-word-constinuent/nocase
      (char-set-uncase char-set:unicode-word-constinuent))
    (lambda (ascii? case?)
      (if ascii?
          (if case?
              char-set:ascii-word-constinuent
              char-set:ascii-word-constinuent/nocase)
          (if case?
              char-set:unicode-word-constinuent
              char-set:unicode-word-constinuent/nocase)))))

(define (named-cset-sre-parse s ascii? case?)
  (case s
    ((any)
     (if ascii?
         char-set:ascii
         char-set:full))
    ((nonl)
     char-set:nonl)
    ((ascii)
     char-set:ascii)
    ((lower-case lower)
     (if ascii?
         (if case?
             char-set:ascii-lower-case
             char-set:ascii-alphabetic)
         (if case?
             char-set:unicode-lower-case
             char-set:unicode-cased)))
    ((upper-case upper)
     (if ascii?
         (if case?
             char-set:ascii-upper-case
             char-set:ascii-alphabetic)
         (if case?
             char-set:unicode-upper-case
             char-set:unicode-cased)))
    ((title-case title)
     (if ascii?
         char-set:empty
         char-set:unicode-title-case))
    ((alphabetic alpha)
     (if ascii?
         char-set:ascii-alphabetic
         char-set:unicode-alphabetic))
    ((numeric num)
     (if ascii?
         char-set:ascii-numeric
         char-set:unicode-numeric))
    ((alphanumeric alphanum alnum)
     (if ascii?
         char-set:ascii-alphanumeric
         char-set:unicode-alphanumeric))
    ((punctuation punct)
     (if ascii?
         char-set:ascii-punctuation
         char-set:unicode-punctuation))
    ((symbol)
     (if ascii?
         char-set:ascii-symbol
         char-set:unicode-symbol))
    ((graph graphic)
     (if ascii?
         char-set:ascii-graphic
         char-set:unicode-graphic))
    ((whitespace white space)
     (if ascii?
         char-set:ascii-whitespace
         char-set:unicode-whitespace))
    ((printing print)
     (if ascii?
         char-set:ascii-printing
         char-set:unicode-printing))
    ((control cntrl)
     (if ascii?
         char-set:ascii-control
         char-set:unicode-control))
    ((hex-digit xdigit)
     char-set:hex-digit)
    (else
     (error "unknown named char-set:" s))
    ))
