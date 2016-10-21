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
(define-module srfi-115
  (use srfi-1)
  (use srfi-13)
  (use srfi-115.sre)
  (use gauche.record)
  (export regexp-matches regexp-matches? regexp-search
          regexp-fold regexp-extract regexp-split regexp-patition
          (rename srfi:regexp-replace regexp-replace)
          (rename srfi:regexp-replace-all regexp-replace-all)
          regexp-match? regexp-match-count
          regexp-match-submatch
          regexp-match-submatch-start regexp-match-submatch-end
          regexp-match->list
          ;; from srfi-115.sre
          regexp rx regexp->sre char-set->sre
          valid-sre? regexp?
          ))

(select-module srfi-115)

(define-record-type regexp-match
    rxmatch->regexp-match
    regexp-match?
  (rxmatch %regexp-match-rxmatch)
  (offset %regexp-match-offset))

(define (regexp-match-count regexp-match)
  (- (rxmatch-num-matches (%regexp-match-rxmatch regexp-match))
     1))

(define (regexp-match-submatch regexp-match field)
  (rxmatch-substring (%regexp-match-rxmatch regexp-match) field))

(define (regexp-match-submatch-start regexp-match field)
  (+ (%regexp-match-offset regexp-match)
     (rxmatch-start (%regexp-match-rxmatch regexp-match) field)))

(define (regexp-match-submatch-end regexp-match field)
  (+ (%regexp-match-offset regexp-match)
     (rxmatch-end (%regexp-match-rxmatch regexp-match) field)))

(define (regexp-match->list regexp-match)
  (rxmatch-substrings (%regexp-match-rxmatch regexp-match)))

(define (regexp-matches re str :optional (start 0) (end (string-length str)))
  (and-let* ((s (string-copy str start end))
             (m ((regexp re) s))
             ((= (rxmatch-start m 0) 0))
             ((= (rxmatch-end m 0) (- end start))))
    (rxmatch->regexp-match m start)))

(define (regexp-matches? re str :optional (start 0) (end (string-length str)))
  (boolean (regexp-matches? re str start end)))

(define (regexp-search re str :optional (start 0) (end (string-length str)))
  (and-let* ((s (string-copy str start end))
             (m ((regexp re) s)))
    (rxmatch->regexp-match m start)))

(define (%regexp-fold:default-finish _i _md _str acc)
  acc)

(define (string-drop* str n)
  (if (< n (string-length str))
      (string-drop str n)
      ""))

(define (regexp-fold re kons knil str
                     :optional
                     (finish %regexp-fold:default-finish)
                     (start 0)
                     (end (string-length str)))
  (let ((re (regexp re)))
    (let loop ((i start)                ; index of end of the last match
               (offset start)           ; start index of `s` in `str`
               (s (string-copy str start end)) ; substring to match
               (acc knil))              ; accumlator
      (cond ((and (< offset end) (re s))
             => (lambda (m)
                  (let ((e (rxmatch-end m 0))
                        (acc~ (kons i
                                    (rxmatch->regexp-match m offset)
                                    str
                                    acc)))
                    (if (zero? e)
                        ;; `re` matched to an empty string.
                        ;; move forward by one char
                        (loop (+ offset e)
                              (+ offset 1)
                              (string-drop* s 1)
                              acc~)
                        (loop (+ offset e)
                              (+ offset e)
                              (rxmatch-after m)
                              acc~)))))
            (else
             (finish i #f str acc))))))

(define (regexp-extract re str :optional (start 0) (end (string-length str)))
  (define (kons x xs)
    (if (string-null? x)
        xs
        (cons x xs)))
  (regexp-fold re
               (lambda (i md str acc)
                 (kons (regexp-match-submatch md 0) acc))
               '()
               str
               (lambda (i md str acc)
                 (reverse! acc))
               start
               end))

(define (regexp-split re str :optional (start 0) (end (string-length str)))
  (regexp-fold re
               (lambda (i md str acc)
                 (let ((idx (regexp-match-submatch-start md 0)))
                   (if (< i idx)
                       (cons (string-copy str i idx) acc)
                       acc)))
               '()
               str
               (lambda (i md str acc)
                 (reverse!
                  (if (< i end)
                      (cons (string-copy str i end) acc)
                      acc)))))

(define (regexp-patition re str :optional (start 0) (end (string-length str)))
  (regexp-fold re
               (lambda (i md str acc)
                 (cons* (regexp-match-submatch md 0)
                        (string-copy str i (regexp-match-submatch-start md 0))
                        acc))
               '()
               str
               (lambda (i md str acc)
                 (reverse!
                  (if (or (< i end) (null? acc))
                      (cons (string-copy str i end) acc)
                      acc)))
               start
               end))

(define (%split-string str start end)
  (let ((len (string-length str)))
    (values (string-copy str 0 start)
            (string-copy str start end)
            (string-drop* str end))))

(define (%regexp-replace-subst m subst)
  (cond ((eq? subst 'pre)
         (rxmatch-before m))
        ((eq? subst 'post)
         (rxmatch-after m))
        ((or (symbol? subst)
             (integer? subst))
         (rxmatch-substring m subst))
        ((string? subst)
         subst)
        (else
         (error "symbol, integer, or string required, but got:" subst))))

(define (srfi:regexp-replace re str subst
                             :optional (start 0) (end #f) (count 0))
  (let ((re (regexp re))
        (end (or end (string-length str))))
    (receive (pre s post) (%split-string str start end)
      (with-output-to-string
        (lambda ()
          (display pre)
          (let loop ((s s)
                     (count count))
            (cond ((re s)
                   => (lambda (m)
                        (display (rxmatch-before m))
                        (cond ((zero? count)
                               (display (%regexp-replace-subst m subst))
                               (display (rxmatch-after m)))
                              ((= (rxmatch-start m)
                                  (rxmatch-end m))
                               (let ((s~ (rxmatch-after m)))
                                 (unless (string-null? s~)
                                   (display (string-ref s~ 0)))
                                 (loop (string-drop* s~ 1)
                                       (- count 1))))
                              (else
                               (display (rxmatch-substring m 0))
                               (loop (rxmatch-after m)
                                     (- count 1))))))
                  (else
                   (display s))))
          (display post))))))

(define (srfi:regexp-replace-all re str subst
                                 :optional (start 0) (end #f))
  (let ((re (regexp re))
        (end (or end (string-length str))))
    (receive (pre s post) (%split-string str start end)
      (with-output-to-string
        (lambda ()
          (display pre)
          (regexp-fold re
                       (lambda (i md str acc)
                         (let ((m (%regexp-match-rxmatch md)))
                           (display (rxmatch-before m))
                           (display (%regexp-replace-subst m subst))
                           #f))
                       #f
                       s
                       (lambda (i md str acc)
                         (when (< i end)
                           (display
                            (string-copy str i end)))))
          (display post))))))
