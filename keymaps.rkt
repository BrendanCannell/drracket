#lang s-exp framework/keybinding-lang

(require srfi/2)
(require (for-syntax racket/list))

;; The raw version of define-shortcut that does not perform any
;; key processing or body wrapping.
(define-syntax-rule (define-shortcut-internal (key ...) name proc)
  (begin
    (define (name ed evt . rest)
      (when (is-a? ed racket:text<%>)
        (send ed begin-edit-sequence)
        (apply proc ed evt rest)
        (send ed end-edit-sequence)))
    (keybinding key name) ...))
 
(define-syntax (define-shortcut stx)
  ;; Add esc; equivalent key bindings for all the meta bindings.
  (define (add-esc-key-bindings s-keys)
    (define keys (syntax->datum s-keys))
    (define esc-variants
      (for/list ([k (in-list keys)]
                 #:when (regexp-match? #rx"m:" k))
                (string-append "esc;" (regexp-replace* #rx"m:" k ""))))
    ;; Use remove-duplicates to combine all key bindings, so that duplicates
    ;; are removed. This means that if we add some esc; key bindings manually,
    ;; for example by accident, it will not be duplicated, affecting display
    ;; of key bindings in DrRacket.
    (remove-duplicates (append esc-variants keys)))
  (syntax-case stx ()
    [(_ key (name . args) body* ...)
     #'(define-shortcut key name
         (λ args body* ...))]
    [(_ (key ...) name proc)
     #`(define-shortcut-internal
         (#,@(add-esc-key-bindings #'(key ...)))
         name proc)]
    [(_ key name proc)
     #'(define-shortcut (key) name proc)]))

(define (get-start/end-position ed)
  (let* ([start-box (box #f)]
         [end-box (box #f)]
         [_ (send ed get-position start-box end-box)])
    (values (unbox start-box) (unbox end-box))))

(define (get-start-position/selection? ed)
  (let-values ([(start end) (get-start/end-position ed)])
    (values start (not (= 0 (- start end))))))

(define (get-end-position/selection? ed)
  (let-values ([(start end) (get-start/end-position ed)])
    (values end (not (= 0 (- start end))))))

;;; Movement

(define (get-paredit-forward-sexp ed sp)
  (cond
    [(send ed get-forward-sexp sp)
     => (λ (pos) pos)]
    [(send ed find-up-sexp sp)
     => (λ (pos)
          (send ed get-forward-sexp pos))]
    [else #f]))

(define-shortcut ("a:d:l") (paredit-forward-sexp ed evt)
  (let-values ([(end selection?) (get-end-position/selection? ed)])
    (if selection?
        (send ed set-position end)
        (and-let* ([dest (get-paredit-forward-sexp ed end)])
            (send ed set-position dest)))))

(define (get-paredit-backward-sexp ed sp)
  (cond
    [(send ed get-backward-sexp sp)
     => (λ (pos) pos)]
    [else (send ed find-up-sexp sp)]))

(define-shortcut ("a:d:j") (paredit-backward-sexp ed evt)
  (let-values ([(start selection?) (get-start-position/selection? ed)])
    (if selection?
        (send ed set-position start)
        (and-let* ([dest (get-paredit-backward-sexp ed start)])
            (send ed set-position dest)))))

(define-shortcut ("a:d:k") (paredit-down-sexp ed evt)
  (send ed down-sexp
        (send ed get-start-position)))

(define (get-forward-atom ed pos)
  (define sp (send ed get-start-position))
  (define dests 
    (filter (λ (x) x)
            (list (send ed find-down-sexp sp)
                  (get-paredit-forward-sexp ed sp))))
  (and (not (null? dests)) (apply min dests)))

(define-shortcut ("a:l") (forward-atom ed evt)
  (let-values ([(end selection?) (get-end-position/selection? ed)])
    (if selection?
        (send ed set-position end)
        (and-let* ([dest (get-forward-atom ed end)])
                  (send ed set-position dest)))))

(define (find-down-sexp-backward ed pos)
  (and-let* ([bw (send ed get-backward-sexp pos)]
             [down (send ed find-down-sexp bw)])
            (if (or (not down) (> down pos))
                #f
                (last-sexp ed down))))

(define (get-backward-atom ed pos)
  (define sp (send ed get-start-position))
  (define dests 
    (filter (λ (x) x)
            (list (find-down-sexp-backward ed sp)
                  (get-paredit-backward-sexp ed sp))))
  (and (not (null? dests)) (apply max dests)))

(define-shortcut ("a:j") (backward-atom ed evt)
  (let-values ([(start selection?) (get-start-position/selection? ed)])
    (if selection?
        (send ed set-position start)
        (and-let* ([dest (get-backward-atom ed start)])
                  (send ed set-position dest)))))

;;; Depth-Changing
(define-shortcut ("a:(") (paredit-splice-sexp ed evt [pos #f] [reindent #t])
  (when (not pos)
    (set! pos (send ed get-start-position)))
  (and-let* ([begin-outer (send ed find-up-sexp pos)]
             [end-outer (send ed get-forward-sexp begin-outer)])
            (send ed delete (- end-outer 1) end-outer)
            (send ed delete begin-outer (+ begin-outer 1))
            (when reindent
              (send ed tabify-selection begin-outer end-outer))))

(define (start-of-sexp ed pos)
  (define fw (send ed get-forward-sexp pos))
  (cond [(if fw
             (send ed get-backward-sexp fw)
             (send ed get-backward-sexp pos))
         => (λ (v) v)]
        [else pos]))

(define (sexp-start ed)
  (start-of-sexp ed (send ed get-start-position)))

(define-shortcut ("d:s:9") (paredit-wrap-round ed evt)
  (send ed insert "(")
  (let ([pos (send ed get-start-position)])
    (send ed forward-sexp pos)
    (send ed insert ")")
    (send ed set-position pos)))

(define (first-sexp ed sp)
  (let loop ([pos sp] [prev sp])
    (if pos
        (loop (send ed get-backward-sexp pos) pos)
        prev)))

(define (last-sexp ed sp)
  (let loop ([pos sp] [prev sp])
    (if pos
        (loop (send ed get-forward-sexp pos) pos)
        prev)))

(define (kill-sexps-backward ed pos)
  (send ed delete (first-sexp ed pos) pos))

(define (kill-sexps-forward ed pos)
  (send ed delete pos (last-sexp ed pos)))

(define (not-toplevel? ed pos)
  (send ed find-up-sexp pos))

(define-shortcut ("c:a:d:up") #;("m:up") (paredit-splice-sexp-killing-backward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("c:a:d:down") #;("m:down") (paredit-splice-sexp-killing-forward ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (kill-sexps-forward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("c:s:9") (paredit-raise-sexp ed evt)
  (define sp (sexp-start ed))
  (when (not-toplevel? ed sp)
    (and-let* ([fw (send ed get-forward-sexp sp)])
              (kill-sexps-forward ed fw))
    (kill-sexps-backward ed sp)
    (paredit-splice-sexp ed evt)))

(define-shortcut ("d:s:/") (paredit-convolute-sexp ed evt)
  (define sp (sexp-start ed))
  (and-let* ([r1 (send ed find-up-sexp sp)]
             [fw (send ed get-forward-sexp r1)]
             [paren (send ed get-text (- fw 1) fw)]
             [r2 (send ed find-up-sexp r1)]
             [text (send ed get-text r1 sp)]
             [end (send ed get-forward-sexp r2)])
            (send ed insert paren end)
            (kill-sexps-backward ed sp)
            (paredit-splice-sexp ed evt (+ r1 1) #f)
            (send ed insert text r2)
            (send ed tabify-selection r2 end)))


;;;Barfage & Slurpage

(define (find-up-sexp-slurp-forward ed sp)
  (let loop ([sp (send ed find-up-sexp sp)])
    (cond [(not sp) #f]
          [(and-let* ([fw1 (send ed get-forward-sexp sp)])
                     (send ed get-forward-sexp fw1)) sp]
          [else (loop (send ed find-up-sexp sp))])))

(define-shortcut ("a:]") #;("c:right" "c:s:0" "c:]") (paredit-slurp-forward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([up (find-up-sexp-slurp-forward ed sp)]
             [end (send ed get-forward-sexp up)]
             [fw (send ed get-forward-sexp end)]
             [paren (send ed get-text (- end 1) end)])
            (send ed insert paren fw)
            (send ed delete end)
            (send ed tabify-selection up fw)))

(define (find-up-sexp-slurp-backward ed sp)
  (let loop ([sp (send ed find-up-sexp sp)])
    (cond [(not sp) #f]
          [(send ed get-backward-sexp sp) sp]
          [else (loop (send ed find-up-sexp sp))])))

(define-shortcut ("a:[") #;("c:m:left" "c:s:9" "c:[") (paredit-slurp-backward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([start (find-up-sexp-slurp-backward ed sp)]
             [bw (send ed get-backward-sexp start)]
             [paren (send ed get-text start (+ start 1))])
            (send ed delete (+ start 1))
            (send ed insert paren bw)
            (send ed tabify-selection bw start)))

(define-shortcut ("a:}") #;("c:left" "c:}") (paredit-barf-forward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([up (send ed find-up-sexp sp)]
             [fw (send ed get-forward-sexp up)]
             [paren (send ed get-text (- fw 1) fw)]
             [last (last-sexp ed sp)]
             [bw (send ed get-backward-sexp last)])
            (and-let* ([bw1 (send ed get-backward-sexp bw)]
                       [x (send ed get-forward-sexp bw1)])
                      (set! bw x))
            (send ed delete fw)
            (send ed insert paren bw)
            (send ed set-position sp)
            (send ed tabify-selection up fw)))

(define-shortcut ("a:{") #;("c:m:right" "c:{") (paredit-barf-backward ed evt)
  (define sp (send ed get-start-position))
  (and-let* ([up (send ed find-up-sexp sp)]
             [paren (send ed get-text up (+ up 1))]
             [down (send ed find-down-sexp up)]
             [fw (send ed get-forward-sexp down)])
            (and-let* ([fw1 (send ed get-forward-sexp fw)]
                       [x (send ed get-backward-sexp fw1)])
                      (set! fw x))
            (send ed insert paren fw)
            (send ed delete (+ up 1))
            (send ed tabify-selection up fw)))

(define-shortcut ("backspace") (smart-delete ed evt)
  (let-values ([(start selection?) (get-start-position/selection? ed)])
    (cond
      [selection? (send ed delete)]
      [(= 0 start) (void)]
      [(and (equal? #\( (send ed get-character (- start 1)))
            (send ed find-up-sexp start))
       => (λ (pos) (send ed remove-parens-forward (- start 1)))]
      [(and (equal? "\"\"" (send ed get-text (- start 1) (+ start 1))))
       (send ed delete (- start 1) (+ start 1))]
      [else (send ed delete)])))

;;; END PAREDIT

(define (rebind key command)
  (keybinding
   key
   (λ (ed evt)
     (send (send ed get-keymap) call-function
           command ed evt #t))))

(define (bind-insert key str)
  (keybinding key (λ (editor evt) (send editor insert str))))

;; Navigation...

;; ...by character

(rebind "d:j" "backward-character")
(rebind "d:l" "forward-character")
(rebind "d:i" "previous-line")
(rebind "d:k" "next-line")

(rebind "d:s:j" "backward-select")
(rebind "d:s:l" "forward-select")
(rebind "d:s:i" "select-up")
(rebind "d:s:k" "select-down")

;; ...by atom

;       "a:j" "backward-atom"
;       "a:l" "forward-atom"
(rebind "a:i" "previous-line")
(rebind "a:k" "next-line")

;; These four and the "select-*-sexp" four are constructed to work around a bug
;; in (I think) DrRacket. The first four should be mapped to "a:s:<key>" and the
;; others should be mapped to "a:d:s:<key>", but for some reason that causes the
;; second set of key chords to be interpreted as the corresponding chords in the
;; first set. The given construction is able to provide the intended interface.

(rebind "a:~d:s:j" "backward-select") ;; TODO backward/forward-select-atom
(rebind "a:~d:s:l" "forward-select")
(rebind "a:~d:s:i" "select-up")
(rebind "a:~d:s:k" "select-down")

;; ...by sexp

;       "a:d:j" "paredit-backward-sexp"
;       "a:d:l" "paredit-forward-sexp"
(rebind "a:d:i" "up-sexp")
;       "a:d:k" "paredit-down-sexp"

(rebind "a:s:j" "select-backward-sexp")
(rebind "a:s:l" "select-forward-sexp")
(rebind "a:s:i" "select-up-sexp")
(rebind "a:s:k" "select-down-sexp")

;; ...by large span

(rebind "c:j" "beginning-of-line")
(rebind "c:l" "end-of-line")
(rebind "c:i" "previous-page")
(rebind "c:k" "next-page")

(rebind "c:s:j" "select-to-beginning-of-line")
(rebind "c:s:l" "select-to-end-of-line")
(rebind "c:s:i" "select-page-up")
(rebind "c:s:k" "select-page-down")

;; ...of/between tabs

(rebind "a:d:left" "prev-tab")
(rebind "a:d:right" "next-tab")
(rebind "c:tab" "next-tab")
(rebind "c:s:tab" "prev-tab")
(rebind "a:d:s:left" "move-current-tab-left")
(rebind "a:d:s:right" "move-current-tab-right")

(rebind "a:r" "Rename Identifier")

(rebind "d:semicolon" "comment-out")
(rebind "s:d:semicolon" "uncomment")

(rebind "a:backspace" "delete-previous-character")

(bind-insert "c:`" "¬")
(bind-insert "c:4" "∀")
(bind-insert "c:3" "∃")
(bind-insert "c:7" "∧")
(bind-insert "c:8" "∨")
(bind-insert "c:b" "β")

(define (debug-dialog msg)
  (message-box "Debug" msg))