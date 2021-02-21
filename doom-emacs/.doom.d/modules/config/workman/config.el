
(define-key evil-normal-state-map "y" nil)
(define-key evil-normal-state-map "o" nil)
(define-key evil-normal-state-map "n" nil)
(define-key evil-normal-state-map "e" nil)
(define-key evil-normal-state-map "Y" nil)
(define-key evil-normal-state-map "O" nil)

(define-key evil-motion-state-map "y" 'evil-backward-char)
(define-key evil-motion-state-map "o" 'evil-forward-char)
(define-key evil-motion-state-map "n" 'evil-next-line)
(define-key evil-motion-state-map "e" 'evil-previous-line)

(autoload #'evil-cp-backward-symbol-begin "evil-cleverparens")
(autoload #'evil-cp-forward-symbol-end "evil-cleverparens")
(define-key evil-motion-state-map "Y" 'evil-cp-backward-symbol-begin)
(define-key evil-motion-state-map "O" 'evil-cp-forward-symbol-end)

(define-key evil-motion-state-map "gn" 'evil-next-visual-line)
(define-key evil-motion-state-map "ge" 'evil-previous-visual-line)
(define-key evil-motion-state-map "gk" nil)
(define-key evil-motion-state-map "gj" nil)

(define-key evil-motion-state-map "E" 'evil-window-top)
(define-key evil-motion-state-map "N" 'evil-join)

(define-key evil-normal-state-map "l" 'evil-open-below)
(define-key evil-normal-state-map "L" 'evil-open-above)
(define-key evil-normal-state-map "j" 'evil-ex-search-next)
(define-key evil-normal-state-map "J" 'evil-ex-search-previous)
(define-key evil-normal-state-map "h" 'evil-yank)
(define-key evil-normal-state-map "H" 'evil-yank-line)

(define-key evil-visual-state-map "h" 'evil-yank)
(define-key evil-visual-state-map "o" nil)
(define-key evil-visual-state-map "O" nil)
(define-key evil-visual-state-map "l" 'exchange-point-and-mark)

(after! magit
  (evil-define-key '(normal visual) magit-mode-map
    "y" nil
    "n" nil
    "e" nil
    "o" nil)
  (define-key magit-mode-map "y" nil)
  (define-key magit-mode-map "n" nil)
  (define-key magit-mode-map "e" nil)
  (define-key magit-mode-map "o" nil)
  (define-key magit-blob-mode-map "y" nil)
  (define-key magit-blob-mode-map "n" nil)
  (define-key magit-blob-mode-map "e" nil)
  (define-key magit-blob-mode-map "o" nil)
  (define-key magit-blame-read-only-mode-map "n" nil))

(after! evil-org
  (evil-define-key '(normal motion) evil-org-mode-map
      "O" nil
      "o" nil))

;; (evil-define-key 'evil-magit-state magit-mode-map
;;   "y" nil
;;   "n" nil
;;   "e" nil
;;   "o" nil)
;; (evil-define-key 'normal magit-mode-map
;;   "y" nil
;;   "n" nil
;;   "e" nil
;;   "o" nil)
;; (evil-define-key 'visual magit-mode-map
;;   "y" nil
;;   "n" nil
;;   "e" nil
;;   "o" nil)
