

;; see also: https://github.com/fmdkdd/dotfiles/blob/master/spacemacs/.emacs.d/private/colemak-hjkl/packages.el

(setq workman-packages
      '(evil
        evil-org
        evil-lisp-state
        evil-cleverparens
        cargo-process
        magit
        evil-magit
        ranger))

(defun workman/post-init-evil ()
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

  (with-eval-after-load 'evil-iedit-state
    (define-key evil-iedit-state-map "o" nil)
    (define-key evil-iedit-state-map "O" nil))

  ;; (define-key compilation-mode-map "n" nil)
  )

(defun workman/post-init-cargo-process ()
  (define-key cargo-process-mode-map "n" nil))

(defun workman/pre-init-evil-org ()
  (spacemacs|use-package-add-hook evil-org
    :post-config
    (evil-define-key 'normal 'evil-org-mode
      "O" nil
      "o" nil)

    (evil-define-key 'normal 'evil-org-mode
      ;; "gn" 'outline-down-heading
      "ge" 'outline-up-heading
      "go" 'org-forward-heading-same-level
      "gy" 'org-backward-heading-same-level
      (kbd "M-y") 'org-metaleft
      (kbd "M-e") 'org-metaup
      (kbd "M-n") 'org-metadown
      (kbd "M-o") 'org-metaright
      (kbd "M-Y") 'org-shiftmetaleft
      (kbd "M-E") 'org-shiftmetaup
      (kbd "M-O") 'org-shiftmetaright
      (kbd "M-N") 'org-shiftmetadown)

    (evil-define-key 'insert 'evil-org-mode
      (kbd "M-y") 'org-metaleft
      (kbd "M-e") 'org-metaup
      (kbd "M-n") 'org-metadown
      (kbd "M-o") 'org-metaright
      (kbd "M-Y") 'org-shiftmetaleft
      (kbd "M-E") 'org-shiftmetaup
      (kbd "M-O") 'org-shiftmetaright
      (kbd "M-N") 'org-shiftmetadown)))

(defun workman/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (define-key magit-mode-map "y" nil)
    (define-key magit-mode-map "n" nil)
    (define-key magit-mode-map "e" nil)
    (define-key magit-mode-map "o" nil)
    (define-key magit-blob-mode-map "y" nil)
    (define-key magit-blob-mode-map "n" nil)
    (define-key magit-blob-mode-map "e" nil)
    (define-key magit-blob-mode-map "o" nil)
    (define-key magit-blame-read-only-mode-map "n" nil)))

(defun workman/pre-init-evil-magit ()
  (with-eval-after-load 'evil-magit
    (evil-define-key 'evil-magit-state magit-mode-map
      "y" nil
      "n" nil
      "e" nil
      "o" nil)
    (evil-define-key 'normal magit-mode-map
      "y" nil
      "n" nil
      "e" nil
      "o" nil)
    (evil-define-key 'visual magit-mode-map
      "y" nil
      "n" nil
      "e" nil
      "o" nil)))

(defun workman/sp-transpose-backwards (&optional arg)
  ""
  (interactive "p")
  (sp-transpose-sexp (- arg)))

(defun workman/pre-init-evil-lisp-state ()
  (spacemacs|use-package-add-hook evil-lisp-state
    :post-config
    (define-key evil-lisp-state-map "y" 'sp-backward-symbol)
    (define-key evil-lisp-state-map "Y" 'sp-backward-sexp)
    (define-key evil-lisp-state-map "o" 'sp-forward-symbol)
    (define-key evil-lisp-state-map "O" 'sp-forward-sexp)

    (define-key evil-lisp-state-map "n" 'lisp-state-next-closing-paren)
    (define-key evil-lisp-state-map "e" 'lisp-state-prev-opening-paren)

    (define-key evil-lisp-state-map "h" 'sp-copy-sexp)

    (define-key evil-lisp-state-map "l" 'sp-splice-sexp-killing-forward)
    (define-key evil-lisp-state-map "L" 'sp-splice-sexp-killing-backward)

    (define-key evil-lisp-state-map "T" 'workman/sp-transpose-backwards)))

(defun workman/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :post-init
    (autoload 'evil-cp-backward-symbol-begin "evil-cleverparens")
    (autoload 'evil-cp-forward-symbol-end "evil-cleverparens")
    (define-key evil-motion-state-map "Y" 'evil-cp-backward-symbol-begin)
    (define-key evil-motion-state-map "O" 'evil-cp-forward-symbol-end)
    ;; :post-config doesn't work, https://github.com/jwiegley/use-package/issues/688
    (with-eval-after-load 'evil-cleverparens
      (evil-define-key 'normal evil-cleverparens-mode-map
        "y" nil
        "n" nil
        "e" nil
        "o" nil
        "Y" nil
        "N" nil
        "E" nil
        "O" nil
        "l" nil
        "L" nil
        (kbd "M-o") nil
        (kbd "M-O") nil)

      (evil-define-key 'visual evil-cleverparens-mode-map
        "y" nil
        "Y" nil)
      (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-n") 'evil-cp-drag-forward)
      (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-e") 'evil-cp-drag-backward)

      (evil-define-key 'normal evil-cleverparens-mode-map "h" 'evil-cp-yank)
      (evil-define-key 'normal evil-cleverparens-mode-map "H" 'evil-cp-yank-line)
      (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-l") 'evil-cp-open-below-form)
      (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-L") 'evil-cp-open-above-form))))

(defun workman/pre-init-ranger ()
  (spacemacs|use-package-add-hook ranger
    :post-config
    (define-key ranger-mode-map "n" 'ranger-next-file)
    (define-key ranger-mode-map "e" 'ranger-prev-file)
    (define-key ranger-mode-map "o" 'ranger-find-file)
    (define-key ranger-mode-map "y" 'ranger-up-directory)
    (define-key ranger-mode-map "h" nil)
    (define-key ranger-mode-map "hh" 'ranger-copy)))
