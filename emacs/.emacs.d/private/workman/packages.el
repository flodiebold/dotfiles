

;; see also: https://github.com/fmdkdd/dotfiles/blob/master/spacemacs/.emacs.d/private/colemak-hjkl/packages.el

(setq workman-packages
      '(
        evil
        evil-org
        magit
        ))

(defun workman/pre-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config

    (define-key evil-normal-state-map "y" nil)
    (define-key evil-normal-state-map "o" nil)
    (define-key evil-normal-state-map "n" nil)
    (define-key evil-normal-state-map "e" nil)

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
    (define-key evil-normal-state-map "L" 'evil-open-below)
    (define-key evil-normal-state-map "j" 'evil-search-next)
    (define-key evil-normal-state-map "J" 'evil-search-previous)
    (define-key evil-normal-state-map "h" 'evil-yank)
    (define-key evil-normal-state-map "H" 'evil-yank-line)

    (define-key evil-visual-state-map "h" 'evil-yank)
    ))

(defun workman/pre-init-evil-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (evil-define-key 'normal evil-org-mode-map
      "o" nil)

    (evil-define-key 'normal evil-org-mode-map
      "gn" 'outline-down-heading
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

    (evil-define-key 'insert evil-org-mode-map
      (kbd "M-y") 'org-metaleft
      (kbd "M-e") 'org-metaup
      (kbd "M-n") 'org-metadown
      (kbd "M-o") 'org-metaright
      (kbd "M-Y") 'org-shiftmetaleft
      (kbd "M-E") 'org-shiftmetaup
      (kbd "M-O") 'org-shiftmetaright
      (kbd "M-N") 'org-shiftmetadown)
    ))

(defun workman/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (define-key magit-mode-map "y" nil)
    (define-key magit-mode-map "n" nil)
    (define-key magit-mode-map "e" nil)
    (define-key magit-mode-map "o" nil)
    (evil-define-key 'evil-magit-state magit-mode-map
      "y" nil
      "n" nil
      "e" nil
      "o" nil)
    (evil-define-key 'normal magit-mode-map
      "y" nil
      "n" nil
      "e" nil
      "o" nil)))
