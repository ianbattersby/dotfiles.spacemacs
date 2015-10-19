;;; extensions.el --- my-csharp Layer extensions File for Spacemacs
;;
;; Copyright (c) 2015 Ian Battersby
;;
;; Author: Ian Battersby <ian.battersby@gmail.com>
;; URL: https://github.com/ianbattersby/spacemacs.layers
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-csharp-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq my-csharp-post-extensions
      '(
        ;; post extension names go here
        omnisharp-emacs
        ))

;; For each extension, define a function my-csharp/init-<extension-name>
;;
;; (defun my-csharp/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun my-csharp/init-omnisharp-emacs ()
  "Initialize my extension"
  (use-package omnisharp
       :init
        (setq omnisharp-debug 't)
        (setq omnisharp-server-executable-path (expand-file-name omnisharp-load-script))
        (eval-after-load 'company
          '(push 'company-omnisharp company-backends-csharp-mode))
        (evil-leader/set-key
          "aO" 'omnisharp-start-omnisharp-server)
       :config
        (add-hook 'csharp-mode-hook 'omnisharp-mode)
        (add-hook 'csharp-mode-hook 'flycheck-mode)
        (add-hook 'csharp-mode-hook 'eldoc-mode)
        (evil-leader/set-key-for-mode 'csharp-mode
            ;; Compile
            "mcc" 'omnisharp-build-in-emacs ;; Only one compile command so use top-level
            ;; Solution/project manipulation
            "mfa" 'omnisharp-add-to-solution-current-file
            "mfA" 'omnisharp-add-to-solution-dired-selected-files
            "mfr" 'omnisharp-remove-from-project-current-file
            "mfR" 'omnisharp-remove-from-project-dired-selected-files
            "mpl" 'omnisharp-add-reference
            ;; Navigation
            "mgg" 'omnisharp-go-to-definition
            "mgG" 'omnisharp-go-to-definition-other-window
            "mgu" 'omnisharp-helm-find-usages
            "mgs" 'omnisharp-helm-find-symbols
            "mgi" 'omnisharp-find-implementations
            "mgr" 'omnisharp-navigate-to-region
            "mgm" 'omnisharp-navigate-to-solution-member
            "mgM" 'omnisharp-navigate-to-solution-member-other-window
            "mgf" 'omnisharp-navigate-to-solution-file
            "mgF" 'omnisharp-navigate-to-solution-file-then-file-member
            ;; Help, documentation, info
            "mht" 'omnisharp-current-type-information
            "mhT" 'omnisharp-current-type-information-to-kill-ring
            ;; Refactoring
            "mrm" 'omnisharp-rename-interactively
            "mrr" 'omnisharp-run-code-action-refactoring
            ;; Server manipulation, inspired spacemacs REPL bindings since C# does not provice a REPL
            "mss" 'omnisharp-start-omnisharp-server
            "msS" 'omnisharp-stop-server
            "msr" 'omnisharp-reload-solution
            ;; Tests
            "mta" 'omnisharp-unit-test-all
            "mtb" 'omnisharp-unit-test-fixture
            "mtt" 'omnisharp-unit-test-single
            ;; Code manipulation
            "mu" 'omnisharp-auto-complete-overrides
            "mi" 'omnisharp-fix-usings
            "m=" 'omnisharp-code-format
            "m." 'omnisharp-fix-code-issue-at-point)

            (define-key company-active-map (kbd ".") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '".")))
            (define-key company-active-map (kbd "]") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '"]")))
            (define-key company-active-map (kbd "[") (lambda() (interactive) (company-complete-selection-insert-key '"[")))
            (define-key company-active-map (kbd ")") (lambda() (interactive) (company-complete-selection-insert-key '")")))
            (define-key company-active-map (kbd "<SPC>") nil)
            (define-key company-active-map (kbd ";") (lambda() (interactive) (company-complete-selection-insert-key '";")))
            (define-key company-active-map (kbd ">") (lambda() (interactive) (company-complete-selection-insert-key '">")))
            (define-key omnisharp-mode-map (kbd "}") 'csharp-indent-function-on-closing-brace) 
            (define-key omnisharp-mode-map (kbd "<RET>") 'csharp-newline-and-indent) 

            (define-key omnisharp-mode-map (kbd "<f12>") 'omnisharp-go-to-definition)
            (define-key omnisharp-mode-map (kbd "s-d") 'omnisharp-go-to-definition)
            (define-key omnisharp-mode-map (kbd "S-s-<up>") 'omnisharp-navigate-up)
            (define-key omnisharp-mode-map (kbd "S-s-<down>") 'omnisharp-navigate-down)
            (define-key omnisharp-mode-map (kbd "S-<f12>") 'omnisharp-helm-find-usages)

            (define-key omnisharp-mode-map (kbd "s-u") 'omnisharp-helm-find-usages)
            (define-key omnisharp-mode-map (kbd "s-i") 'omnisharp-helm-find-implementations)
            (define-key omnisharp-mode-map (kbd "S-s-<f12>") 'omnisharp-helm-find-usages)
            (define-key omnisharp-mode-map (kbd "<M-RET>") 'omnisharp-run-code-action-refactoring)
            (define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-run-code-action-refactoring)

            (define-key omnisharp-mode-map (kbd "C-k C-d") 'omnisharp-code-format)
            (define-key omnisharp-mode-map (kbd "C-d") 'duplicate-current-line-or-region)

            (define-key omnisharp-mode-map (kbd "<f2>") 'omnisharp-rename-interactively)
            (define-key omnisharp-mode-map (kbd "<f5>") 'omnisharp-build-in-emacs)

            (setq omnisharp-auto-complete-want-documentation nil)
            (setq c-basic-offset 4) ; indents 4 chars
            (setq tab-width 4)          ; and 4 char wide for TAB
            (setq indent-tabs-mode nil) ; And force use of spaces
            (setq eldoc-idle-delay 0.1
                  flycheck-display-errors-delay 0)

            (yas-minor-mode)
            (company-mode)
            (linum-mode)))
            ;;(flycheck-mode))
            ;;(linum-mode)
            ;;(turn-on-eldoc-mode)))

;; Company mode stuff
(defun company-complete-selection-insert-key(company-key)
  (company-complete-selection)
  (insert company-key))

(defun company-complete-selection-insert-key-and-complete(company-key)
  (company-complete-selection-insert-key company-key)
  (company-complete))

(defun csharp-indent-function-on-closing-brace()
  (interactive)
  (insert "}")
  (c-indent-defun))

(defun csharp-newline-and-indent ()
  "Open a newline and indent.
If point is between a pair of braces, opens newlines to put braces
on their own line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (and
             (looking-at " *}")
             (save-match-data
               (when (looking-back "{ *")
                 (goto-char (match-beginning 0))
                 (unless (looking-back "^[[:space:]]*")
                   (newline-and-indent))
                 t)))
        (unless (and (boundp electric-pair-open-newline-between-pairs)
                     electric-pair-open-newline-between-pairs
                     electric-pair-mode)
          (goto-char (match-beginning 0))
          (newline-and-indent)))))
  (newline-and-indent))
