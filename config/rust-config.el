;; (use-package rust-mode
;;   :init
;;   (setq rust-format-on-save t))

;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))

;; (add-hook 'rust-mode-hook
;;           (lambda () (prettify-symbols-mode)))

(use-package rustic
  :init
  (setq rustic-lsp-client 'eglot)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer")))

(add-hook 'rust-mode-hook 'eglot-ensure)
