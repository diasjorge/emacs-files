(add-hook 'term-mode-hook
          '(lambda () (autopair-mode -1)))

(unless autopair-global-mode
  (autopair-global-mode))
