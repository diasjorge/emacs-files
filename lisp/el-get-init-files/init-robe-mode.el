(add-hook 'robe-mode-hook 'ac-robe-setup)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(add-hook 'robe-mode-hook '(lambda ()
                             (local-set-key (kbd "M-.") 'robe-jump)
                             (local-set-key (kbd "M->") 'robe-doc)
                             ))
