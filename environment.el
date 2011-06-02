(setq mac-command-modifier 'meta)
(set-default-font "Monaco-14")
(menu-bar-mode (global-set-key [kp-delete] 'delete-char))

(let ((xing-root (expand-file-name "~/development/xing/code/")))
  (progn
    (setenv "PATH"
            (concat
             xing-root "bin" ":"
             xing-root "gems/bin" ":"
             (getenv "PATH") ) )
    (setenv "GEM_HOME"
            (concat
             xing-root "gems") )
    (setenv "GEM_PATH"
            (concat
             xing-root "gems" ":"
             (getenv "GEM_PATH") ) )
    )
)