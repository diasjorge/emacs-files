(require 'flymake)
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
