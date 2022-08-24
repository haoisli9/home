
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tree-sitter-debug)
(require 'tree-sitter-query)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;; (add-hook 'prog-mode-hook #'tree-sitter-hl-mode)

;; 1.
;; git clone https://github.com/Wilfred/tree-sitter-elisp
;; git clone https://github.com/ikatyang/tree-sitter-vue.git
;; git clone https://github.com/tree-sitter/tree-sitter-typescript.git
;; 2. gcc ./src/parser.c -fPIC -I./ --shared -o elisp.so
;; gcc ./src/parser.c -fPIC -I./ -I./src/ --shared -o elisp.dll
;; gcc ./src/parser.c ./src/scanner.cc -lstdc++ -fPIC -I./ -I./src --shared -o vue.dll
;; gcc ./tsx/src/parser.c ./tsx/src/scanner.c -fPIC -I./ -I./tsx/src/ --shared -o typescript.dll 
;; 3. cp ./elisp.so ~/.tree-sitter-langs/bin

;; The grammar itself is in grammar.js. You'll need to regenerate the code after editing the grammar.
;; $ npm run generate

;; (tree-sitter-load 'elisp "elisp")
;; (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
;; (tree-sitter-load 'vue "vue")
;; (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))

(provide 'init-treesitter)
