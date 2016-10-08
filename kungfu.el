(defvar home-path (replace-regexp-in-string "/\\(\\w+\\)/\\(\\w+\\)/\\(.*\\)" "/\\1/\\2" (getenv "PWD")) )

;;;; for test ;;; 
(defun my-fixture (body)
  (unwind-protect
      (progn [set up]
             (funcall body))
    [tear down]))
;;;;

;;  (get-mark-content (current-buffer))
(defun get-mark-content (buffername)
  (with-current-buffer buffername  (buffer-substring-no-properties (region-beginning) (region-end)) )
  )

(defun downcase-str ()
  (let ((str (get-mark-content (current-buffer)) ))
    (downcase str)))

;; Eval: (rb-underscore "AdddTaaaGaaa") => "addd_taaa_gaaa"
(defun rb-underscore (str)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/rb-underscore.drb " str)))
    (shell-command-to-string cmd-str)
    )
  )
(defun rb-understr ()
  (interactive)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/rb-underscore.drb " (get-point-keyword) )))
    (progn
      (kill-region (region-beginning) (region-end))
      (insert (shell-command-to-string cmd-str)))
    )
  )

(define-key global-map (kbd "C-c i") 'rb-understr)

(defun rb-camelize (str)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/rb-camelize.drb " str)))
    (shell-command-to-string cmd-str)
    )
  )

(defun ruby-parser (str)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/ruby_parser.drb " "\"" str "\"")))
    (shell-command-to-string cmd-str)
    )
  )

(defun ruby-parser-mark ()
  (interactive)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/ruby_parser.drb " "\"" (get-mark-content (current-buffer))  "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )

(define-key global-map (kbd "C-c l") 'ruby-parser-mark)

;; Usage: (get-api-to-doc "http://127.0.0.1:3000/api/dasddsa")
(defun get-api-to-doc (url)
  (let ((url-cmd   (concat " drb " home-path "/clojure_emacs/drb-help/http.drb " url)))
    (shell-command-to-string url-cmd)
    )
  )


(add-to-list 'load-path (concat home-path "/clojure_emacs/apib-mode") )
(autoload 'apib-mode "apib-mode"
  "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;;;;;;;;;;
(server-force-delete)
(server-start)



(defun get-mark-content-for-bash (buffername)
  (replace-regexp-in-string "`" "'" (get-mark-content buffername) )
  )

;;; 就像zshrc一样使用Elisp: alias v=' vi ~/.zshrc ; echo "Source zshrc ... "; source ~/.zshrc  ' 
(defun source ()
  (interactive)
  (load-file (concat home-path "/clojure_emacs/init.el") )
  )

;; Eval: (http-send-apiary "users" "post")
(defun http-send-apiary (url http-method)
  (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/http_send_for_apiary.drb " "http://localhost:3000/api/" url "  \" " (get-mark-content-for-bash (current-buffer)) "\" " http-method ) ) )

;; 只是单个参数,可以不用Mark,而获取当前行的内容
(defun http-one-params (url http-method)
  (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/http_send_for_apiary_one.drb " "http://localhost:3000/api/" url "  \" " (get-mark-content-for-bash (current-buffer)) "\" " http-method ) ) )

(defun http-send-apiary-params (url http-method)
  (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/http_send_for_apiary_params.drb " "http://localhost:3000/api/" url "  \" " (get-mark-content-for-bash (current-buffer)) "\" " http-method ) ) )

;; (inf-ruby-switch-setup) + ` C-x C-q `
(defun ininf ()
  (inf-ruby-switch-setup)
  (inf-ruby-maybe-switch-to-compilation)
  )

;; http://docs.huihoo.com/homepage/shredderyin/emacs_elisp.html ==> (read-char)
;; `C-c a `=> Mark 向下一个的光标, 或者不要Mark的向下的一个字符
;; "aaaaaaaaaaaaaaaaaaaaa"
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(define-key global-map (kbd "C-c t") 'wy-go-to-char)

;;; Emacs查看Ruby的函数定义跳转: Mark "obj.method" => rb-source
(defun rb-source ()
  (interactive)
  (let ((obj-method (get-mark-content (current-buffer))))
    (message (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/rb_source.drb " obj-method)) )
    )
  )

(defvar rb-obj-root nil)
(defvar rb-method-root nil)
;; Mark "instace.method" => "C-c g"
(defun rb-source-find ()
  (interactive)
  (let ((obj-method (get-mark-content (current-buffer))))
    (let ((file-and-line (second
			  (read (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/rb_source.drb " obj-method)) )
			  )) )
      (let ((rb-file (first file-and-line))
	    (rb-line (first (last file-and-line))) )
	(let ((rb-buffer (first (last (split-string rb-file "/"))))
              (rb-obj (first (split-string obj-method "\\.")) )
              )
	  (progn
	    (find-file rb-file)
	    (goto-line rb-line rb-buffer)
            (setq rb-obj-root rb-obj)
	    (message (concat "cool, open the file: " rb-file " , " (number-to-string rb-line) " , " rb-obj))
	    )
	  )
	)
      )
    )
  )

(define-key global-map (kbd "C-c g") 'rb-source-find)

					; Mark "instace_method" => "C-c n"
(defun rb-source-find-next ()
  (interactive) ;;;; only diff in obj-method
  (let ((obj-method   (concat rb-obj-root "." (get-mark-content (current-buffer)) )  ))
    (let ((file-and-line (second
			  (read (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/rb_source.drb " obj-method)) )
			  )) )
;;;;;;;;;;;;;
      (if (first file-and-line) 

	  (let ((rb-file (first file-and-line))
		(rb-line (first (last file-and-line))) )
	    (let ((rb-buffer (first (last (split-string rb-file "/"))))
		  (rb-obj (first (split-string obj-method "\\.")) )
		  )
	      (progn
		(find-file rb-file)
		(goto-line rb-line rb-buffer)
		(setq rb-obj-root rb-obj)
		(message (concat "cool, open the file: " rb-file " , " (number-to-string rb-line) " , " rb-obj))
		)
	      )
	    )

	(progn
	  (setq rb-method-root (first (last file-and-line)) )
	  (message (concat "please go to rb-method-root : " rb-method-root " , Run : C-c b"))
	  )
	)
;;;;;;;;;;;;;;;;;;;;;;;;
      )
    )
  )

(define-key global-map (kbd "C-c n") 'rb-source-find-next)

;;;;;;;;;;;;;;;;;;
;;; No need Mark any, use (buffer-file-name) as Class name, rb-method-root as method name => ` C-c b `
(defun rb-source-find-next-super ()
  (interactive) ;;;; only diff in obj-method
  (let ((obj-method   (concat
		       (rb-camelize (first (split-string (first (last (split-string (buffer-file-name) "lib/"))) "\\.")) )
		       "."
		       rb-method-root
		       )  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;(message obj-method) ;; Pry::Command.run
    (let ((file-and-line (second
			  (read (shell-command-to-string (concat " drb " home-path "/clojure_emacs/drb-help/rb_source.drb " obj-method)) )
			  )) )

      (let ((rb-file (first file-and-line))
	    (rb-line (first (last file-and-line))) )
	(let ((rb-buffer (first (last (split-string rb-file "/"))))
              (rb-obj (first (split-string obj-method "\\.")) )
              )
	  ;;         (message (concat rb-file "===" "245" "===" rb-buffer "=======" rb-obj) )
	  ;; /usr/local/rvm/gems/ruby-2.2.3/gems/pry-0.10.1/lib/pry/command.rb===245===command.rb=======Pry::Command

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  (progn
	    (find-file rb-file)
	    (goto-line rb-line rb-buffer)
	    (message (concat "cool, open the file: " rb-file " , " (number-to-string rb-line) " , " rb-obj))
	    )
;;;;;;;;;;;;;;;;;;;;;;;

	  )
	)
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    )
  )

(define-key global-map (kbd "C-c b") 'rb-source-find-next-super)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Like `C-x C-e` eval the ruby expression, such as: `User.first.id` => 3
(defun rb-eval ()
  (interactive)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/binding_eval.drb " "\"" (get-mark-content (current-buffer))  "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )
(define-key global-map (kbd "C-c p") 'rb-eval)


(defun rb-eval-var ()
  (interactive)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/binding_eval.drb " "\"" (get-point-keyword)  "\"")))
    (progn
      ;;(keyboard-quit) 
      (message (shell-command-to-string cmd-str))
      )
    )
  )
(define-key global-map (kbd "C-c y") 'rb-eval-var)

;;;;for testdatas
(defun rb-eval9018 ()
  (interactive)
  (let ((cmd-str (concat "drb9018 " home-path "/clojure_emacs/drb-help/binding_eval.drb " "\"" (get-mark-content (current-buffer))  "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )
(define-key global-map (kbd "C-c o") 'rb-eval9018) ;; C-x p

;;;;=> 如果是测试纯函数方法,需要提取方法上面的注释内容作为测试用法, 在drb服务端用lambda包住传进来的方法定义放上面＋注释用法调用放下面 : `#update_has_many_relation User.last(2), :post { |post| post.name="steve" } `
;; ==> 解决双单引号报错的问题
;; ` "aaa dsadas dsads".gsub(/a/, 'A') ` ==> (get-rb-obj-body) ==> "  \"aaa dsadas dsads\".gsub(/a/, 'A') "
;;; ===> ;; %Q{aaa dsadas dsads}.gsub(/a/, %q{A}) 已支持 ==> "AAA dsAdAs dsAds"
(defun rb-eval-expression-at ()
  (interactive)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/binding_eval.drb " "\"" (get-rb-obj-body)  "\"")))
    ;; cmd-str ==> "drb /home/clojure/clojure_emacs/drb-help/binding_eval.drb \"  \"aaa dsadas dsads\".gsub(/a/, 'A') \""
    (message (shell-command-to-string cmd-str))
    )
  )
(define-key global-map (kbd "C-c j") 'rb-eval-expression-at)

;;; for other testdatas project: drb9018
(defun rb-eval-expression-at9018 ()
  (interactive)
  (let ((cmd-str (concat "drb9018 " home-path "/clojure_emacs/drb-help/binding_eval.drb " "\"" (get-rb-obj-body)  "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )
(define-key global-map (kbd "C-c k") 'rb-eval-expression-at9018) ;; C-x j

;;;;;;;;; prod export data to dev
(defun prod-to-dev-datas ()
  (interactive)
  (let ((cmd-str (concat "drb9018 " home-path "/clojure_emacs/drb-help/binding_eval_prod_to_dev.drb " "\"" (get-rb-obj-body)  "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )
;;;;;;;;;;;;;; for spec
(defun spec-app-file ()
  (replace-regexp-in-string "spec" "app" (replace-regexp-in-string "_spec" "" (buffer-file-name) ))
  )
(defun app-spec-file ()
  (replace-regexp-in-string "app" "spec" (replace-regexp-in-string ".rb" "_spec.rb" (buffer-file-name) ))
  )
(defun open-spec () (interactive) (find-file (app-spec-file)) )
(defun open-app () (interactive) (find-file (spec-app-file)) )
;;;;;;;;;;;;;;;;

;;; The brakeman help parse the rails project
(defun method-find-call ()
  (interactive)
  (let ((cmd-str (concat "drb9 " home-path "/clojure_emacs/drb-help/method-find-call.drb9 " "\"" (get-mark-content (current-buffer))  "\""))) 
    (message (shell-command-to-string cmd-str))
    )
  )

(defun is-rb-params ()
  (interactive)
  (let ((cmd-str (concat home-path "/clojure_emacs/rkt-help/params_type " "\"" (ruby-parser-mark)  "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )

;;;;;

(defun get-point-keyword ()
  (interactive)
  (progn  
    (set-mark-command nil)
    (forward-sexp 1) 
    (get-mark-content (current-buffer)) )
  )
;;;;;;;;;;;;;;;;;;;;;;;;
;; %Q{aaa dsadas dsads}.gsub(/a/, %q{A}) 已支持
(defun get-rb-obj-body ()
  (interactive)
  (let ((cmd-str (concat home-path "/clojure_emacs/rb-help/expression_at.rb "  (buffer-file-name)  "  " (number-to-string (line-number-at-pos))  )))
    (shell-command-to-string cmd-str)
    )
  )
;;;;(define-key global-map (kbd "C-c y") 'get-rb-obj-body)

;;; (get-rb-obj-body-line-number 1) ==> 获取注释下面一行的表达式体
(defun get-rb-obj-body-line-number (line-number)
  (interactive)
  (line-number-at-pos)
  (let ((cmd-str (concat home-path "/clojure_emacs/rb-help/expression_at.rb "  (buffer-file-name)  "  " (number-to-string (+ line-number (line-number-at-pos))  )  )))
    (message (shell-command-to-string cmd-str))
    )
  )


;;  # aaa 1
;;  def aaa a
;;    a + 1
;;  end   
;;; rb-eval-expression-at-lambda ===> 2
(defun rb-eval-expression-at-lambda ()
  (interactive)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/binding_eval.drb " "\"" 
;;;;;;;;;
			 "-> { " (get-rb-obj-body-line-number 1) " ; " 
			 
			 (replace-regexp-in-string "^[[:space:]]+#" "" (get-rb-obj-body-line-number 0))

			 " }[] "
;;;;;;;;
			 "\"")))
    (message (shell-command-to-string cmd-str))
    )
  )
;;;C-c C-j整体  + C-c p局部Mark + C-c t wy-go-to-char
;;; 没有错误提示
(define-key global-map (kbd "C-c C-j") 'rb-eval-expression-at-lambda)

;;;;;;;;; Ruby纯函数 + `Use db/scheme.rb  & rails scaffold for test & factoryGril as datas`
;;;;; 下一个问题是什么: drb 的错误事件捕捉及对应类型生成代码, 用AutoFixErro的我写的Gem去做　==>> `C-c j or C-c p ` drb的错误处理中心，处理错误事件发生的处理, 如果发现不关联那就生成关联语句到项目drb的服务端, 并自动重启drb server服务端
(defun rails-scaffold-by-sch ()
  (interactive)
  (line-number-at-pos)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/expression_at_for_schema.drb "  (buffer-file-name)  "  " (number-to-string (line-number-at-pos))  )))
    (progn 
      (message (concat "Run: " (shell-command-to-string cmd-str)))
      (shell-command-to-string (shell-command-to-string cmd-str))
      (shell-command-to-string " rake db:migrate ")  )
    )
  )

(defun rails-scaffold-str ()
  (line-number-at-pos)
  (let ((cmd-str (concat "drb " home-path "/clojure_emacs/drb-help/expression_at_for_schema.drb "  (buffer-file-name)  "  " (number-to-string (line-number-at-pos))  )))
    (progn
      (message (shell-command-to-string cmd-str)))
    )
  )

;;; 检查drb server 9000 是否启动 ;;;;;;;; drb传异常给Emacs
(defun drb-server-check ()
  (message "The drb server is start")
  )

;; 打印到minibuffer然后再C-v复制出来吧
(defun copy
    (interactive)
  (get-mark-content (current-buffer))  
  )


(provide 'kungfu)

