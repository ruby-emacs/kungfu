# KungFu : 实现Ruby项目Repl的循环, 与Emacs的交互式开发, 灵感来自Cider(Clojure)

*项目创建的起源: 解决Ruby项目Gem等开发,无法实时和Emacs交互的问题, 起初是为了用Rails的underscore功能直接修改文档, 利用Emacs的replace-regexp, 将文档中的驼峰式写法转为underscore写法*

### 功能: 

* 是对调试功能的扩充: 调试功能是提供一个repl循环, 可以和用户交互实现片段代码测试是否正确, 而本项目是实现Emacs与repl循环的交互式开发
* 与Emacs交互开发实现: 当前项目断点环境binding的片段代码执行, 标记代码执行, 当前Ruby函数的测试 
* Repl可以实现多次交互执行的, 历史补丁: 如交互过程中, 变量或者方法重名, 后者会覆盖前者的定义或者结果, binding的历史补丁
* 通过本项目可以用Ruby快速开发当前项目环境相关的Emacs插件: 使得当前项目的开发, 变得命令化, 功能平行化， 就像 `.zshrc or .bashrc ` 帮助bash开发一样方便, 不同之处在于, 这里的插件的命令是运行在某一特定的项目环境binding下
* 文档驱动开发, 代码注释作为第一手测试: 通过apiary或markdown文档, 驱动开发, 结果对比文档的结果
* 实现代码的精确跳转, 通过项目环境的binding查找定义位置跳转, 而不是用ctags等: 解决代码的多层往下跳转的问题, 以及
* 利用Ruby转为S表达式, 实现代码的多个条件结构化递归搜索: 多关键词组合搜索, 语法结构特征关键词(如Lambda), 变量名或方法名关键词, 调用或者是定义的结构特征关键词(find_call) 等定位多条件搜索结果,精确到某个方法定义或者lambda定义, 或者的变量定义

### API文档:

| 函数名        | 键名       | 使用功能说明  |
| ------------- |:-------------:| -----:|
| downcase-str |    |将标记部分正则替换replace-regexp为全小写,并用中隔线相连,解决Markdown的URL目录生成问题  |
| rb-underscore |    | 将标记部分正则替换为Rails的underscore写法      |
| rb-understr | C-c i  | 将标记部分替换为Rails的underscore写法      |
| rb-camelize |     | 将标记部分正则替换为Rails的camelize大写  |
| ruby-parser |     | 传入Ruby字符串,将其转为S表达式 |
| ruby-parser-mark | C-c l  |  将标记部分的Ruby表达式转为S表达式,打印出来|
| get-api-to-doc |     |  Http请求API将其JSON结果打印处理,url为参数  |
| get-mark-content |   |  获取标记部分的内容,传递给其他函数使用  |
| get-mark-content-for-bash |   |  获取标记部分的内容,替换为Bash接受的字符串  |
| source |   |  就像source ~/.zshrc 一样让init.el生效 |
| http-send-apiary |    |  (http-send-apiary "users" "post")post请求users的API  |
| http-one-params |    |  获取当前行的内容作为参数请求API: 参数为(url http-method)  |
| http-send-apiary-params |   | 获取多参数作为API请求, 参数为(url http-method)  |
| wy-go-to-char | C-c t  |  单字符的向下跳转,用于快速Mark标记跳转到某字符处 |
| rb-source |   |  Emacs查看Ruby的函数定义位置不跳转: Mark "obj.method"  |
| rb-source-find | C-c g  |  Emacs查看Ruby的函数定义跳转: Mark "obj.method"     |
| rb-source-find-next | C-c n  |  第二次跳转用第一次的对象查找它的方法, 会try两次包括实例方法和类方法: Mark "instace_method"      |
| rb-source-find-next-super | C-c b  |  No need Mark any, use (buffer-file-name) as Class name, rb-method-root as method name     |
| rb-eval | C-c p  |   获取Mark缓冲内容发送给项目的repl执行    |
| rb-eval-var | C-c y  |    只是看变量的值   |
| rb-eval9018  | C-c o  |   执行表达式在生产环境的sandbox的repl    |
| rb-eval-expression-at | C-c j  | 发送当前表达式的多行结构到repl执行    |
| rb-eval-expression-at9018  | C-c k  |   发送当前表达式的多行结构到生产环境sandbox的repl执行     |
| open-spec/open-app |  | 打开对应的测试或源文件 |
|  method-find-call |  |  查找项目中所有的call调用,灵感来自brakeman |
| is-rb-params |  |  判断当前标记部分或当前的表达式是否为params的结构体,采用Racket编写判断表达式类型,灵感来自brakeman和Lisp列表的处理  |
| get-point-keyword |  | 获取当前的光标的单词  |
| get-rb-obj-body | C-c y  |  获取当前光标位置向下获取完整的独立结构定义的内容: source_location     |
| get-rb-obj-body-line-number |  | 获取注释下面一行的表达式体  |
| rb-eval-expression-at-lambda | C-c C-j  |  用注释内容测试当前纯函数:  C-c C-j整体  + C-c p局部Mark + C-c t wy-go-to-char   |
| jw-eval-or-clear-buffer | C-c v | 执行整个文件的内容,就像cider的`C-c C-k`  |
| restart-drb-server | |  重启drb-server,并把客户端的执行环境,合并到drb server环境,即binding的合并 |
