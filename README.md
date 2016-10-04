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
| wy-go-to-char | C-c t  |  单字符的向下跳转,用于快速Mark标记跳转到某字符处 |
| rb-source-find | C-c g  |       |
| rb-source-find-next | C-c n  |       |
| rb-source-find-next-super | C-c b  |       |
| rb-eval | C-c p  |       |
| rb-eval-var | C-c y  |       |
| rb-eval9018  | C-c o  |       |
| rb-eval-expression-at | C-c j  |       |
| rb-eval-expression-at9018  | C-c k  |       |
| get-rb-obj-body | C-c y  |       |
| rb-eval-expression-at-lambda | C-c C-j  |       |
