# My Emacs configuration
这是我的个人配置，可能会存在一些过于个人化的部分。

总的来说我的使用习惯倾向于「减小心智负担」而不是「快速」，所以有些地方我会更多使用M-x和一个好记的交互函数而不是定义太多的按键绑定。

我的主要使用平台是macOS上的mac port和ns port，以及终端下的`emacs -nw`。配置文件会尽可能兼容其他平台，但不常使用的平台可能更新并不及时。

这个配置会兼容Emacs版本最低29最高master HEAD。

# 配置结构
配置文件的入口是init.el。init.el会按列表加载(`require`)位于`conf`下的各个配置。

各个配置部分按照松散的名字归类，从文件名上可以直接看出内涵。绝大部分的部分可以自由选择要不要`require`，只有下面几个例外：

- 包管理的部分，init-elpa与init-straight是互斥的，只可以启用一个。不过二者都提供了相同的兼容函数供配置的其他部分使用。见详细说明。
- init-flymake与init-flycheck互斥，分别使用内置Flymake或Flycheck显示和导航diagnostics。
- init-helm与init-consult互斥。我尽可能在二者都使用相同的按键绑定，这样除了操作逻辑和界面的差别之外，肌肉记忆大致上保持一致。
- init-company与init-corfu互斥。我尽可能让二者的手感和偏好一致，虽然总归会有一些微小的差别。
- init-gui/init-terminal还有init-macos/init-x11部分虽然互斥，但init.el里可以按照逻辑自动判断，不需要手动注释。

init-local是留给各个系统配置一些系统独有、不便放入这个repo里的配置。

# 详细
## init-elpa
包管理配置之一，基于内置的package.el，源里加上了MELPA。package目录按Emacs版本分开存放（比如`elpa-30.2/`），多个版本共用这份配置时互不干扰。另外装了quelpa，用来装不在ELPA/MELPA里的包。

与init-straight互斥。二者提供相同的ensure-package和upgrade-all-packages接口，配置的其他部分只管调用，不用关心背后是哪个包管理器。

### 函数
- ensure-package : 装包的统一接口。参数是包名就走package.el，是`(name :url ... [:branch ...])`形式的recipe就走quelpa
- upgrade-all-packages : 升级所有已安装的包，ELPA版实现

## init-straight
包管理配置之一，基于straight（develop分支，浅clone）。build目录按Emacs版本分开（比如`build-30.2`）。与init-elpa互斥，同样提供ensure-package和upgrade-all-packages两个统一接口。

macOS不再自带texinfo，straight构建info文件会失败，而Homebrew装的texinfo是keg-only不在PATH里。所以在macOS上我用这样的命令来bootstrap和升级：

```
PATH="$(brew --prefix)/opt/texinfo/bin:$PATH" emacs --batch -l ~/.emacs.d/init.el --eval '(straight-pull-all)'
```

### 函数
- ensure-package : 装包的统一接口，straight版实现。同样接受包名或者`(name :url ... [:branch ...])`形式的recipe
- upgrade-all-packages : 升级所有已安装的包，straight版实现，实际就是straight-pull-all

## init-env
GUI下用exec-path-from-shell把shell里的PATH等环境变量同步进Emacs。

## init-common
基础的杂项配置，大致包括：备份/自动保存/lock文件统一放到`user-emacs-directory`下的tmp目录、visible bell（mode-line-bell）、 y/n代替yes/no、tramp默认ssh、滚动行为、global-auto-revert、dired的ls参数按系统区分、bookmark（有Dropbox的机器用`~/Dropbox/dropbox.bmk`）、 macOS下删除进回收站、ibuffer（ibuffer-vc按repo分组）、savehist、 save-place、recentf、uniquify，还有让minibuffer继承调用处buffer的输入法。

orderless也放在这里配置，因为consult和corfu都会用到，放这里减少重复。

### 按键绑定
- C-x C-b : ibuffer（remap list-buffers）
- \<f5\> : goto-line
- \<f6\> : display-line-numbers-mode
- \<f8\> : rename-buffer
- \<f12\> : bookmark-bmenu-list
- C-x M-c : save-buffers-kill-emacs

## init-editor
编辑行为相关的配置：fill-column 70、缩进用空格且tab-width默认2、 show-paren、electric-pair（补充了几组全角括号引号）、prog-mode显示行号、大文件用so-long和vlf、subword、拼写检查（优先aspell其次hunspell）、hippie-expand代替dabbrev-expand、symbol-overlay高亮、 whole-line-or-region（没有选区时C-w/M-w/M-;等直接作用于当前行）、 separedit、tab-bar、dogears记录光标位置、查词典（macOS用系统词典，其他系统用Bing）、放开narrow相关命令、indent-bars缩进参考线。

### 函数
- ffap-vlf : 用VLF打开光标处的文件名，配合大文件使用

### 按键绑定
- C-c ; : flyspell-correct-wrapper，修正拼写（flyspell-mode下）
- M-/ : hippie-expand（remap dabbrev-expand）
- C-c h : symbol-overlay-put
- \<f4\> : symbol-overlay-jump-next
- M-c : capitalize-dwim
- C-\<return\> : 在下方新开一行并跳过去，org-mode之外自动缩进
- C-x t v / C-x t b / C-x t l : tab-next / tab-previous / tab-list
- M-g d : dogears-go
- M-g M-b / M-g M-f : dogears-back / dogears-forward
- M-g M-d / M-g M-D : dogears-list / dogears-sidebar
- C-c d : 查词典，macOS是osx-dictionary-search-word-at-point，其他系统是bing-dict-brief

## init-utils
各种工具类package：projectile、bm（可视书签）、rg、dashboard（启动页， 末尾加了一段自制的Emacs运行状态）、magit、scratch、vundo、ox-hugo。

### 按键绑定
- C-c p : projectile-command-map，projectile的命令前缀
- \<f9\> / C-\<f9\> / S-\<f9\> : bm-toggle / bm-next / bm-previous
- C-c r : rg-dwim，直接使用当前关键词
- C-c R : rg，带交互选项
- C-c D : 打开Dashboard
- C-x g : 只打开magit，不刷新，保证打开速度。按g刷新状态
- C-c s : scratch，开一个当前major mode的scratch buffer
- C-x u : vundo

## init-symbols
代码符号相关：eglot（29.1起是内置）、citre（ctags/gtags前端）、xref（搜索用ripgrep）、tree-sitter的语法源列表和major mode remap。

### 函数
- citre-global-toggle : 全局开关citre-mode（挂/摘prog-mode-hook，对当前buffer立即生效）
- citre-peek+ : 光标在symbol上时citre-peek，否则恢复上一次peek会话

### 按键绑定
- C-c ' : citre-peek+

### 补充
我希望保持一个轻量化的查找，所以默认状态下可以直接用etags文件，使用emacs内置；或者可以用gtags或者ctags，用`citre-global-toggle`打开citre-mode，使用citre的跳转和peek功能；或者可以打开eglot，使用LSP。

citre是全局手工开关；eglot是按项目手工开关。

## init-flymake
使用Emacs内置Flymake，在text-mode和prog-mode中启用。与init-flycheck互斥。

### 按键绑定
- M-n / M-p : flymake-goto-next-error / flymake-goto-prev-error

## init-flycheck
使用Flycheck作为diagnostics frontend，并启用内置Eglot bridge。Eglot仍负责LSP功能，Flycheck负责显示和导航diagnostics；不启用Flycheck的diagnostics-only native LSP或inline annotations。与init-flymake互斥。

### 按键绑定
- M-n / M-p : flycheck-next-error / flycheck-previous-error

## init-helm
helm全家桶，与init-consult互斥。除helm本体外还有helm-swoop、helm-ag、 wgrep-helm、helm-ls-git、helm-xref、flyspell-correct-helm、 helm-projectile。同时把init-notes用的my/notes-grep-function和my/notes-find-function设置成helm的实现。

### 函数
- my/helm-do-grep : 用普通grep递归搜索default-directory
- helm-grep-ag-projectile : 用rg搜索当前projectile项目
- helm-grep-ag-projectile-again : 在一个helm grep会话里改成搜索整个项目

### 按键绑定
- M-x / C-x C-f / C-x b / M-y / C-x r b : remap到对应的helm版本（helm-M-x、helm-find-files、helm-mini、helm-show-kill-ring、 helm-filtered-bookmarks）
- C-c v : helm-resume，恢复上一次helm会话
- C-c g : rg搜索当前目录，再按一次搜索整个项目
- M-g i / M-g I : helm-semantic-or-imenu / helm-imenu-in-all-buffers
- M-g p / M-g P : helm-browse-project / helm-projects-history
- M-s d : helm-find
- M-s o : helm-occur（isearch中也可用，M-s O是多buffer版）
- C-c ] : helm-toggle-buffers-details（helm的buffer列表内）
- C-c C-l : helm-minibuffer-history（minibuffer内）
- C-c G / M-s g : helm-grep-ag-projectile，rg搜索当前项目
- C-c SPC : helm-projectile

### 补充
helm默认没有一个「搜索整个项目」的功能，这里借助projectile来实现。C-c g设计成默认查找当前目录，在搜索结果里再按一次C-c g就可以直接用当前关键词查找整个项目。

## init-consult
minibuffer补全全家桶：vertico + consult + embark + marginalia，与init-helm互斥。绑定大部分来自consult README的推荐绑定。同时把init-notes用的my/notes-grep-function设置成consult-ripgrep。

consult-line/consult-ripgrep等命令会把光标处的symbol当作初始输入（灰色显示，直接输入新内容会清掉）。

### 按键绑定
vertico/embark部分：
- C-c v : vertico-repeat，恢复上一次minibuffer会话（对应helm-resume）
- RET / DEL / M-DEL : vertico-directory的目录导航（vertico内）
- C-. : embark-act（全局和vertico内。flyspell和org里的C-.已解绑让位）
- C-h B : embark-bindings
- C-c C-o : embark-export（vertico内）

consult部分：
- C-x b / C-x 4 b / C-x 5 b / C-x t b : consult-buffer系列
- C-x r b : consult-bookmark
- C-x p b : consult-project-buffer
- M-y : consult-yank-pop
- M-# / M-' / C-M-# : consult-register-load / consult-register-store / consult-register
- C-c M-x / C-c m / C-c i : consult-mode-command / consult-man / consult-info
- C-x M-: : consult-complex-command
- M-g g（M-g M-g）: consult-goto-line
- M-g e / M-g f : consult-compile-error / consult-flymake或consult-flycheck（按启用的diagnostics模块选择）
- M-g o / M-g m / M-g k : consult-outline / consult-mark / consult-global-mark
- M-g i / M-g I : consult-imenu / consult-imenu-multi
- M-s f : consult-fd
- M-s c : consult-locate
- M-s g / M-s G : consult-ripgrep / consult-git-grep
- C-c g : rg搜索当前目录，再按一次搜索整个项目
- M-s l / M-s L : consult-line / consult-line-multi（isearch中也可用）
- M-s k / M-s u : consult-keep-lines / consult-focus-lines
- M-s e : consult-isearch-history（isearch内M-e也可以）
- M-s / M-r : consult-history（minibuffer内）
- C-c SPC : consult-projectile

### 补充
C-c g的设计和init-helm对齐。

## init-company
buffer内补全，与init-corfu互斥。backend顺序是capf（带yasnippet）、文件名、dabbrev-code、keywords、dabbrev。用company-prescient按使用频率排序。

### 函数
- my/company-good-candidate-p : 候选过滤规则，不补全带中文的、纯数字的、超过30字符的

### 按键绑定
- C-; : company-complete，手动触发补全
- C-n / C-p : 上下选择候选（补全菜单内）
- C-s : company-filter-candidates，在候选里过滤（补全菜单内）
- TAB : company-complete-common-or-cycle（补全菜单内）
- M-n / M-p : 翻看候选的文档（补全菜单内）
- C-h : company-show-doc-buffer（补全菜单内）

## init-corfu
buffer内补全，与init-company互斥。搭配cape补充dabbrev/文件名/keyword等来源，kind-icon显示类型图标。corfu-prescient只负责排序，过滤交给orderless，这样TAB还能补公共前缀。31之前的版本在终端里用corfu-terminal。另外取消了corfu对C-a/C-e的remap，让它们像company一样先提交候选再退出。

### 函数
- my/corfu-good-candidate-p : cape-dabbrev的候选过滤规则，和company版本相同

### 按键绑定
- C-; : completion-at-point，手动触发补全
- TAB : corfu-expand（补全菜单内）
- S-TAB : corfu-previous（补全菜单内）
- C-s : corfu-insert-separator，输入orderless分隔符（补全菜单内）

## init-modes
各种major mode的杂项配置：markdown-mode、dockerfile-mode、yaml-mode、 go-mode、beancount，以及text-mode/prog-mode的公共hook（visual-line、 flyspell、显示行尾空格）。还有CJK相关的word-wrap-by-category和把中文句号等算进sentence-end。

## init-yasnippet
yasnippet加yasnippet-snippets词库，after-init时全局启用。

### 按键绑定
- C-c y : yas-insert-snippet

## init-rime
输入法配置之一（emacs-rime），与init-pyim互斥。默认中文，通过rime-disable-predicates在代码区、大写字母、行首标点、ascii字符后、 minibuffer等场景自动回落到英文。退出Emacs时主动finalize librime避免崩溃。

### 函数
- rime-send-menu-keybinding : 给librime发送C-\`打开Rime的方案/选项菜单（TUI下这个键直接按发不出去，包了一层）

### 按键绑定
- C-c \` : rime-send-menu-keybinding（rime-mode下）
- M-j : rime-force-enable，强制中文（rime-mode下）

## init-pyim
输入法配置之一（pyim），与init-rime互斥。探针规则对应init-rime的predicates：代码区（注释/字符串除外）、ascii后、minibuffer默认英文；行首标点和半角标点后继续半角。默认方案是全拼（清华词库），五笔86的词库也装了可以切换。没有定义函数。

### 按键绑定
- M-j : pyim-convert-string-at-point，把光标前的拼音串转成中文

## init-org
org mode配置。org-directory优先选择`~/Dropbox/notes`，否则就是`~/notes`，和init-notes一致。TODO关键字用TODO/NEXT/DONE加DEFER/HOLD/ABORT两条序列。capture模板、refile目标、归档位置都指向org-directory下的文件，agenda文件列表放在org-directory里的`.agenda-files`维护（配合公开repo不泄露私有文件名）。还调整了emphasis的正则，让markup记号前后可以直接是中文。

agenda定义了三个自定义视图：d（每日dashboard）、r（每周回顾）、n（所有任务）。

### 函数
- my/org-capture-setup-windows : org-capture时准备一个简单的写作布局， 窗口够宽时右边放原来的buffer
- org-journal : 打开journal.org并定位到文件结尾，自制的org-journal

### 按键绑定
- C-c l : org-store-link
- C-c b : org-switchb
- C-c c : org-capture
- C-c a : org-agenda
- C-c j : org-journal

### 补充
C-c ! 插入非激活当前日期，C-u前缀插入日期和时间。

## init-notes
操作个人笔记的配置。包含了notes-find和notes-grep的定义。前者用于查找notes目录里的文件，后者用于搜索notes目录的文件内容。两个函数都能优先使用helm/consult里定义的帮助函数。

个人笔记的路径优先选择`~/Dropbox/notes`，否则就是`~/notes`。

### 按键绑定
- C-c n g : notes-grep，grep/rg notes目录
- C-c n f : notes-find，find/fd notes目录
- C-c n i : notes-inbox，直接打开inbox.org文件并且定位到文件结尾

## init-my
自己写的一些通用小函数，不依赖外部package。

### 函数
- my/insert-time : 插入当前时间，带prefix插入完整日期时间
- my/textmate-shift-right / my/textmate-shift-left : 行或选区整体右移/左移一个tab-width
- my/smarter-move-beginning-of-line : 在行首和第一个非空白字符之间来回切换
- my/eshell-here / my/shell-here : 在下方分屏，在当前buffer所在目录打开eshell/shell，同目录已有的会话会复用
- my/bookmark-reload : 从bookmark-default-file重新加载书签
- my/split-window-other : 分屏并跳过去，C-u时上下分，否则左右分
- my/split-window-for-writing : 写作布局，窗口够宽时左右分屏，当前buffer在左，右边放上一个buffer

### 按键绑定
- M-] : my/textmate-shift-right（M-】也绑了，输入法全角状态下也能用）
- M-[ : my/textmate-shift-left（终端下是C-M-]，避开转义序列冲突）
- C-a : my/smarter-move-beginning-of-line（remap move-beginning-of-line）
- C-c t : my/split-window-other
- C-c T : my/split-window-for-writing

## init-theme
主题相关：把`themes/`加进custom-theme-load-path，里面有几个自己收的主题和一份改过的solarized。实际启用哪个主题由customize管理（custom-enabled-themes），不在这里写死。

## init-terminal
终端下的配置，与init-gui互斥，init.el按window-system自动选择。内容只有鼠标相关：xterm-mouse-mode打开终端鼠标支持，调整滚轮行为（一次2行， 按住shift一次1行，不加速，滚动鼠标所在的窗口）。

## init-gui
GUI下的通用配置，与init-terminal互斥。保留menu-bar，关掉tool-bar和scroll-bar，frame标题格式，光标用bar，默认frame大小90x50，改了fringe里续行箭头的样式，最后start server。

### 按键绑定
- C-z : 解绑，GUI下不要最小化

## init-macos
macOS下的配置（mac/ns port）。option作为meta，command作为hyper，然后给hyper绑一套接近macOS习惯的快捷键。NS port有单独一段调整（滚动、透明标题栏等）。mac port下启用mac-auto-operator-composition-mode（连字）和mac-auto-ascii-mode（按下prefix key后自动把输入法切回ascii）。

字体部分：英文用PragmataPro Mono，CJK定义了一组字体profile（思源宋/黑、Noto宋/黑、苹方、新书宋、华文中宋、北魏楷书），每个profile按charset（han/kana/hangul等）成套设置，切换字体就是换一个profile。启动时应用normal字号。

### 函数
- mac-switch-meta : 在两种modifier布局之间切换（option=meta+ command=hyper，或者option不动+command=meta）
- my/set-font-normal / my/set-font-large : 常规/大字号两套字体设置， 分别搭配苹方和新书宋

### 按键绑定
hyper就是command键，模仿macOS习惯的一套：
- H-a : mark-whole-buffer
- H-c / H-v : kill-ring-save / yank
- H-s : save-buffer
- H-z : undo
- H-o : find-file
- H-f : isearch-forward
- H-l : goto-line
- H-r : revert-buffer
- H-{ / H-} : previous-buffer / next-buffer

frame操作：
- H-\` : other-frame
- H-n : make-frame-command
- H-w : delete-frame
- H-m : suspend-frame

## init-x11
X11/pgtk下的配置：设置默认字体（PragmataPro Mono），x下把Alt当作meta（x-alt-keysym），关掉menu-bar。

## init-local
不在repo里，留给每台机器放本地私有配置。init.el里用`(require 'init-local nil t)`加载，文件不存在也不会报错。
