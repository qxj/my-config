"""" General
set nocompatible " get out of horrible vi - compatible mode
filetype on " detect the type of file
filetype plugin on " load filetype plugins
filetype indent on
set history=1000 " How many lines of history to remember
set clipboard+=unnamed " turns out I do like is sharing windows clipboard
set viminfo+=! " make sure it can save viminfo
set isk+=_,$,@,%,#,- " none of these should be word dividers,so make them not be
set autowrite
set autoread
set autochdir
set nobackup
set gdefault
"set helplang=cn
set copyindent
set showbreak=\ \ \ \ \  "indicator for wrapped lines
set shellslash

"""" Theme/Colors
syntax on " syntax highlighting on
if (has("gui_running"))
    set background=dark " we are using a dark background
    set nowrap
    set guifont=Consolas:h11
    set lines=50 columns=90
    colorscheme morning
else
    set paste "this option is useful when using Vim in a terminal
    set wrap
    colo ron
endif

"""" Vim UI
set wmh=0 "This sets the minimum window height to 0
set lsp=0 " space it out a little more (easier to read)
set wildmenu " turn on wild menu
set wildmode=list:longest,full
set ruler " Always show current positions along the bottom
set cmdheight=1 " the command bar is 2 high
"set number " turn on line numbers
set lz " do not redraw while running macros (much faster) (LazyRedraw)
set hid " you can change buffer without saving
set switchbuf=useopen
set backspace=2 " make backspace work normal
set whichwrap=b,s,<,>,[,],h,l  " backspace and cursor keys wrap to
set mouse=a " use mouse everywhere
set shortmess=atI " shortens messages to avoid 'press a key' prompt
set report=0 " tell us when anything is changed via :...
" make the splitters between windows be blank
"set fillchars=vert:\,stl:\,stlnc:\
"donot show toolbar,menubar and tabbar
set guioptions-=b
set guioptions-=T "get rid of toolbar
"set guioptions-=m "get rid of menu
"set guioptions-=e "remove the gui tabbar
"avoid windows explain alt combining keybinds
set winaltkeys=no
if version>=700
    set pumheight=10 "set popup menu hight
    set showtabline=2
endif

"""" Visual Cues
set showcmd
set showmatch " show matching brackets
set matchpairs=(:),{:},[:],<:>
set mat=5 " how many tenths of a second to blink matching brackets for
set nohlsearch " do not highlight searched for phrases
set incsearch " BUT do highlight as you type you search phrase
set ignorecase smartcase
"set listchars=tab: \ | \ ,trail: .,extends: > ,precedes: < ,eol: $ " what to show when I hit :set list
"set lines=41 " 80 lines tall
"set columns=160 " 160 cols wide
set so=5 " Keep 10 lines (top/bottom) for scope
set novisualbell " don't blink
set noerrorbells " no noises
set titlestring=%F
set statusline=%k(%02n)%t%m%r%h%w\ \[%{&ff}:%{&fenc}:%Y]\ \[%l/%L,%c]\ [%p%%]
set laststatus=2 " always show the status line
"set cursorline

"""" Text Formatting/Layout
set fo=tcrqn " See Help (complex)
set si " smartindent
set tabstop=4 " tab spacing (settings below are just to unify it)
set softtabstop=4 " unify
set shiftwidth=4 " unify
set noexpandtab " real tabs please!
"set nowrap " do not wrap lines
set smarttab " use tabs at the start of a line,spaces elsewhere

"""" File encoding
if has("multi_byte")
    set fileencodings=ucs-bom,utf-8,cp936,big5,euc-jp,euc-kr,latin1
    if v:lang =~ "^zh_CN"
        " Use cp936 to support GBK, euc-cn == gb2312
        set encoding=cp936
        set termencoding=cp936
        set fileencoding=cp936
    endif
    " Detect UTF-8 locale, and replace CJK setting if needed
    if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
        set encoding=utf-8
        set termencoding=utf-8
        set fileencoding=utf-8
    endif
else
    echoerr "Sorry, this version of (g)vim was not compiled with multi_byte"
endif

"""" Folding
set foldenable " Turn on folding
"set foldmethod=indent " Make folding indent sensitive
set foldmethod=manual " Make folding indent sensitive
set foldlevel=100 " Don't autofold anything (but I can still fold manually)
set foldopen-=search " don't open folds when you search into them
set foldopen-=undo " don't open folds when you undo stuff

"""" Win Manager
let g:winManagerWidth=35 " How wide should it be( pixels)
let g:winManagerWindowLayout='FileExplorer' " What windows should it
"let g:winManagerWindowLayout='TagList,FileExplorer|BufExplorer' " What windows should it
let g:persistentBehaviour=0 "vim will quit if only the explorers window are the one left

"""" Buffer Explorer
let bufExplorerDefaultHelp=0
let bufExplorerDetailedHelp=0
let bufExplorerMaxHeight=15

"""" Remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :20  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:20,%,n~/.viminfo
if has("autocmd")
    au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

"""" Keybind
inoremap <C-a> <Home>
inoremap <C-e> <End>

