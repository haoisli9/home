
set nocp

" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes
Plug 'majutsushi/tagbar'
Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'
Plug 'preservim/nerdcommenter'
"Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'
Plug 'skywind3000/vim-preview'
Plug 'luochen1990/rainbow'
Plug 'dkprice/vim-easygrep'
"Plug 'mileszs/ack.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'google/vim-searchindex'
Plug 'tpope/vim-repeat' "更为强大的重做功能
Plug 'tpope/vim-surround' "符号自动环绕
Plug 'mhinz/vim-startify'
Plug 'tmhedberg/matchit' 
Plug 'mattn/calendar-vim'
Plug 'name5566/vim-bookmark'
Plug 'xuhdev/SingleCompile'

Plug 'terryma/vim-expand-region'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'
Plug 'ervandew/supertab'

" color
Plug 'tomasr/molokai'
Plug 'liuchengxu/space-vim-dark'
"Plug 'altercation/vim-colors-solarized'

" Any valid git URL is allowed
"   Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
"   Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
"
" Plugin outside ~/.vim/plugged with post-update hook
"   Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
"Plug 'Yggdroot/LeaderF', { 'do': '.\install.bat' }
"Plug 'Yggdroot/LeaderF-marks'

"   Unmanaged plugin (manually installed and updated)
Plug '~/.vim/plugin/WhereFrom.vim'
Plug '~/.vim/plugin/SyntaxAttr.vim'
Plug '~/.vim/plugin/Translateit'

" Initialize plugin system
call plug#end()

"------------------------------------------------------------------
" Settings  
"------------------------------------------------------------------ 
" 终端支持255色
set t_Co=256

set cursorline

set fileencodings=utf-8,ucs-bom,cp936,gb18030,gb2312,big5,euc-jp,euc-kr,latin1
set termencoding=utf-8
set encoding=utf-8

set fileformats=unix,dos,mac " EOL formats(\r\n or \n or \r) in order

" set gfn=Sarasa Mono SC Regular:h20
"set gfn=Fira\ Code:h20:cANSI
"set gfw=楷体:h20:cGB2312

set background=dark
"colo torte
colo space-vim-dark

" 启动的时候不显示那个援助索马里儿童的提示
set shortmess=atI
 
set viminfo=%,<50,'10,/50,:50,h,f0,n~/.viminfo
"           | |    |   |   |    | |  + viminfo file path
"           | |    |   |   |    | + file marks 0-9,A-Z 0=NOT stored
"           | |    |   |   |    + disable 'hlsearch' loading viminfo
"           | |    |   |   + command-line history saved
"           | |    |   + search history saved
"           | |    + files marks saved
"           | + lines saved each register (old name for <, vi6.2)
"           + save/restore buffer listlet $LANG = 'en'

"------------------------------------------------------------------
" mouse使用V模式，否则在Select模式下，部分mswin操作不正常 
"------------------------------------------------------------------ 
set mouse=a
set selectmode=key

"------------------------------------------------------------------
" VIM user interface setting 
" No sound on errors.
set noerrorbells
set visualbell
set t_vb=
set ignorecase
set backspace=indent,eol,start      "Set backspace
set whichwrap+=<,>,h,l              "允许backspace和光标键跨越行边界
set incsearch
set hlsearch

"------------------------------------------------------------------
"" 文本编辑设置 
"------------------------------------------------------------------ 
set sw=4       " 自动缩进的时候， 缩进尺寸为 4 个空格
set ts=4       " Tab 宽度为 4 个字符 
set sts=4      " softtabstop
set et         " 编辑时将所有 Tab 替换为空格
set lbr        " 不在单词中间断行
set nu         " Show line number.
set showmatch  " show matching bracets
set linespace=4

set autoindent                          " 用来自动缩进
set smartindent                        " 自动判断缩进长度，一般适用于类C语言

"------------------------------------------------------------------
" 当使用UTF-8时，定义中文字符宽度 
"------------------------------------------------------------------ 
set ambiwidth=double

"------------------------------------------------------------------
" leader设定 
"------------------------------------------------------------------ 
let mapleader = "\<space>"

"------------------------------------------------------------------
" iskeyword setting.  
" The "@" stands for all alphabetic letters.  "48-57" stands for ASCII
" characters 48 to 57, which are the numbers 0 to 9.  "192-255" are the
" printable latin characters.
"------------------------------------------------------------------ 
"set iskeyword=@,48-57,_,192-255,-

"-------------------------------------------------------------------
" show TAB with >. and trail space with .
"-------------------------------------------------------------------
"set listchars=tab:>.,trail:.
set listchars=tab:>>,trail:-,eol:<,nbsp:. 

"-------------------------------------------------------------------
" backup file 
"------------------------------------------------------------------- 
set backup
set backupdir=~/auto-save-list
" set swap file directory, you can see it with :set directory?
set directory=~/auto-save-list

"-------------------------------------------------------------------
" backup file 
"------------------------------------------------------------------- 
set undofile
set undodir=~/.cache/vim
set undolevels=100 "maximum number of changes that can be undone

"-------------------------------------------------------------------
"  fold things 
"------------------------------------------------------------------- 
set foldmethod=indent
set foldcolumn=1
set foldlevel=10
set foldopen-=search " don't open folds when you search into them
set foldopen-=undo " don't open folds when you undo stuff
set foldenable
" 用空格开关折叠 <CR> --> <space>
nnoremap <silent> <leader>zz :call FoldEx()<CR>
function FoldEx()
    if foldlevel(line('.')) != 0
        if foldclosed(line('.')) < 0
            execute 'normal zc'
        else
            execute 'normal zo'
        endif
    else
        execute 'normal l'
    endif
endfunc
                                  
"------------------------------------------------------------------
"" diff 忽略空格 
"------------------------------------------------------------------ 
set diffopt=filler,iwhite

"在插入模式下移动光标
imap <C-h> <Left>
imap <C-j> <Down>
imap <C-k> <Up>
imap <C-l> <Right>

"-------------------------------------------------------------------
"  Visual Search 
"------------------------------------------------------------------- 
" From an idea by Michael Naumann
function! VisualSearch(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"
    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")
    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    else
        execute "normal /" . l:pattern . "^M"
    endif
    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

"Basically you press * or # to search for the current selection !! Really useful
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

function! Range_Search(mypat, start_line, end_line)
    let full_pat = '\%>' . a:start_line . "l" . '\%<' . a:end_line . "l" . a:mypat
    silent exe "normal /" . full_pat . "^M"
    let @/ = full_pat
    norm n
endfunction

command -range -nargs=1 RangSearch call Range_Search(<f-args>,<line1>,<line2>)
" 注意RangSearch后面要有一个空格
vnoremap /  :RangSearch 

" Make a simple "search" text object.
" Use / to search 'String', use csString to replace string.
" Then use n . to replace;
" vnoremap <silent> s//e<C-r>=&selection=='exclusive'?'+1':''<CR><CR>
"             \:<C-u>call histdel('search',-1)<Bar>let@/=histget('search',-1)<CR>gv
" omap s :normal vs<CR>

"------------------------------------------------------------------------
function AlwaysCWD()
    if bufname("") !~ "^\[A-Za-z0-9\]*://"
        silent! lcd %:p:h
    endif
endfunction
" autocmd BufEnter * call AlwaysCWD()
" "map <F5> :call AlwaysCWD()<CR>

" Function to display the current character code in its 'file encoding'
function! EchoCharCode()
    let char_enc=matchstr(getline('.'), '.', col('.') - 1)
    let char_fenc=iconv(char_enc, &encoding, &fileencoding)
    let i=0
    let len=len(char_fenc)
    let hex_code=''
    while i < len
        let hex_code.=printf('%.2x',char2nr(char_fenc[i]))
        let i+=1
    endwhile
    echo '<' . char_enc . '> Hex ' . hex_code . ' (' .
                \(&fileencoding != '' ? &fileencoding : &encoding) . ')'
endfunction
nmap <silent> <leader>9  :call EchoCharCode()<CR>
nmap <silent> <leader>8	 :call SyntaxAttr()<CR>

fun! HelpWord() 
    "Assign current word under cursor to a script variable: 
    let s:help_word = expand('<cword>') 

    " Read in the help file for help_word
    :exe ":help " . s:help_word
endfun 
map <Leader>h :call HelpWord()<CR>

"----------------------------------------------------------------------
"" 括号自动补齐
"----------------------------------------------------------------------
function! AutoPair(open, close)
    let line = getline('.')
    if col('.') > strlen(line) || line[col('.') - 1] == ' '
        return a:open.a:close."\<ESC>i"
    else
        return a:open
    endif
endf

function! ClosePair(char)
    if getline('.')[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endf

function! SamePair(char)
    let line = getline('.')
    if col('.') > strlen(line) || line[col('.') - 1] == ' '
        return a:char.a:char."\<ESC>i"
    elseif line[col('.') - 1] == a:char
        return "\<Right>"
    else
        return a:char
    endif
endf

"按Tab跳出括号和引号
function SkipPair()  
    if getline('.')[col('.') - 1] == ')' || getline('.')[col('.') - 1] == ']' || getline('.')[col('.') - 1] == '"' || getline('.')[col('.') - 1] == "'"
        return "\<ESC>la"  
    else  
        return "\t"  
    endif  
endf

inoremap ( <c-r>=AutoPair('(', ')')<CR>
inoremap ) <c-r>=ClosePair(')')<CR>
inoremap { <c-r>=AutoPair('{', '}')<CR>
inoremap } <c-r>=ClosePair('}')<CR>
inoremap [ <c-r>=AutoPair('[', ']')<CR>
inoremap ] <c-r>=ClosePair(']')<CR>
"inoremap " <c-r>=SamePair('"')<CR>
"inoremap ' <c-r>=SamePair("'")<CR>
"inoremap ` <c-r>=SamePair('`')<CR>
inoremap <C-f> <c-r>=SkipPair()<CR>

"----------------------------------------------------------------------------
" Quickly exiting help files
au BufRead *.txt,*.cnx      if &buftype=='help'|nmap <buffer> q <C-W>c|endif

" disable <CR> call in QuickFix buffer
autocmd FileType qf nnoremap <buffer> <CR> <CR>
autocmd CmdwinEnter : nnoremap <buffer> <CR> <CR> 
autocmd FileType qf execute 'nmap <silent><buffer> q :cclose<CR>'

"--------------------------------------------------------------------------
" package configuration
"--------------------------------------------------------------------------
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle

if executable('ag')
    let g:ackprg = 'ag -i'
endif

let g:deoplete#enable_at_startup = 1

"----------------------------------------------------------------------
"" Tagbar
"----------------------------------------------------------------------
let g:tagbar_width = 30
let g:tagbar_iconchars = ['+', '-']
" add support for markdown
let g:tagbar_type_markdown = {
    \ 'ctagstype': 'markdown',
    \ 'ctagsbin' : '~/.vim/plugin/markdown2ctags.py',
    \ 'ctagsargs' : [
      \'-f',
      \'-',
      \'--sort=yes',
      \],
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
    \ }
nnoremap <silent> <Leader>t :TagbarToggle<cr>

""------------------------------------------------------------------
" LeaderF 
"------------------------------------------------------------------ 
"popup mode
"let g:Lf_WindowPosition = 'popup'
"let g:Lf_PreviewInPopup = 1
"let g:Lf_PreviewResult = {'Function': 0, 'BufTag': 0 }
" 自下而上查找
let g:Lf_ReverseOrder = 1
let g:Lf_StlSeparator = { 'left': '', 'right': '' }

let g:Lf_ShortcutF = "<leader>ff"
let g:Lf_ShortcutB = "<leader>b"
noremap <leader>fr :<C-U><C-R>=printf("Leaderf mru %s", "")<CR><CR>
noremap <leader>ft :<C-U><C-R>=printf("Leaderf bufTag %s", "")<CR><CR>
noremap <leader>fl :<C-U><C-R>=printf("Leaderf line %s", "")<CR><CR>
noremap <leader>fm :LeaderfMarks<CR>

noremap <leader>rb :<C-U><C-R>=printf("Leaderf! rg --current-buffer -e %s ", expand("<cword>"))<CR>
if(!has('nvim'))
noremap <leader>rg :<C-U><C-R>=printf("Leaderf! rg --path-separator='//' -e %s ", expand("<cword>"))<CR>
else
noremap <leader>rg :<C-U><C-R>=printf("Leaderf! rg -e %s ", expand("<cword>"))<CR>
endif

" search visually selected text literally
"xnoremap <leader>rf :<C-U><C-R>=printf("Leaderf! rg -F -e %s ", leaderf#Rg#visual())<CR>
"noremap go :<C-U>Leaderf! rg --recall<CR>

" should use `Leaderf gtags --update` first
"let g:Lf_GtagsAutoGenerate = 0
"let g:Lf_Gtagslabel = 'native-pygments'
"noremap <leader>fr :<C-U><C-R>=printf("Leaderf! gtags -r %s --auto-jump", expand("<cword>"))<CR><CR>
"noremap <leader>fd :<C-U><C-R>=printf("Leaderf! gtags -d %s --auto-jump", expand("<cword>"))<CR><CR>
"noremap <leader>fo :<C-U><C-R>=printf("Leaderf! gtags --recall %s", "")<CR><CR>
"noremap <leader>fn :<C-U><C-R>=printf("Leaderf gtags --next %s", "")<CR><CR>
"noremap <leader>fp :<C-U><C-R>=printf("Leaderf gtags --previous %s", "")<CR><CR>

"------------------------------------------------------------------
" Statusline 
"------------------------------------------------------------------ 
"Always show the statusline
set laststatus=2
let g:statusline_max_path = 20
fun! StatusLineGetPath() "{{{
    let p = expand('%:.:h') "relative to current path, and head path only
    let p = substitute(p,'\','/','g')
    let p = substitute(p, '^\V' . $HOME, '~', '')
    if len(p) > g:statusline_max_path 
        let p = simplify(p)
        let p = pathshorten(p)
    endif
    return p . '/'
endfunction "}}}

function! CurDir()
    let curdir = substitute(getcwd(), $VIM, "~/", "g")
    if len(curdir) > g:statusline_max_path
        let curdir = simplify(curdir)
        let curdir = pathshorten(curdir)
    endif
    
    return curdir . '/'
endfunction

let g:lightline = {
            \ 'active' : {
            \   'left': [ [ 'mode', 'paste' ],
            \             [ 'cpath', 'rpath' ],
            \             [ 'readonly', 'filename', 'modified' ] ],
            \   'right': [ [ 'lineinfo', 'percent', 'totalline' ],
            \              [ 'fileformat', 'fileencoding', 'filetype' ] ]
            \   },
            \ 'component' : {
            \   'totalline': '%L'
            \  },
            \ 'component_function': {
            \   'rpath': 'StatusLineGetPath',
            \   'cpath': 'CurDir'
            \ },
            \ 'inactive' : {
		    \   'left': [ [ 'filename' ] ],
		    \   'right': [ [ 'lineinfo', 'percent' ] ]
            \ },
            \ }

"-------------------------------------------------------------------------
" NERDTree
"-------------------------------------------------------------------------
lmap <leader>w :NERDTreeToggle<CR>
""当NERDTree为剩下的唯一窗口时自动关闭
autocmd bufenter *
            \ if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
""修改树的显示图标
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
let NERDTreeAutoCenter = 1
"let NERDTreeDirArrows = 0 " 字体无法显示箭头时使用

" 显示行号
" let NERDTreeShowLineNumbers=1
" " 是否显示隐藏文件
let NERDTreeShowHidden=1
" " 设置宽度
let NERDTreeWinSize=25
" " 在终端启动vim时，共享NERDTree
" let g:nerdtree_tabs_open_on_console_startup=1
" " 忽略一下文件的显示
let NERDTreeIgnore=['\.pyc','\~$','\.swp']

"-------------------------------------------------------------------------
" expand_region
"-------------------------------------------------------------------------
let g:expand_region_text_objects = {
      \ 'iw'  :0,
      \ 'iW'  :0,
      \ 'i"'  :0,
      \ 'i''' :0,
      \ 'i]'  :1,
      \ 'ib'  :1,
      \ 'iB'  :1,
      \ 'il'  :0,
      \ 'ip'  :0,
      \ 'ie'  :0,
      \ }

call expand_region#custom_text_objects({
      \ "\/\\n\\n\<CR>": 1,
      \ 'a]' :1,
      \ 'ab' :1,
      \ 'aB' :1,
      \ 'ii' :1,
      \ 'ai' :1,
      \ })

"---------------------------------------------------------------------------------
" gutentags.
"----------------------------------------------------------------------------------
" gutentags搜索工程目录的标志，碰到这些文件/目录名就停止向上一级目录递归 "
let g:gutentags_project_root = ['.root', '.svn', '.git', '.gitignore', '.project']

" 所生成的数据文件的名称 "
let g:gutentags_ctags_tagfile = 'TAGS'

" 将自动生成的 tags 文件全部放入 ~/.cache/tags 目录中，避免污染工程目录 "
let s:vim_tags = expand('~/.cache/tags')
let g:gutentags_cache_dir = s:vim_tags
" 检测 ~/.cache/tags 不存在就新建 "
if !isdirectory(s:vim_tags)
    silent! call mkdir(s:vim_tags, 'p')
endif

" gtags configuration.
"let $GTAGSLABEL = 'native-pygments'
let $GTAGSCONF = '~/.globalrc'

" 同时开启 ctags 和 gtags 支持：
let g:gutentags_modules = []
if executable('ctags')
    let g:gutentags_modules += ['ctags']
endif
" if executable('gtags-cscope') && executable('gtags')
"     let g:gutentags_modules += ['gtags_cscope']
" endif

" 配置 ctags 的参数 "
let g:gutentags_ctags_extra_args = ['--fields=+niazS', '--extra=+q']
let g:gutentags_ctags_extra_args += ['--c++-kinds=+pxI']
let g:gutentags_ctags_extra_args += ['--c-kinds=+px']

" 如果使用 universal ctags 需要增加下面一行，老的 Exuberant-ctags 不能加下一行
let g:gutentags_ctags_extra_args += ['--output-format=e-ctags']

" 禁用 gutentags 自动加载 gtags 数据库的行为
let g:gutentags_auto_add_gtags_cscope = 0
" change focus to quickfix window after search (optional).
" let g:gutentags_plus_switch = 1
" for debug
"let g:gutentags_define_advanced_commands = 1

" mappings
let g:gutentags_plus_nomap = 1
nmap <silent> <leader>gs <Plug>GscopeFindSymbol
nmap <silent> <leader>gg <Plug>GscopeFindDefinition
nmap <silent> <leader>gr <Plug>GscopeFindCallingFunc
nmap <silent> <leader>gt <Plug>GscopeFindText
nmap <silent> <leader>ge <Plug>GscopeFindEgrep
nmap <silent> <leader>gf <Plug>GscopeFindFile
nmap <silent> <leader>gi <Plug>GscopeFindInclude
nmap <silent> <leader>gd <Plug>GscopeFindCalledFunc
nmap <silent> <leader>ga <Plug>GscopeFindAssign
nmap <silent> <leader>gz <Plug>GscopeFindCtag
nmap <silent> <leader>gk :GscopeKill<cr>

" preview
autocmd FileType qf nnoremap <silent><buffer> p :PreviewQuickfix<cr>
autocmd FileType qf nnoremap <silent><buffer> q :PreviewClose<cr>

"----------------------------------------------------------------------
" vim-startify
"----------------------------------------------------------------------
autocmd User Startified setlocal cursorline

let g:startify_enable_special         = 1
let g:startify_files_number           = 10
let g:startify_relative_path          = 1
let g:startify_change_to_dir          = 1
let g:startify_session_dir = '~/.vim.d/session'
let g:startify_session_autoload       = 1
let g:startify_session_persistence    = 1
let g:startify_session_delete_buffers = 1

let g:startify_list_order = [
            \ ['   Most Recent Files:'],
            \ 'files',
            \ ['   MRU within this dir:'],
            \ 'dir',
            \ ['   Sessions:'],
            \ 'sessions',
            \ ['   Bookmarks:'],
            \ 'bookmarks',
            \ ]

let g:startify_skiplist = [
            \ 'COMMIT_EDITMSG',
            \ escape(fnamemodify(resolve($VIMRUNTIME), ':p'), '\') .'doc',
            \ 'doc',
            \ 'plugged/.*/doc',
            \ escape(fnamemodify($HOME, ':p'), '\') .'mysecret.txt',
            \ ]

let g:startify_bookmarks = [ {'v': '~/.vimrc'}, '~/.vim/plugged' ]

function! s:filter_center(lines) abort
        let longest_line   = max(map(copy(a:lines), 'len(v:val)'))
            let centered_lines = map(copy(a:lines),
                            \ 'repeat(" ", (&columns / 2) - (longest_line / 2)) . v:val')
                return centered_lines
            endfunction

            let g:startify_custom_header = s:filter_center([
                        \ '+------------------------------+',
                        \ '|    Welcome to LIHAO`s vim    |',
                        \ '|                              |',
                        \ '+------------------------------+',
                        \])

            let g:startify_custom_footer = s:filter_center([
                        \ '',
                        \ '--- END ---',
                        \ '',
                        \])

" CtrlP or NERDTree open a split in Startify!
autocmd User Startified setlocal buftype=
            
hi StartifyBracket ctermfg=240
hi StartifyFile    ctermfg=147
hi StartifyFooter  ctermfg=240
hi StartifyHeader  ctermfg=114
hi StartifyNumber  ctermfg=215
hi StartifyPath    ctermfg=245
hi StartifySlash   ctermfg=240
hi StartifySpecial ctermfg=240

"----------------------------------------------------------------------
"" calendar
"----------------------------------------------------------------------
let g:calendar_diary = "~/.vim/diary"  " 设置日记的存储路径
let g:calendar_focus_today = 1      " 光标在当天的日期上
"let g:calendar_mark = 'left-fit' "可以让*和数字可靠近
let g:calendar_mark = 'right' "上面设置后在昨天写日志，修改成right正常
let g:calendar_mruler = '一月,二月,三月,四月,五月,六月,七月,八月,九月,十月,冬月,腊月'
let g:calendar_wruler = '日 一 二 三 四 五 六'
let g:calendar_navi_label = '往前,今日,往后'
map <F8> :Calendar<cr>
" 快捷键，默认 <leader>cal,水平方向：<leader>caL
let g:calendar_weeknm = 1
let g:calendar_datetime = 'statusline'
let g:calendar_keys = { 'goto_next_year': '<C-Down>', 'goto_prev_year':'<C-Up>'}

"------------------------------------------------------------------
" SuperTab setting 
"------------------------------------------------------------------ 
let g:SuperTabMappingForward = '<s-tab>'
let g:SuperTabMappingBackward = '<tab>'

"------------------------------------------------------------------
" TranslateIt
"------------------------------------------------------------------ 
let g:TranslateIt_DicDir = "D:/Unix/dic/"

"-----------------------------------------------------------------
" Bookmarks.
"-----------------------------------------------------------------
" 禁用默认的按键绑定
let g:vbookmark_disableMapping = 1
" 使用 Visual Studio 书签的按键方式
nnoremap <silent> <leader>bb :VbookmarkToggle<CR>
nnoremap <silent> <leader>bn :VbookmarkNext<CR>
nnoremap <silent> <leader>bp :VbookmarkPrevious<CR>
nnoremap <silent> <leader>bc :VbookmarkClearAll<CR>

"----------------------------------------------------------------------------- 
"" {{{ KEY MAPPING 
"----------------------------------------------------------------------------- 
"设置切换Buffer快捷键"
nnoremap <C-tab> :bn<CR>
nnoremap <C-S-TAB> :bp<CR>

" 行内移动，一次移动一个自然行 
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <C-Down> <C-o>gj
inoremap <C-Up> <C-o>gk

"-----------------------------------------------------------------------------
" visual shifting (builtin-repeat)  
" 在visual模式下缩进 (无限可重复) 
"----------------------------------------------------------------------------- 
vnoremap < <gv 
vnoremap > >gv

" 输入模式下，在行中直接换行 
inoremap <M-CR> <esc>o

inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-f> <PageDown>
inoremap <C-b> <PageUp>

"----------------------------------------------------------------------------- 
" 删除结尾空格定义 
command! -nargs=0 TrimR :%s/\s\+$//g 
" delete ^M, file will be modified. 
command! -nargs=0 TrimM :%s/\r//g 

"----------------------------------------------------------------------
"自动跳转到粘贴文本的最后
"使用 ppppp 进行多行多次粘贴操作
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" 进入当前目录为工作目录
map <F10> :lcd %:p:h<CR>

"------------------------------------------------------------------
" TabMessage: Put output of ex commands in a new tab/scrach buffer. 
"------------------------------------------------------------------ 
function! Tmsg(cmd)
    redir => message
    silent execute a:cmd
    redir END
    "tabnew
    Sscratch
    silent put=message
    set nomodified
endfunction

command! -nargs=+ -complete=command Tmsg call Tmsg(<q-args>)

" }}}

"------------------------------------------------------------------------------
" Line base motion. copy from unimpaired.vim.
"------------------------------------------------------------------------------
" Line operations {{{1
function! s:BlankUp(count) abort
    put!=repeat(nr2char(10), a:count)
    ']+1
    silent! call repeat#set("\<Plug>unimpairedBlankUp", a:count)
endfunction

function! s:BlankDown(count) abort
    put =repeat(nr2char(10), a:count)
    '[-1
    silent! call repeat#set("\<Plug>unimpairedBlankDown", a:count)
endfunction

nnoremap <silent> <Plug>unimpairedBlankUp   :<C-U>call <SID>BlankUp(v:count1)<CR>
nnoremap <silent> <Plug>unimpairedBlankDown :<C-U>call <SID>BlankDown(v:count1)<CR>

nmap [<Space> <Plug>unimpairedBlankUp
nmap ]<Space> <Plug>unimpairedBlankDown

function! s:Move(cmd, count, map) abort
    normal! m`
    silent! exe 'move'.a:cmd.a:count
    norm! ``
    silent! call repeat#set("\<Plug>unimpairedMove".a:map, a:count)
endfunction

function! s:MoveSelectionUp(count) abort
    normal! m`
    silent! exe "'<,'>move'<--".a:count
    norm! ``
    silent! call repeat#set("\<Plug>unimpairedMoveSelectionUp", a:count)
endfunction

function! s:MoveSelectionDown(count) abort
    normal! m`
    exe "'<,'>move'>+".a:count
    norm! ``
    silent! call repeat#set("\<Plug>unimpairedMoveSelectionDown", a:count)
endfunction

nnoremap <silent> <Plug>unimpairedMoveUp            :<C-U>call <SID>Move('--',v:count1,'Up')<CR>
nnoremap <silent> <Plug>unimpairedMoveDown          :<C-U>call <SID>Move('+',v:count1,'Down')<CR>
noremap  <silent> <Plug>unimpairedMoveSelectionUp   :<C-U>call <SID>MoveSelectionUp(v:count1)<CR>
noremap  <silent> <Plug>unimpairedMoveSelectionDown :<C-U>call <SID>MoveSelectionDown(v:count1)<CR>

nmap <M-up> <Plug>unimpairedMoveUp
nmap <M-down> <Plug>unimpairedMoveDown
xmap <M-up> <Plug>unimpairedMoveSelectionUp
xmap <M-down> <Plug>unimpairedMoveSelectionDown
" }}}1


function! FzyCommand(choice_command, vim_command)
  try
    let output = system(a:choice_command . " | fzy ")
  catch /Vim:Interrupt/
    " Swallow errors from ^C, allow redraw! below
  endtry
  redraw!
  if v:shell_error == 0 && !empty(output)
    exec a:vim_command . ' ' . output
  endif
endfunction

"nnoremap <leader>e :call FzyCommand("find . -type f", ":e")<cr>
"nnoremap <leader>v :call FzyCommand("find . -type f", ":vs")<cr>
"nnoremap <leader>s :call FzyCommand("find . -type f", ":sp")<cr>o

nnoremap <leader>e :call FzyCommand("ag . --silent -l -g ''", ":e")<cr>
"nnoremap <leader>v :call FzyCommand("ag . --silent -l -g ''", ":vs")<cr>
"nnoremap <leader>s :call FzyCommand("ag . --silent -l -g ''", ":sp")<cr>

"----------------------------------------------------------------------------------
" " 字符数
" " :%s/./&/gn
" " 单词数
" " :%s/\i\+/&/gn
" " 行数
" " :%s/^//n
" " 任何地方出现的 "the"
" " :%s/the/&/gn
" " 作为单词出现的 "the"
" " :%s/\<the\>/&/gn
" " 列出匹配the的行
" " :g/the/p
"----------------------------------------------------------------------------------------

" vim:fdm=marker et ts=4 sw=4
"
