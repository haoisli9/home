set nocp

let g:python3_host_prog='d:/cygwin64/bin/python3.8.exe'
let g:loaded_python_provider = 0

" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes
Plug 'majutsushi/tagbar'
Plug 'itchyny/lightline.vim'
Plug 'easymotion/vim-easymotion'
Plug 'preservim/nerdcommenter'
Plug 'ludovicchabant/vim-gutentags'
Plug 'skywind3000/gutentags_plus'
Plug 'skywind3000/vim-preview'
Plug 'luochen1990/rainbow'
Plug 'dkprice/vim-easygrep'
Plug 'haya14busa/incsearch.vim'
Plug 'google/vim-searchindex'
Plug 'tpope/vim-repeat' "��Ϊǿ�����������
Plug 'tpope/vim-surround' "�����Զ�����
Plug 'mhinz/vim-startify'
Plug 'tmhedberg/matchit' 
Plug 'mattn/calendar-vim'
Plug 'name5566/vim-bookmark'
Plug 'xuhdev/SingleCompile'
Plug 'Shougo/vimproc.vim', {'do' : 'make'}

Plug 'terryma/vim-expand-region'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-line'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-indent'

" color
Plug 'tomasr/molokai'
Plug 'liuchengxu/space-vim-dark'
Plug 'dracula/vim', {'as':'dracula'}
" Any valid git URL is allowed
"   Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
"   Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
"
" Plugin outside ~/.vim/plugged with post-update hook
"   Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'Yggdroot/LeaderF', { 'do': '.\install.bat' }
Plug 'Yggdroot/LeaderF-marks'
Plug 'Yggdroot/indentLine'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

"   Unmanaged plugin (manually installed and updated)
Plug '~/.vim/plugin/WhereFrom.vim'
Plug '~/.vim/plugin/SyntaxAttr.vim'
Plug '~/.vim/plugin/Translateit'

" Initialize plugin system
call plug#end()
"------------------------------------------------------------------
" Settings  
"------------------------------------------------------------------ 
" ϵͳ������
nmap <C-v> "+gp  

" �ն�֧��255ɫ
set t_Co=256

set cursorline

set fileencodings=utf-8,ucs-bom,cp936,gb18030,gb2312,big5,euc-jp,euc-kr,latin1
set termencoding=utf-8
set encoding=utf-8

set fileformats=unix,dos,mac " EOL formats(\r\n or \n or \r) in order

"set gfn=YaHei\ Consolas\ Hybrid:h16:cANSI
"set gfn=Cascadia\ Mono\ PL:h16:cANSI
set  gfn=Sarasa\ Mono\ SC:h16
"set gfw=����:h20:cGB2312

set background=dark
"colo torte
"colo space-vim-dark
colo dracula

" ������ʱ����ʾ�Ǹ�Ԯ���������ͯ����ʾ
set shortmess=atI
 
set viminfo=%,<50,'10,/50,:50,h,f0,n~/.vim/viminfo
"           | |    |   |   |    | |  + viminfo file path
"           | |    |   |   |    | + file marks 0-9,A-Z 0=NOT stored
"           | |    |   |   |    + disable 'hlsearch' loading viminfo
"           | |    |   |   + command-line history saved
"           | |    |   + search history saved
"           | |    + files marks saved
"           | + lines saved each register (old name for <, vi6.2)
"           + save/restore buffer listlet $LANG = 'en'

"------------------------------------------------------------------
" mouseʹ��Vģʽ��������Selectģʽ�£�����mswin���������� 
"------------------------------------------------------------------ 
set mouse=a
set selectmode=key

"------------------------------------------------------------------
" VIM user interface setting 
" No sound on errors.
set noerrorbells
set novisualbell
set t_vb=
set ignorecase
set backspace=indent,eol,start      "Set backspace
set whichwrap+=<,>,h,l              "����backspace�͹�����Խ�б߽�
set incsearch
set hlsearch

"------------------------------------------------------------------
"" �ı��༭���� 
"------------------------------------------------------------------ 
set sw=4       " �Զ�������ʱ�� �����ߴ�Ϊ 4 ���ո�
set ts=4       " Tab ���Ϊ 4 ���ַ� 
set sts=4      " softtabstop
set et         " �༭ʱ������ Tab �滻Ϊ�ո�
set lbr        " ���ڵ����м����
set nu         " Show line number.
set showmatch  " show matching bracets
set linespace=4

set autoindent                          " �����Զ�����
set smartindent                        " �Զ��ж��������ȣ�һ����������C����

"------------------------------------------------------------------
" ��ʹ��UTF-8ʱ�����������ַ���� 
"------------------------------------------------------------------ 
set ambiwidth=double

"------------------------------------------------------------------
" leader�趨 
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
" �ÿո񿪹��۵� <CR> --> <space>
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
"" diff ���Կո� 
"------------------------------------------------------------------ 
set diffopt=filler,iwhite

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
" ע��RangSearch����Ҫ��һ���ո�
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
"" �����Զ�����
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

"��Tab�������ź�����
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
inoremap ' <c-r>=SamePair("'")<CR>
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
" ���¶��ϲ���
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
xnoremap <leader>rf :<C-U><C-R>=printf("Leaderf! rg -F -e %s ", leaderf#Rg#visual())<CR>
noremap go :<C-U>Leaderf! rg --recall<CR>

" should use `Leaderf gtags --update` first
"let g:Lf_GtagsAutoGenerate = 0
"let g:Lf_Gtagslabel = 'native-pygments'
"noremap <leader>fr :<C-U><C-R>=printf("Leaderf! gtags -r %s --auto-jump", expand("<cword>"))<CR><CR>
"noremap <leader>fd :<C-U><C-R>=printf("Leaderf! gtags -d %s --auto-jump", expand("<cword>"))<CR><CR>
"noremap <leader>fo :<C-U><C-R>=printf("Leaderf! gtags --recall %s", "")<CR><CR>
"noremap <leader>fn :<C-U><C-R>=printf("Leaderf gtags --next %s", "")<CR><CR>
"noremap <leader>fp :<C-U><C-R>=printf("Leaderf gtags --previous %s", "")<CR><CR>

"-----------------------------------------------------------------------------
" indent lines.
"-----------------------------------------------------------------------------
let g:indent_guides_guide_size            = 1  " ָ�������ߵĳߴ�
let g:indent_guides_start_level           = 2  " �ӵڶ��㿪ʼ���ӻ���ʾ����

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
""��NERDTreeΪʣ�µ�Ψһ����ʱ�Զ��ر�
autocmd bufenter *
            \ if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
""�޸�������ʾͼ��
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
let NERDTreeAutoCenter = 1
"let NERDTreeDirArrows = 0 " �����޷���ʾ��ͷʱʹ��

" ��ʾ�к�
" let NERDTreeShowLineNumbers=1
" " �Ƿ���ʾ�����ļ�
let NERDTreeShowHidden=1
" " ���ÿ��
let NERDTreeWinSize=25
" " ���ն�����vimʱ������NERDTree
" let g:nerdtree_tabs_open_on_console_startup=1
" " ����һ���ļ�����ʾ
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
" gutentags��������Ŀ¼�ı�־��������Щ�ļ�/Ŀ¼����ֹͣ����һ��Ŀ¼�ݹ� "
let g:gutentags_project_root = ['.root', '.svn', '.git', '.gitignore', '.project']

" �����ɵ������ļ������� "
let g:gutentags_ctags_tagfile = 'TAGS'

" ���Զ����ɵ� tags �ļ�ȫ������ ~/.cache/tags Ŀ¼�У�������Ⱦ����Ŀ¼ "
let s:vim_tags = expand('~/.cache/tags')
let g:gutentags_cache_dir = s:vim_tags
" ��� ~/.cache/tags �����ھ��½� "
if !isdirectory(s:vim_tags)
    silent! call mkdir(s:vim_tags, 'p')
endif

" gtags configuration.
"let $GTAGSLABEL = 'native-pygments'
"let $GTAGSCONF = '~/.globalrc'

" ͬʱ���� ctags �� gtags ֧�֣�
let g:gutentags_modules = []
if executable('ctags')
    let g:gutentags_modules += ['ctags']
endif
"if executable('gtags-cscope') && executable('gtags')
    "let g:gutentags_modules += ['gtags_cscope']
"endif

" ���� ctags �Ĳ��� "
let g:gutentags_ctags_extra_args = ['--fields=+niazS', '--extra=+q']
let g:gutentags_ctags_extra_args += ['--c++-kinds=+pxI']
let g:gutentags_ctags_extra_args += ['--c-kinds=+px']

" ���ʹ�� universal ctags ��Ҫ��������һ�У��ϵ� Exuberant-ctags ���ܼ���һ��
let g:gutentags_ctags_extra_args += ['--output-format=e-ctags']

" ���� gutentags �Զ����� gtags ���ݿ����Ϊ
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
let g:startify_session_dir = '~/.vim/session'
let g:startify_session_autoload       = 0
let g:startify_session_persistence    = 0
let g:startify_session_delete_buffers = 1

let g:startify_list_order = [
            \ ['   Most Recent Files:'],
            \ 'files',
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

let g:startify_bookmarks = [ {'v': '~/init.vim'}, '~/.vim/plugged' ]

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
let g:calendar_diary = "~/.vim/diary"  " �����ռǵĴ洢·��
let g:calendar_focus_today = 1      " ����ڵ����������
"let g:calendar_mark = 'left-fit' "������*�����ֿɿ���
let g:calendar_mark = 'right' "�������ú�������д��־���޸ĳ�right����
let g:calendar_mruler = 'һ��,����,����,����,����,����,����,����,����,ʮ��,����,����'
let g:calendar_wruler = '�� һ �� �� �� �� ��'
let g:calendar_navi_label = '��ǰ,����,����'
map <F8> :Calendar<cr>
" ��ݼ���Ĭ�� <leader>cal,ˮƽ����<leader>caL
let g:calendar_weeknm = 1
let g:calendar_datetime = 'statusline'
let g:calendar_keys = { 'goto_next_year': '<C-Down>', 'goto_prev_year':'<C-Up>'}

"------------------------------------------------------------------
" TranslateIt
"------------------------------------------------------------------ 
let g:TranslateIt_DicDir = "D:/Unix/dic/"

"-----------------------------------------------------------------
" Bookmarks.
"-----------------------------------------------------------------
" ����Ĭ�ϵİ�����
let g:vbookmark_disableMapping = 1
" ʹ�� Visual Studio ��ǩ�İ�����ʽ
nnoremap <silent> <leader>bb :VbookmarkToggle<CR>
nnoremap <silent> <leader>bn :VbookmarkNext<CR>
nnoremap <silent> <leader>bp :VbookmarkPrevious<CR>
nnoremap <silent> <leader>bc :VbookmarkClearAll<CR>

"------------------------------------------------------------
" coc configure.
"------------------------------------------------------------
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=1

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

"----------------------------------------------------------------------------- 
"" {{{ KEY MAPPING 
"----------------------------------------------------------------------------- 
"�����л�Buffer��ݼ�"
nnoremap <C-tab> :bn<CR>
nnoremap <C-S-TAB> :bp<CR>

" �����ƶ���һ���ƶ�һ����Ȼ�� 
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <C-Down> <C-o>gj
inoremap <C-Up> <C-o>gk

"-----------------------------------------------------------------------------
" visual shifting (builtin-repeat)  
" ��visualģʽ������ (���޿��ظ�) 
"----------------------------------------------------------------------------- 
vnoremap < <gv 
vnoremap > >gv

" ����ģʽ�£�������ֱ�ӻ��� 
inoremap <M-CR> <esc>o

inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-f> <PageDown>
inoremap <C-b> <PageUp>

"----------------------------------------------------------------------------- 
" ɾ����β�ո��� 
command! -nargs=0 TrimR :%s/\s\+$//g 
" delete ^M, file will be modified. 
command! -nargs=0 TrimM :%s/\r//g 

"----------------------------------------------------------------------
"�Զ���ת��ճ���ı������
"ʹ�� ppppp ���ж��ж��ճ������
vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

" ���뵱ǰĿ¼Ϊ����Ŀ¼
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
" " �ַ���
" " :%s/./&/gn
" " ������
" " :%s/\i\+/&/gn
" " ����
" " :%s/^//n
" " �κεط����ֵ� "the"
" " :%s/the/&/gn
" " ��Ϊ���ʳ��ֵ� "the"
" " :%s/\<the\>/&/gn
" " �г�ƥ��the����
" " :g/the/p
"----------------------------------------------------------------------------------------

" vim:fdm=marker et ts=4 sw=4
"
