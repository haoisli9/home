" File: translateit.vim
" Author: Taras Ivashchenko <naplanetu@gmail.com> 
" Updated: Lihao
" Version: 2.0
" License: GPL
"
" Description: 
" This script looks up a word under cursor in a dictionary using custom utility 
" such as sdcv (console version of StarDict program)
"
" User-provided mappings can be used instead by mapping to <Plug>CommandName, for instance:
"
" nmap ,d <Plug>TranslateIt
"
" The default mappings are as follow:
"
"   <Leader>d TranslateIt
"
" Several variables are checked by the script to determine behavior as follow:
"
" TranslateIt_Bin
"   Path to the dictionary binary utility
"
" Installation:
" Put this file into your $HOME/.vim/plugin directory.

if !exists('TranslateIt_Bin')
	let g:TranslateIt_Bin = "sdcv"
endif

if !exists('TranslateIt_DicDir')
	let g:TranslateIt_DicDir = "c:/stardict/sdcv/"
endif

" Buffer Title for buffer listing
let s:TranslateIt_BufName = "--Sdcv_DicBuffer--"

function! s:TranslateWord()
  let txt = input("Enter Word: ")
  call s:TranslateItQ(txt)
endfunction

function! s:TranslateIt()
  let txt = expand("<cword>")
  call s:TranslateItQ(txt)
endfunction

" Section: Utility functions
function! s:TranslateItQ(word_txt)
    if a:word_txt == ''
       let s:phrase = expand("<cword>")
    else
       let s:phrase = a:word_txt
    endif

    " Create a new buffer or Reopen the idle window
    exe "silent! " . "pedit " . s:TranslateIt_BufName

    " Move to it
    silent! wincmd P
    if &previewwindow
        " First make it modifiable
        setlocal modifiable
        setlocal nobuflisted
        setlocal buftype=nofile
	    setlocal wrap	

        silent execute "r !" . g:TranslateIt_Bin . " " . " --data-dir " . g:TranslateIt_DicDir . " -n " . shellescape(s:phrase)
       
        " set up some small syntax highlighting for dictionary window
        syn clear
        syn match sdcvKeyword "^-->.*$"
        syn match sdcvComment "^<<[^>]*>>$"
        syn match sdcvString "^\[.*\]$"
        syn match sdcvNumber "^\d"
        hi link sdcvKeyword	Keyword
        hi link sdcvComment Comment
        hi link sdcvString	String
        hi link sdcvNumber  PreProc

		nnoremap <buffer> <silent> q :pclose!<CR>

        setlocal nomodifiable

		normal gg
        " Put it on the bottom of (G)Vim
        silent! wincmd J

    endif
    " Go back to the privious window
    ""silent! wincmd p

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" old version
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
   " let s:phrase  = expand("<cword>")
	"let s:tmpfile = tempname()
  
	"silent execute "!" . g:TranslateIt_Bin . " " . " --data-dir " . g:TranslateIt_DicDir . " -n " . shellescape(s:phrase) . " > " . s:tmpfile
	"let s:lines = system("wc -l " . s:tmpfile) 

	"if s:lines == 0
		"echo s:phrase . ": Not found."
	"else
        "" Reopen the Source Explorer idle window
        "exe "silent! " . "pedit " . s:tmpfile
	"end
endfun

" Section: Command definitions 
command! TranslateIt call s:TranslateIt()
command! TranslateWord call s:TranslateWord()

" Section: Plugin command mappings
nnoremap <silent> <Plug>TranslateIt :TranslateIt<CR>

" Section: Default mappings
if !hasmapto('<Plug>TranslateIt')
	nmap <unique> <Leader>d <Plug>TranslateIt
endif

