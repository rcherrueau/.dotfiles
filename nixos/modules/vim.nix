# Custom vim
#
# For vim configuration see https://nixos.wiki/wiki/Vim and
# https://beyermatthias.de/blog/2015/11/25/how-to-setup-neovim-on-nixos/
# For vimrc see https://github.com/amix/vimrc
{ pkgs, lib, ... }:

let
  plugins = with pkgs.vimPlugins; {
    # Loaded on launch
		start = [ vim-nix nord-vim ];
    # Manually loadable by calling `:packadd $plugin-name`
		opt = [];
  };
  rc = ''
     " => General
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Set to auto read when a file is changed from the outside
     set autoread

     " :W sudo saves the file
     " (useful for handling the permission-denied error)
     command W w !sudo tee % > /dev/null

     " Yanked text accessible throuht system path and middle click
     set clipboard^=unnamed,unnamedplus

     " => VIM user interface
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Turn on the WiLd menu (for command line completion)
     set wildmenu

     " Ignore compiled files
     set wildignore=*.o,*~,*.pyc
     if has("win16") || has("win32")
         set wildignore+=.git\*,.hg\*,.svn\*
     else
         set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
     endif

     " Configure backspace so it acts as it should act
     set backspace=eol,start,indent

     " Ignore case when searching
     set ignorecase

     " When searching try to be smart about cases
     set smartcase

     " Highlight search results
     set hlsearch

     " Makes search act like search in modern browsers
     set incsearch

     " Don't redraw while executing macros (good performance config)
     set lazyredraw

     " For regular expressions turn magic on
     set magic

     " Show matching brackets when text indicator is over them
     set showmatch
     " How many tenths of a second to blink when matching brackets
     set mat=2

     " No annoying sound on errors
     set noerrorbells
     set novisualbell
     set t_vb=
     set tm=500

     " => Colors and Fonts
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Enable syntax highlighting
     syntax enable

     " Enable 256 colors palette
     set t_Co=256

     " Set utf8 as standard encoding and en_US as the standard language
     set encoding=utf8

     " Use Unix as the standard file type
     set ffs=unix,dos,mac

     " Use Nord color scheme
     colorscheme nord

     " => Files, backups and undo
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Turn backup off, since most stuff is in SVN, git et.c anyway...
     set nobackup
     set nowb
     set noswapfile

     " => Text, tab and indent related
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Use spaces instead of tabs
     set expandtab

     " Be smart when using tabs ;)
     set smarttab

     " 1 tab == 2 spaces
     set shiftwidth=2
     set tabstop=2

     " Linebreak on 500 characters
     set lbr
     set tw=500

     set ai "Auto indent
     set si "Smart indent
     set wrap "Wrap lines

     " => Visual mode related
     """"""""""""""""""""""""""""""
     " Visual mode pressing * searches for the current selection
     " Super useful! From an idea by Michael Naumann
     vnoremap <silent> * :<C-u>call VisualSelection(\'\', \'\')<CR>/<C-R>=@/<CR><CR>

     " Smart way to move between windows
     map <C-j> <C-W>j
     map <C-k> <C-W>k
     map <C-h> <C-W>h
     map <C-l> <C-W>l

     " Return to last edit position when opening files (You want this!)
     au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

     " => Status line
     """"""""""""""""""""""""""""""
     " Always show the status line
     set laststatus=2

     " Display line,columne number in the status line
     set ruler

     " => Editing mappings
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Delete trailing white space on save, useful for Python and CoffeeScript ;)
     func! DeleteTrailingWS()
       exe "normal mz"
       %s/\s\+$//ge
       exe "normal `z"
     endfunc
     autocmd BufWrite *.py :call DeleteTrailingWS()
     autocmd BufWrite *.coffee :call DeleteTrailingWS()

     " Trigger <Esc> will pressing fd quickly
     " See http://vim.wikia.com/wiki/Avoid_the_escape_key
     inoremap fd <Esc>

     " => Spell checking
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     set spelllang=en
     " Enable spell checking by default
     set spell

     " Underline spelling error
     hi clear SpellBad
     hi SpellBad cterm=underline

     " => Helper functions
     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     " Display CLRF (Ctrl+Q-L)
     function! FoldPageFeed()
       setl foldmethod=expr
       setl foldexpr=getline(v:lnum)[0]==\"\\<c-l>\"
       setl foldminlines=0
       setl foldtext='---\ new\ page\ '
       setl foldlevel=0
       set foldclose=all
     endfunction

     function! CmdLine(str)
         exe "menu Foo.Bar :" . a:str
         emenu Foo.Bar
         unmenu Foo
     endfunction

     function! VisualSelection(direction, extra_filter) range
         let l:saved_reg = @"
         execute "normal! vgvy"

         let l:pattern = escape(@", "\\/.*'$^~[]")
         let l:pattern = substitute(l:pattern, "\n$", "", "")

         if a:direction == 'gv'
             call CmdLine("Ag '" . l:pattern . "' " )
         elseif a:direction == 'replace'
             call CmdLine("%s" . '/'. l:pattern . '/')
         endif

         let @/ = l:pattern
         let @" = l:saved_reg
     endfunction

     " Returns true if paste mode is enabled
     function! HasPaste()
         if &paste
             return 'PASTE MODE  '
         endif
         return \'\'
     endfunction

     " Don't close window, when deleting a buffer
     command! Bclose call <SID>BufcloseCloseIt()
     function! <SID>BufcloseCloseIt()
        let l:currentBufNum = bufnr("%")
        let l:alternateBufNum = bufnr("#")

        if buflisted(l:alternateBufNum)
          buffer #
        else
          bnext
        endif

        if bufnr("%") == l:currentBufNum
          new
        endif

        if buflisted(l:currentBufNum)
          execute("bdelete! ".l:currentBufNum)
        endif
     endfunction

		'';
  vim-no-gui = lib.overrideDerivation
    # vim_configurable (i.e, build the custom vim derivation)
    # https://nixos.wiki/wiki/Vim
    (pkgs.vim_configurable.customize {
      name = "vim";
		  vimrcConfig.packages.myplugins = plugins;
      vimrcConfig.customRC = rc;
    })
    # Override derivation for what features should be compiled into
    # https://beyermatthias.de/blog/2015/11/25/how-to-setup-neovim-on-nixos/
    (oldAttrs: {
      # Disable GUI, see [0] for other options. Alternatively, one
      # could achieved it by removing `--enable-gui` configure flag:
      #
      # > configureFlags = lib.filter
      # >   (f: ! lib.hasPrefix "--enable-gui" f) oldAttrs.configureFlags;
      #
      # [0] https://github.com/NixOS/nixpkgs/blob/41307593381d295283ed51333743ba7cee9c07e4/pkgs/applications/editors/vim/configurable.nix#L15-L27
      guiSupport = false;
    });
in {
  environment.variables.EDITOR = "vim";
  environment.systemPackages = [ vim-no-gui ];
}
