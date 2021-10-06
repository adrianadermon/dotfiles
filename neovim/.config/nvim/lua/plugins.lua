return require('packer').startup(function()
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Color theme
    use 'andreypopp/vim-colors-plain'

    -- Start screen
    use 'mhinz/vim-startify'

    -- Change directory
    use 'airblade/vim-rooter'

    -- Additional mappings
    use 'tpope/vim-unimpaired'

    -- Comment
    use 'tpope/vim-commentary'

    -- Directory viewer
    use 'justinmk/vim-dirvish'

    -- Manipulate surroundings
    use 'machakann/vim-sandwich'
    -- use 'tpope/vim-surround'

    -- Language server protocol support
    -- use {
    --     'neovim/nvim-lspconfig',
    --     config = [[require('lspconfig').julials.setup{}]]
    -- }

    -- Git
    use {
        'TimUntersberger/neogit',
        requires = 'nvim-lua/plenary.nvim'
    }

    -- Git signs
    use {
        'lewis6991/gitsigns.nvim',
        requires = {'nvim-lua/plenary.nvim'},
        config = [[require('gitsigns').setup()]]
    }

    -- Fuzzy finder
    use {
        'nvim-telescope/telescope.nvim',
        requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
    }

    use {'norcalli/nvim-colorizer.lua',
    config = [[require('colorizer').setup()]]}

    -- Markdown and padoc support
    use 'vim-pandoc/vim-pandoc'
    use 'vim-pandoc/vim-pandoc-syntax'

    -- Julia support
    use 'JuliaEditorSupport/julia-vim'

    -- Communicate with R
    use 'jalvesaq/Nvim-R'


    -- Latex editing
    use 'lervag/vimtex'

    -- REPL
    use {
        'hkupty/iron.nvim',
        ft = {'julia'},
        config = {
            function()
                require('iron').core.add_repl_definitions {
                    julia = {
                        julia = {
                            command = {"julia", "--color=no"} },
                        }
                    }
                end
            }
        }

        -- Focus mode
        use {
            "folke/zen-mode.nvim",
            config = function()
                require("zen-mode").setup {
                    window = {
                        width = 80,
                        height = 0.85,
                        options = {
                            signcolumn = "no",
                            number = false,
                            relativenumber = false,
                        },
                    },
                }
            end
        }

        -- -- Simple plugins can be specified as strings
        -- use '9mm/vim-closer'

        -- -- Lazy loading:
        -- -- Load on specific commands
        -- use {'tpope/vim-dispatch', opt = true, cmd = {'Dispatch', 'Make', 'Focus', 'Start'}}

        -- -- Load on an autocommand event
        -- use {'andymass/vim-matchup', event = 'VimEnter'}

        -- -- Load on a combination of conditions: specific filetypes or commands
        -- -- Also run code after load (see the "config" key)
        -- use {
        --   'w0rp/ale',
        --   ft = {'sh', 'zsh', 'bash', 'c', 'cpp', 'cmake', 'html', 'markdown', 'racket', 'vim', 'tex'},
        --   cmd = 'ALEEnable',
        --   config = 'vim.cmd[[ALEEnable]]'
        -- }

        -- -- Plugins can have dependencies on other plugins
        -- use {
        --   'haorenW1025/completion-nvim',
        --   opt = true,
        --   requires = {{'hrsh7th/vim-vsnip', opt = true}, {'hrsh7th/vim-vsnip-integ', opt = true}}
        -- }

        -- -- Plugins can also depend on rocks from luarocks.org:
        -- use {
        --   'my/supercoolplugin',
        --   rocks = {'lpeg', {'lua-cjson', version = '2.1.0'}}
        -- }

        -- -- You can specify rocks in isolation
        -- use_rocks 'penlight'
        -- use_rocks {'lua-resty-http', 'lpeg'}

        -- -- Local plugins can be included
        -- use '~/projects/personal/hover.nvim'

        -- -- Plugins can have post-install/update hooks
        -- use {'iamcco/markdown-preview.nvim', run = 'cd app && yarn install', cmd = 'MarkdownPreview'}

        -- -- Post-install/update hook with neovim command
        -- use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

        -- -- Post-install/update hook with call of vimscript function with argument
        -- use { 'glacambre/firenvim', run = function() vim.fn['firenvim#install'](0) end }

        -- -- Use specific branch, dependency and run lua file after load
        -- use {
        --   'glepnir/galaxyline.nvim', branch = 'main', config = function() require'statusline' end,
        --   requires = {'kyazdani42/nvim-web-devicons'}
        -- }

        -- -- Use dependency and run lua function after load
        -- use {
        --   'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' },
        --   config = function() require('gitsigns').setup() end
        -- }

        -- -- You can specify multiple plugins in a single call
        -- use {'tjdevries/colorbuddy.vim', {'nvim-treesitter/nvim-treesitter', opt = true}}

        -- -- You can alias plugin names
        -- use {'dracula/vim', as = 'dracula'}
    end)
