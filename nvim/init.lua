vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.cmd.filetype("on")
vim.cmd.filetype("plugin on")
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Line numbers
vim.wo.number = true
vim.wo.relativenumber = true

-- Package manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	-- Own vim-sexp
	{
		dir = "~/projects/formedit/",
		dev = true,
		ft = "clojure",
		dependencies = {
			'nvim-treesitter/nvim-treesitter',
		},
	},
	-- Own Harpoon
	{
		dir = "~/projects/fenpoon/",
		dev = true,
	},

	-- Theme
	"p00f/alabaster.nvim",      -- Theme
	'nvim-treesitter/nvim-treesitter', -- AST
	'nvim-treesitter/playground', -- View Treesitter AST
	"HiPhish/nvim-ts-rainbow2", -- Rainbow parens

	-- Git manager
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
			"sindrets/diffview.nvim",
		},
		config = true
	},
	"lewis6991/gitsigns.nvim", -- Git gutter + hunks

	-- Search
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-telescope/telescope-live-grep-args.nvim"
		}
	},
	{
		"nvim-telescope/telescope-fzf-native.nvim", -- Sorting
		build = "make",
		lazy = false
	},

	-- Project tree
	{
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
		}
	},

	-- LSP + Autocomplete
	"PaterJason/cmp-conjure",
	{
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		dependencies = {
			-- LSP Support
			{ 'neovim/nvim-lspconfig' },
			{ 'williamboman/mason.nvim' },
			{ 'williamboman/mason-lspconfig.nvim' },

			-- Autocompletion
			{ 'hrsh7th/nvim-cmp' },
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'L3MON4D3/LuaSnip' },
		}
	},


	-- Basics
	"echasnovski/mini.nvim", -- comments, pair, surround, statusline, leap, whichKey
	"tpope/vim-sleuth", -- Indent

	-- Lisp
	"Olical/conjure",           -- REPL
	{ "Olical/nfnl", ft = "fennel" }, -- Fennel development
	"jose-elias-alvarez/null-ls.nvim", -- fnlfmt, mdfmt

	-- Misc
	"NoahTheDuke/vim-just", -- Build tool
	"folke/todo-comments.nvim", -- TODO: comments
	"nvim-lua/plenary.nvim", -- Lots of packages use as dep

	-- Progress bar
	{
		"j-hui/fidget.nvim",
		tag = "legacy",
		event = "LspAttach",
	},

	-- Markdown
	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = ":call mkdp#util#install()",
	},
})

vim.o.termguicolors = true
vim.o.background = "light"
vim.cmd [[colorscheme alabaster]]

-- Options
require('mini.basics').setup()
vim.opt.clipboard = "unnamedplus"
vim.opt.relativenumber = true
vim.opt.colorcolumn = "80"

-- Plugins
require('mini.trailspace').setup() -- Trailing space
require('mini.comment').setup()    -- Comments - gcc
require('mini.pairs').setup()      -- Auto close
require('mini.surround').setup()   -- add/change/delete surround
require('mini.cursorword').setup() -- Highlight current cursorword
require('mini.jump2d').setup()     -- Quick jump anywhere - <CR> <follow letters>
require('mini.bracketed').setup()  -- Bracket movement
require('mini.splitjoin').setup()  -- gS split or join args
require('mini.starter').setup()    -- Starter screen
require('mini.sessions').setup()   -- Sessions
-- Move code
require('mini.move').setup({
	mappings = {
		up = '<S-up>',
		down = '<S-down>'
	}
})

-- Status line
require('mini.statusline').setup({
	set_vim_settings = false
})
vim.opt.laststatus = 3

require("neo-tree").setup()      -- File tree
require("todo-comments").setup() -- Highlight TODO: comments
require("fidget").setup()        -- Progress bar
require("neogit").setup({})      -- Git manager
require("gitsigns").setup({})    -- Git gutter

-- LSP
local null_ls = require("null-ls")

null_ls.setup({
	sources = {
		null_ls.builtins.formatting.fnlfmt,
		null_ls.builtins.formatting.prettier,
		null_ls.builtins.diagnostics.markdownlint
	},
})

local lsp = require('lsp-zero').preset({})

lsp.on_attach(function(_, bufnr)
	lsp.default_keymaps({
		buffer = bufnr,
		preserve_mappings = false
	})
	vim.keymap.set('n', 'gr', '<cmd>Telescope lsp_references<cr>', { buffer = true })
end)

lsp.ensure_installed({
	"lua_ls",
	"fennel_language_server",
	"clojure_lsp",
	"clangd",
	"dockerls",
	"docker_compose_language_service",
	"terraformls",
	"jsonls",
	"marksman",
})

require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())
require('lspconfig').fennel_language_server.setup({
	settings = {
		fennel = {
			diagnostics = {
				globals = { 'vim' },
			},
		},
	},
})

lsp.setup()

-- Completion
local cmp = require("cmp")
require("luasnip.loaders.from_vscode").lazy_load()

cmp.setup({
	sources = {
		{ name = "path" },
		{ name = "nvim_lsp" },
		{ name = 'conjure' },
		{ name = "buffer",  keyword_length = 3 },
		{ name = "luasnip", keyword_length = 2 },
	},
	mapping = {
		['<CR>'] = cmp.mapping.confirm({ select = false }),
	}
})

-- Treesitter
require("nvim-treesitter.configs").setup({
	ensure_installed = {
		"help",
		"lua",
		"clojure",
		"fennel",
		"c",
		"yaml",
		"json",
		"terraform",
		"dockerfile",
		"markdown",
	},
	highlight = {
		enable = true,
	},
	indent = {
		enable = true,
	},
	rainbow = {
		enable = true,
		query = 'rainbow-parens',
		strategy = require 'ts-rainbow.strategy.global'
	},
	incremental_selection = {
		enable = true,
		keymaps = {
			node_incremental = '<TAB>',
			node_decremental = '<S-TAB>',
		},
	}
})

-- Telescope
local telescope = require("telescope")
local actions = require("telescope.actions")
telescope.setup({
	defaults = {
		mappings = {
			i = {
				["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
			},
			n = {
				["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
			}
		}
	},
	pickers = {
		buffers = {
			mappings = {
				i = {
					["<c-d>"] = "delete_buffer",
				}
			}
		}
	},
	extensions = {
		live_grep_args = {
			auto_quoting = true,
		}
	}
})
telescope.load_extension('fzf')
telescope.load_extension('live_grep_args')
telescope.load_extension('fenpoon')

-- Plugin dev
local ok, plenary_reload = pcall(require, "plenary.reload")
local reloader = require
if ok then
	reloader = plenary_reload.reload_module
end

P = function(v)
	print(vim.inspect(v))
	return v
end

RELOAD = function(...)
	return reloader(...)
end

R = function(name)
	RELOAD(name)
	return require(name)
end

-- Keymaps
local nmap_leader = function(suffix, rhs, desc)
	vim.keymap.set('n', '<Leader>' .. suffix, rhs, { desc = desc })
end

local formedit = require("formedit")
vim.keymap.set('o', 'af', formedit.select.form, { desc = "Outer form" })
vim.keymap.set('o', 'aF', formedit.select.root, { desc = "Outer root form" })
vim.keymap.set('o', 'if', formedit.select.inner, { desc = "Inner form" })
vim.keymap.set('o', 'iF', formedit.select["inner-root"], { desc = "Inner root form" })
vim.keymap.set('n', '<localleader>h', formedit.insertion.head, { desc = "Head insert" })
vim.keymap.set('n', '<localleader>H', formedit.insertion.tail, { desc = "Tail insert" })
vim.keymap.set('n', '<localleader>i', formedit.wrap.head, { desc = "Wrap form " })
vim.keymap.set('n', '<localleader>o', formedit.raise.form, { desc = "Raise form" })
vim.keymap.set('n', '<localleader>O', formedit.raise.element, { desc = "Raise element" })
vim.keymap.set('n', '>)', formedit.slurp.forward, { desc = "Slurp forward" })
vim.keymap.set('n', '<(', formedit.slurp.backward, { desc = "Slurp backward" })
vim.keymap.set('n', '>(', formedit.barf.backward, { desc = "Barf backward" })
vim.keymap.set('n', '<)', formedit.barf.forward, { desc = "Barf forward" })

-- Project Tree
nmap_leader('t', '<cmd>Neotree focus right<cr>', 'Focus tree')
nmap_leader('T', '<cmd>Neotree toggle right<cr>', 'Toggle tree')

-- Git
nmap_leader('g', '<cmd>Neogit<cr>', 'Git')

-- LSP
nmap_leader("la", "<cmd>lua vim.lsp.buf.code_action()<cr>", "Action")
nmap_leader("lf", "<cmd>lua vim.lsp.buf.format()<cr>", "Format")
nmap_leader("lr", "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename")
nmap_leader("ls", "<cmd>Telescope lsp_document_symbols symbols=function,variable<cr>", "Symbol")
nmap_leader("ld", "<cmd>Telescope diagnostics<cr>", "Diagnostic")

-- LSP workspace
nmap_leader("wf", "<cmd>Neotree reveal<cr>", "File")
nmap_leader("ws", "<cmd>Telescope lsp_workspace_symbols symbols=function,variable<cr>", "Symbol")

-- Search
nmap_leader("sf", "<cmd>Telescope find_files<cr>", "File")
nmap_leader("sh", "<cmd>Telescope help_tags<cr>", "Help")
nmap_leader("sw", "<cmd>Telescope grep_string<cr>", "Word")
nmap_leader("sg", ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>", "Grep")
nmap_leader("sd", "<cmd>Telescope diagnostic<cr>", "Diagnostics")
nmap_leader("sn", "<cmd>TodoTelescope<cr>", "Note")
nmap_leader("sr", "<cmd>Telescope resume<cr>", "Resume")
nmap_leader("sq", "<cmd>Telescope quickfix<cr>", "Quickfix")
nmap_leader("sc", "<cmd>Telescope commands<cr>", "Commands")

-- Git hunks
nmap_leader("hs", "<cmd>Gitsigns stage_hunk<cr>", "Stage")
nmap_leader("hr", "<cmd>Gitsigns reset_hunk<cr>", "Reset")
nmap_leader("hp", "<cmd>Gitsigns preview_hunk<cr>", "Preview")
vim.keymap.set('n', '[h', "<cmd>Gitsigns prev_hunk<cr>", { desc = "Hunk" })
vim.keymap.set('n', ']h', "<cmd>Gitsigns next_hunk<cr>", { desc = "Hunk" })

-- Buffers
nmap_leader("b", "<cmd>Telescope buffers theme=dropdown ignore_current_buffer=true previewer=false<cr>", "Buffer")

-- Quickfix
nmap_leader("q", "<cmd>:copen<cr>", "Focus quickfix")
nmap_leader("Q", "<cmd>:cclose<cr>", "Toggle quickfix")

-- Registers
nmap_leader("r", "<cmd>Telescope registers<cr>", "Registers")

-- Marks + Fenpoon
nmap_leader("n", "<cmd>Telescope fenpoon<cr>", "Harpoon")
nmap_leader("N", "<cmd>:lua require('fenpoon.api').mark()<cr>", "Harpoon file")
nmap_leader("m", "<cmd>Telescope marks<cr>", "Marks")

local miniclue = require('mini.clue')
miniclue.setup({
	triggers = {
		-- Leader triggers
		{ mode = 'n', keys = '<Leader>' },
		{ mode = 'x', keys = '<Leader>' },
		{ mode = 'n', keys = '<LocalLeader>' },

		-- Built-in completion
		{ mode = 'i', keys = '<C-x>' },

		-- `g` key
		{ mode = 'n', keys = 'g' },
		{ mode = 'x', keys = 'g' },

		-- Marks
		{ mode = 'n', keys = "'" },
		{ mode = 'n', keys = '`' },
		{ mode = 'x', keys = "'" },
		{ mode = 'x', keys = '`' },

		-- Registers
		{ mode = 'n', keys = '"' },
		{ mode = 'x', keys = '"' },
		{ mode = 'i', keys = '<C-r>' },
		{ mode = 'c', keys = '<C-r>' },

		-- Window commands
		{ mode = 'n', keys = '<C-w>' },

		-- `z` key
		{ mode = 'n', keys = 'z' },
		{ mode = 'x', keys = 'z' },

		-- `Bracketed` key
		{ mode = 'n', keys = '[' },
		{ mode = 'n', keys = ']' },
		{ mode = 'x', keys = '[' },
		{ mode = 'x', keys = ']' },
	},

	clues = {
		{ mode = 'n', keys = '<Leader>w', desc = '+Buffers' },
		{ mode = 'n', keys = '<Leader>s', desc = '+Search' },
		{ mode = 'n', keys = '<Leader>l', desc = '+LSP' },
		{ mode = 'n', keys = '<Leader>h', desc = '+Hunk' },
		-- Enhance this by adding descriptions for <Leader> mapping groups
		miniclue.gen_clues.builtin_completion(),
		miniclue.gen_clues.g(),
		miniclue.gen_clues.marks(),
		miniclue.gen_clues.registers(),
		miniclue.gen_clues.windows(),
		miniclue.gen_clues.z(),
	},
})
