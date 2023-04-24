vim.g.mapleader = " "
vim.g.maplocalleader = ","

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
	{
		dir = "~/projects/fenpoon/",
		dev = true
	}, -- own harpoon
	-- "grierson/fenpoon", -- Marks

	"nvim-lua/plenary.nvim",  -- Lots of packages use as dep
	"echasnovski/mini.nvim",  -- comments, pair, surround, statusline, leap
	"debugloop/telescope-undo.nvim", -- Undo
	"folke/which-key.nvim",   -- Keymap
	"AckslD/nvim-neoclip.lua", -- Clipboard

	"folke/todo-comments.nvim", -- TODO: comments
	"tpope/vim-abolish",      -- Subvert (Search and replace)

	-- Clojure
	"Olical/conjure",                      -- REPL
	"guns/vim-sexp",                       -- Add form and element text objects
	"tpope/vim-sexp-mappings-for-regular-people", -- Better sexp
	"Olical/aniseed",                      -- Fennel
	"folke/neodev.nvim",                   -- Plugin dev

	-- Git
	"tpope/vim-fugitive", -- Git manager
	"lewis6991/gitsigns.nvim", -- Git gutter + hunks

	-- Markdown
	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = ":call mkdp#util#install()",
	},

	-- Search
	"nvim-telescope/telescope.nvim",
	{
		"nvim-telescope/telescope-fzf-native.nvim", -- Better search
		build = "make",
		lazy = false
	},

	-- Project tree
	{
		"nvim-neo-tree/neo-tree.nvim",
		dependencies = {
			"MunifTanjim/nui.nvim",
			"nvim-tree/nvim-web-devicons",
		}
	},

	-- Colorscheme
	"p00f/alabaster.nvim", -- Theme
	"HiPhish/nvim-ts-rainbow2", -- Rainbow parens 2
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
	}, -- Better highlighting

	-- LSP + Autocomplete
	"jose-elias-alvarez/null-ls.nvim",
	"PaterJason/cmp-conjure",
	{
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		dependencies = {
			-- LSP Support
			{ 'neovim/nvim-lspconfig' },
			{
				'williamboman/mason.nvim',
				build = function()
					pcall(vim.cmd, 'MasonUpdate')
				end,
			},
			{ 'williamboman/mason-lspconfig.nvim' },

			-- Autocompletion
			{ 'hrsh7th/nvim-cmp' },
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'L3MON4D3/LuaSnip' },

			-- Progress bar
			{ "j-hui/fidget.nvim" },
		}
	},
})

vim.cmd [[colorscheme alabaster]]

-- Options
require('mini.basics').setup()
vim.opt.clipboard = "unnamedplus"
vim.opt.relativenumber = true
vim.opt.colorcolumn = "80"
vim.g.sexp_filetypes = "clojure,fennel"

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
require('mini.move').setup({
	mappings = {
		up = '<S-up>',
		down = '<S-down>'
	}
}) -- Move code
require('mini.statusline').setup({
	set_vim_settings = false
}) -- Status line
vim.opt.laststatus = 3

require("neo-tree").setup()      -- Project tree
require("todo-comments").setup() -- Highlight TODO: comments
require("fidget").setup()        -- Progress bar
require('neoclip').setup()       -- Easy register
require('gitsigns').setup()      -- Git
require("neodev").setup()        -- Plugin dev

-- LSP + Complete
local lsp = require('lsp-zero').preset({})

lsp.ensure_installed({
	"lua_ls",
	"clojure_lsp",
	"dockerls",
	"docker_compose_language_service",
	"terraformls",
	"jsonls",
	"marksman",
})

lsp.on_attach(function(_, bufnr)
	local opts = { buffer = bufnr, remap = false }

	vim.keymap.set("n", "gd", function() vim.lsp.buf.definition() end, opts)
	vim.keymap.set("n", "K", function() vim.lsp.buf.hover() end, opts)
	vim.keymap.set("n", "[d", function() vim.diagnostic.goto_next() end, opts)
	vim.keymap.set("n", "]d", function() vim.diagnostic.goto_prev() end, opts)
end)

lsp.setup()

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

local null_ls = require("null-ls")

null_ls.setup({
	sources = {
		null_ls.builtins.formatting.fnlfmt,
		null_ls.builtins.formatting.markdownlint,
		null_ls.builtins.diagnostics.markdownlint,
	},
})

-- Rainbow parens
require("nvim-treesitter.configs").setup({
	ensure_installed = {
		"help",
		"lua",
		"clojure",
		"fennel",
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
	},
})

local telescope = require("telescope")
local actions = require("telescope.actions")
telescope.setup({
	-- Use smart send instead
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
	}
})
telescope.load_extension('fzf')
telescope.load_extension('neoclip')
-- telescope.load_extension('harpoon')
telescope.load_extension('fenpoon')
telescope.load_extension("undo")

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

local whichKey = require("which-key")
whichKey.register({
	["["] = {
		name = "+prev",
		h = { "<cmd>Gitsigns prev_hunk<cr>", "Hunk" },
	},
	["]"] = {
		name = "+prev",
		h = { "<cmd>Gitsigns next_hunk<cr>", "Hunk" },
	},
	["<leader>"] = {
		w = {
			name = "+workspace",
			f = { "<cmd>NeoTreeReveal<cr>", "File" },
			s = { "<cmd>Telescope lsp_workspace_symbols symbols=function,variable<cr>", "Symbol" },
		},
		s = {
			name = "+search",
			f = { "<cmd>Telescope find_files<cr>", "File" },
			h = { "<cmd>Telescope help_tags<cr>", "Help" },
			w = { "<cmd>Telescope grep_string<cr>", "Word" },
			g = { "<cmd>Telescope live_grep<cr>", "Grep" },
			d = { "<cmd>Telescope diagnostic<cr>", "Diagnostics" },
			n = { "<cmd>TodoTelescope<cr>", "Note" },
			r = { "<cmd>Telescope resume<cr>", "Resume" },
			q = { "<cmd>Telescope quickfix<cr>", "Resume" },
		},
		l = {
			name = "+LSP",
			a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Action" },
			f = { "<cmd>lua vim.lsp.buf.format()<cr>", "Format" },
			r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
			s = { "<cmd>Telescope lsp_document_symbols symbols=function,variable<cr>", "Symbol" },
			d = { "<cmd>lua vim.diagnostic.setqflist()<cr>", "Diagnostic" },
		},
		h = {
			name = "+hunk",
			s = { "<cmd>Gitsigns stage_hunk<cr>", "Stage" },
			r = { "<cmd>Gitsigns reset_hunk<cr>", "Reset" },
			p = { "<cmd>Gitsigns preview_hunk<cr>", "Preview" },
			q = { "<cmd>Gitsigns setqflist<cr>", "Quickfix" },
		},
		-- Buffers
		b = { "<cmd>Telescope buffers theme=dropdown ignore_current_buffer=true previewer=false<cr>", "Buffer" },
		-- Clipboard
		c = { "<cmd>Telescope neoclip<cr>", "Clipboard" },
		-- Project tree
		t = { "<cmd>NeoTreeFocus<cr>", "Focus tree" },
		T = { "<cmd>NeoTreeShowToggle<cr>", "Toggle tree" },
		-- Undo
		u = { "<cmd>Telescope undo<cr>", "Undo" },
		-- Quickfix
		q = { "<cmd>:copen<cr>", "Focus quickfix" },
		Q = { "<cmd>:cclose<cr>", "Toggle quickfix" },
		-- Marks (Fenpoon)
		m = { "<cmd>Telescope fenpoon<cr>", "Marks" },
		M = { "<cmd>:lua require('fenpoon.api').mark()<cr>", "Mark file" },
	}
})
