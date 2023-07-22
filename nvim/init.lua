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
		dev = true,
	}, -- own harpoon
	-- "grierson/fenpoon", -- Marks

	"nvim-lua/plenary.nvim", -- Lots of packages use as dep
	{ "Olical/nfnl", ft = "fennel" },
	"echasnovski/mini.nvim", -- comments, pair, surround, statusline, leap
	"folke/which-key.nvim", -- Keymap
	"tpope/vim-sleuth",  -- Indent

	"folke/todo-comments.nvim", -- TODO: comments
	"tpope/vim-abolish", -- Subvert (Search and replace)

	-- Clojure
	"Olical/conjure",                      -- REPL
	"guns/vim-sexp",                       -- Add form and element text objects
	"tpope/vim-sexp-mappings-for-regular-people", -- Better sexp
	"jose-elias-alvarez/null-ls.nvim",
	"folke/neodev.nvim",                   -- Plugin dev

	-- C
	"NoahTheDuke/vim-just",

	-- Git
	"tpope/vim-fugitive", -- Git manager
	"lewis6991/gitsigns.nvim", -- Git gutter + hunks

	-- Markdown
	{
		"iamcco/markdown-preview.nvim",
		ft = "markdown",
		build = ":call mkdp#util#install()",
	},

	-- Neorg
	{
		"nvim-neorg/neorg",
		build = ":Neorg sync-parsers",
		dependencies = { "nvim-lua/plenary.nvim" },
		run = ":Neorg sync-parsers", -- This is the important bit!
		config = function()
			require("neorg").setup {
				load = {
					["core.defaults"] = {}, -- Loads default behaviour
					["core.concealer"] = {}, -- Adds pretty icons to your documents
					["core.concealer"] = {}, -- Adds pretty icons to your documents
					["core.dirman"] = { -- Manages Neorg workspaces
						config = {
							workspaces = {
								notes = "~/notes",
							},
						},
					},
				},
			}
		end,
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
		branch = "v3.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
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
			{
				"j-hui/fidget.nvim",
				tag = 'legacy'
			},
		}
	},
})

vim.cmd [[colorscheme alabaster]]

-- Options
require('mini.basics').setup()
vim.opt.clipboard = "unnamedplus"
vim.opt.relativenumber = true
vim.opt.colorcolumn = "80"
vim.g.sexp_filetypes = "clojure,fennel,fnl"

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
require("gitsigns").setup()      -- Git
require("neodev").setup()        -- Plugin dev

local null_ls = require("null-ls")

null_ls.setup({
	sources = {
		null_ls.builtins.formatting.fnlfmt,
		null_ls.builtins.formatting.prettier,
		null_ls.builtins.diagnostics.markdownlint
	},
})

-- LSP + Complete
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
	"clojure_lsp",
	"clangd",
	"dockerls",
	"docker_compose_language_service",
	"terraformls",
	"jsonls",
	"marksman",
})

require('lspconfig').lua_ls.setup(lsp.nvim_lua_ls())

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

-- Rainbow parens
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
-- Plugin dev

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
			f = { "<cmd>Neotree reveal<cr>", "File" },
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
			q = { "<cmd>Telescope quickfix<cr>", "Quickfix" },
		},
		l = {
			name = "+LSP",
			a = { "<cmd>lua vim.lsp.buf.code_action()<cr>", "Action" },
			f = { "<cmd>lua vim.lsp.buf.format()<cr>", "Format" },
			r = { "<cmd>lua vim.lsp.buf.rename()<cr>", "Rename" },
			s = { "<cmd>Telescope lsp_document_symbols symbols=function,variable<cr>", "Symbol" },
			d = { "<cmd>Telescope diagnostics<cr>", "Diagnostic" },
		},
		h = {
			name = "+hunk",
			s = { "<cmd>Gitsigns stage_hunk<cr>", "Stage" },
			r = { "<cmd>Gitsigns reset_hunk<cr>", "Reset" },
			p = { "<cmd>Gitsigns preview_hunk<cr>", "Preview" },
		},
		-- Buffers
		b = { "<cmd>Telescope buffers theme=dropdown ignore_current_buffer=true previewer=false<cr>", "Buffer" },
		-- Project tree
		t = { "<cmd>Neotree focus<cr>", "Focus tree" },
		T = { "<cmd>Neotree toggle<cr>", "Toggle tree" },
		-- Quickfix
		q = { "<cmd>:copen<cr>", "Focus quickfix" },
		Q = { "<cmd>:cclose<cr>", "Toggle quickfix" },
		-- Registers
		r = { "<cmd>Telescope registers<cr>", "Registers" },
		-- Marks + Fenpoon
		n = { "<cmd>Telescope fenpoon<cr>", "Harpoon" },
		N = { "<cmd>:lua require('fenpoon.api').mark()<cr>", "Harpoon file" },
		m = { "<cmd>Telescope marks<cr>", "Marks" },
	}
})
