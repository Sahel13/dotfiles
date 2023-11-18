return {
  {
    "sainnhe/gruvbox-material",
    lazy = false,
    priority = 1000,
    init = function()
      vim.o.termguicolors = true
      vim.o.background = "dark"
      vim.g.gruvbox_material_background = "medium"
      vim.g.gruvbox_material_better_performance = 1
    end,
    config = function()
      vim.cmd("colorscheme gruvbox-material")
    end,
  },
  {
    "vimwiki/vimwiki",
    init = function()
      vim.g.vimwiki_list = {
        {        
          path = "~/Documents/vimwiki",
          syntax = "markdown",
          ext = ".md",
        }
      }
      vim.g.vimwiki_global_ext = 0
      vim.g.vimwiki_markdown_link_ext = 1
    end,
  },
  {
    "lervag/vimtex",
    init = function()
      vim.g.maplocalleader = " "
      vim.g.vimtex_quickfix_mode = 0
      vim.g.vimtex_compiler_latexmk = {
        aux_dir = "aux_dir",
        out_dir = "out_dir",
      }
      vim.g.vimtex_indent_enabled = 0
    end,
    },
  {
    "SirVer/ultisnips",
    init = function()
      vim.g.UltiSnipsExpandTrigger = "<tab>"
      vim.g.UltiSnipsJumpForwardTrigger = "<tab>"
      vim.g.UltiSnipsJumpBackwardTrigger = "<s-tab>"
      vim.g.UltiSnipsSnippetDirectories = {"UltiSnips"}
    end,
  },
  {
    "cloudhead/neovim-fuzzy",
    keys = {{ "<C-_>", "<cmd>:FuzzyOpen<cr>", silent = true}},
  },
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function ()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
          ensure_installed = { "c", "lua", "vim", "vimdoc", "query", "haskell", "python", "julia" },
          sync_install = false,
          highlight = { enable = true },
        })
    end
  },
}
