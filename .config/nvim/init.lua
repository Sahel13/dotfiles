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

-- General settings
vim.g.python3_host_prog = "/usr/bin/python3"
vim.o.clipboard = "unnamedplus"
vim.o.cursorline = true
vim.o.number = true
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.smarttab = true
vim.o.expandtab = true
vim.o.smarttab = true
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keymaps
vim.g.mapleader = " "
vim.keymap.set({ "n" }, ";", ":")
vim.keymap.set({ "i" }, "jk", "<ESC>")

-- Split navigation
vim.o.splitbelow = true
vim.o.splitright = true
vim.keymap.set({ "n" }, "<C-J>", "<C-W><C-J>")
vim.keymap.set({ "n" }, "<C-K>", "<C-W><C-K>")
vim.keymap.set({ "n" }, "<C-L>", "<C-W><C-L>")
vim.keymap.set({ "n" }, "<C-H>", "<C-W><C-H>")

-- Move vertically by visual line.
vim.keymap.set({ "n" }, "j", "gj")
vim.keymap.set({ "n" }, "k", "gk")

-- Plugins using lazy.nvim
require("lazy").setup("plugins")
