return {
  "Olical/conjure",
  ft = { "clojure", "fennel", "lua" },
  init = function()
    vim.g["conjure#mapping#prefix"] = "<leader>c"
    vim.g["conjure#mapping#eval_buf"] = false
    vim.g["conjure#mapping#eval_current_form"] = false
    vim.g["conjure#mapping#eval_root_form"] = false
    vim.g["conjure#mapping#eval_word"] = false
    vim.g["conjure#mapping#eval_visual"] = false
    vim.g["conjure#mapping#eval_interrupt"] = false
    vim.g["conjure#mapping#log_split"] = "l"
    vim.g["conjure#mapping#log_close_visible"] = "q"
    vim.g["conjure#mapping#refresh_changed"] = "r"
    vim.g["conjure#mapping#refresh_all"] = "R"
    vim.g["conjure#mapping#doc_word"] = "d"
    vim.g["conjure#mapping#def_word"] = "gd"
    vim.g["conjure#mapping#connect_port_host"] = "c"
  end,
  keys = {
    { "<leader>e", "<cmd>ConjureEvalCurrentForm<cr>", ft = { "clojure", "fennel", "lua" }, desc = "Eval current form" },
    { "<leader>eb", "<cmd>ConjureEvalBuf<cr>", ft = { "clojure", "fennel", "lua" }, desc = "Eval buffer" },
    { "<leader>er", "<cmd>ConjureEvalRootForm<cr>", ft = { "clojure", "fennel", "lua" }, desc = "Eval root form" },
    { "<leader>ew", "<cmd>ConjureEvalWord<cr>", ft = { "clojure", "fennel", "lua" }, desc = "Eval word" },
    { "<leader>e", "<cmd>ConjureEvalVisual<cr>", mode = "v", ft = { "clojure", "fennel", "lua" }, desc = "Eval selection" },
    { "<leader>ei", "<cmd>ConjureEvalInterrupt<cr>", ft = { "clojure", "fennel", "lua" }, desc = "Eval interrupt" },
  },
}
