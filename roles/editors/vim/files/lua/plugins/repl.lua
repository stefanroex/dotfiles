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
  config = function()
    vim.api.nvim_create_autocmd("FileType", {
      pattern = { "clojure", "fennel", "lua" },
      callback = function(ev)
        local opts = function(desc)
          return { buffer = ev.buf, desc = desc }
        end
        vim.keymap.set("n", "<leader>e", "<cmd>ConjureEvalCurrentForm<cr>", opts("Eval current form"))
        vim.keymap.set("n", "<leader>eb", "<cmd>ConjureEvalBuf<cr>", opts("Eval buffer"))
        vim.keymap.set("n", "<leader>er", "<cmd>ConjureEvalRootForm<cr>", opts("Eval root form"))
        vim.keymap.set("n", "<leader>ew", "<cmd>ConjureEvalWord<cr>", opts("Eval word"))
        vim.keymap.set("v", "<leader>e", "<cmd>ConjureEvalVisual<cr>", opts("Eval selection"))
        vim.keymap.set("n", "<leader>ei", "<cmd>ConjureEvalInterrupt<cr>", opts("Eval interrupt"))
      end,
    })
  end,
}
