return {
  "tpope/vim-projectionist",
  config = function()
    vim.g.projectionist_heuristics = {
      ["deps.edn|project.clj"] = {
        ["src/*.clj"] = { alternate = "test/{}_test.clj" },
        ["test/*_test.clj"] = { alternate = "src/{}.clj" },
        ["src/*.cljc"] = { alternate = "test/{}_test.cljc" },
        ["test/*_test.cljc"] = { alternate = "src/{}.cljc" },
        ["src/*.cljs"] = { alternate = "test/{}_test.cljs" },
        ["test/*_test.cljs"] = { alternate = "src/{}.cljs" },
      },
    }
  end,
  keys = {
    { "<leader>pt", "<cmd>A<cr>", desc = "Toggle test/implementation" },
  },
}
