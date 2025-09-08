return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "MunifTanjim/nui.nvim",
  },
  keys = {
    { "<leader>a", "<cmd>Neotree toggle<cr>", desc = "NeoTree" },
  },
  config = function ()
    require("neo-tree").setup({
      filesystem = {
        filtered_items = {
          never_show = {
            ".DS_Store",
          },
        },
        window = {
          mappings = {
            ["o"] = { "open", nowait = true },
            ["oc"] = "none",
            ["od"] = "none",
            ["og"] = "none",
            ["om"] = "none",
            ["on"] = "none",
            ["os"] = "none",
            ["ot"] = "none",
          }
        }
      }
    })
  end,
}
