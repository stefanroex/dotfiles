return {
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',
  dependencies = { 'nvim-lua/plenary.nvim' },
  opts = function()
    local actions = require("telescope.actions")
    return {
      defaults = {
        mappings = {
          i = {
            ["<esc>"] = actions.close,
          },
        },
      },
    }
  end,
  keys =  {
    {
      "<leader>f",
      "<cmd>Telescope find_files sort_mru=true sort_lastused=true<cr>",
      desc = "Find files",
    }
  }
}
