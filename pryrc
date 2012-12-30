Pry.config.should_load_plugins = false
Pry.plugins["doc"].activate!

Pry.config.history.file = "~/.irb_history"
Pry.config.prompt = ->(obj, nest_level, _){ nest_level > 0 ? "#{obj}:#{nest_level}> " : ">> " }

rails = File.join Dir.getwd, 'config', 'environment.rb'

if File.exist?(rails)
  require rails

  if Rails.version[0..0] == "3"
    require 'rails/console/app'
    require 'rails/console/helpers'
    extend Rails::ConsoleMethods
  else
    warn "[WARN] cannot load Rails console commands"
  end
end
