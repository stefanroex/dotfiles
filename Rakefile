require 'rake'

desc "install the dot files into user's home directory"
task :install do
  replace_all = true
  Dir['*'].each do |file|
    next if %w[Rakefile README.md].include? file
    
    if File.exist?(File.join(ENV['HOME'], ".#{file}"))
      if replace_all
        replace_file(file)
      else
        print "overwrite ~/.#{file}? [ynaq] "
        case $stdin.gets.chomp
        when 'a'
          replace_all = true
          replace_file(file)
        when 'y'
          replace_file(file)
        when 'q'
          exit
        else
          puts "skipping ~/.#{file}"
        end
      end
    else
      link_file(file)
    end
  end
end


desc "Compile Command-T plugin"
task :command_t => :macvim_check do
  vim = which('mvim') || which('vim') or abort "vim not found on your system"
  ruby = read_ruby_version(vim)

  Dir.chdir "vim/bundle/Command-T/ruby/command-t" do
    if ruby
      puts "Compiling Command-T plugin..."
      sh(*Array(ruby).concat(%w[extconf.rb]))
      sh "make clean && make"
    else
      warn color('Warning:', 31) + " Can't compile Command-T, no ruby support in #{vim}"
      sh "make clean"
    end
  end
end

desc "Install gems"
task :gems do
  sh "gem install pry pry-doc"
end

task :macvim_check do
  if mvim = which('mvim') and '/usr/bin/vim' == which('vim')
    warn color('Warning:', 31) + " You have MacVim installed, but `vim` still opens system Vim."
    warn "To use MacVim version when you invoke `vim`:  " + color("$ ln -s mvim #{File.dirname(mvim)}/vim", 37)
  end
end

def replace_file(file)
  system %Q{rm "$HOME/.#{file}"}
  link_file(file)
end

def link_file(file)
  puts "linking ~/.#{file}"
  system %Q{ln -s "$PWD/#{file}" "$HOME/.#{file}"}
end

def color msg, code
  if $stderr.tty? then "\e[1;#{code}m#{msg}\e[m"
  else msg
  end
end

def which cmd
  exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
  ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
    exts.each { |ext|
      exe = "#{path}/#{cmd}#{ext}"
      return exe if File.executable? exe
    }
  end
  return nil
end

def read_ruby_version vim
  script = %{require "rbconfig"; print File.join(RbConfig::CONFIG["bindir"], RbConfig::CONFIG["ruby_install_name"])}
  version = `#{vim} --nofork --cmd 'ruby #{script}' --cmd 'q' 2>&1 >/dev/null | grep -v 'Vim: Warning'`.strip
  version unless version.empty? or version.include?("command is not available")
end
