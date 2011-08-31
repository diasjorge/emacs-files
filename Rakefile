task :compile => :clean do
  make_dirs do
    system "make EMACS=#{emacs}"
  end

  Dir["elisp/*.el", "elisp/js2/*.el"].each do |f|
    compile_file(f)
  end
end

task :clean do
  puts red("Cleaning up")

  make_dirs do
    system "make clean || make distclean"
  end

  elc_files = Dir["**/*.elc"]

  unless elc_files.empty?
    cmd = %{rm #{elc_files.join(" ")}}
    puts red(cmd)
    system cmd
  end
end

def compile_file(file)
  puts green(file)
  system "#{emacs} -q -Q -L . -batch -f batch-byte-compile #{file}"
end

def green(str)
  "\e[1;32m#{str}\e[0m"
end

def red(str)
  "\e[1;31m#{str}\e[0m"
end

def emacs
  ENV['EMACS'] || %x{which emacs}.chomp
end

def make_dirs
  Dir["**/*/Makefile"].each do |x|
    dir = File.dirname(x)
    Dir.chdir(dir) do
      puts green(dir)
      yield
    end
  end
end

task :default => "compile"
