module Process
  def invoke(cmd)
    begin
      require 'rubygems'
      require 'win32/process'
      args = {
        'command_line' => cmd,
      }
      Process.create(args)
    rescue LoadError
      system(cmd + ' &')
    end
  end
  module_function :invoke
end
