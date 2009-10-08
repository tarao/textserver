require 'rbconfig'

module RbConfig
  cmd = File.join(CONFIG['bindir'], CONFIG['ruby_install_name'])
  CONFIG['ruby_command'] = cmd.sub(/.*\s.*/m, '"\&"')

  def self_invoke_command
    cmd = File.expand_path($0)
    cmd.sub(/.*\s.*/m, '"\&"')
    ext = Config::CONFIG['EXEEXT']
    unless ext.length != 0 && $0 =~ /#{ext}$/
      cmd = CONFIG['ruby_command'] + ' ' + cmd
    end
    return cmd
  end

  module_function :self_invoke_command
end
