#! /usr/bin/ruby
$:.unshift(File.dirname($0))
$:.unshift(File.join(File.dirname($0), 'lib'))
require 'webrick'
require 'time'
require 'open-uri'
require 'tmpdir'
require 'yaml'
require 'nkf'
require 'getopt'
require 'file/observer'
require 'invoke'
require 'rbcommand'

class String
  def to_utf8
    return NKF.nkf('-w', self)
  end
end

# options

dargs = {
  :config => File.join(File.dirname($0), "#{File.basename($0, '.*')}.yml"),
}
args = GetOpt.new($*, %w'
  stop
  reset
  c|config=s
  help
', dargs)
if args[:help]
  puts <<"EOM"
Usage: #{File.basename($0)} [-h] [-c config] [--stop]
A daemon for synchronize contents of textarea in a browser with that of a file.
Options:
  -c, --config    Configuration file.
  --stop          Stop daemon.
  -h, --help      Show help.
Default Configuration File:
  #{dargs[:config]}
Configurations:
  :dir       A directory to put :text, :last, :feedback.
  :text      A file which will be send to textarea in a browser.
  :reset     A file to receive reset signal.
  :lock      Lock file.
  :logfile   Log file.
  :loglevel  Log level, minimum 0 (no log) to maximum 5 (debug log).
  :server    WEBrick server options.
EOM
  exit
end

$conf = {
  :dir      => Dir.tmpdir,
  :text     => 'text',
  :reset    => 'reset',
  :lock     => 'lock',
  :logfile  => STDOUT,
  :loglevel => 0,
  :server   => {
    :BindAddress  => '127.0.0.1',
    :Port         => 18080,
  },
}
if args[:config] && File.exist?(args[:config])
  $conf.merge!(YAML.load_file(args[:config])) do |k,v1,v2|
    v1.is_a?(Hash) ? v1.merge(v2) : v2
  end
  $conf[:server][:Logger] = WEBrick::Log.new($conf[:logfile], $conf[:loglevel])
end
$conf[:pid] ||= File.join($conf[:dir], File.basename($0, '.*') + '.pid')

$lock = File.join($conf[:dir], $conf[:lock])
File.unlink($lock) if File.exist?($lock)

$text = File.join($conf[:dir], $conf[:text])
open($text,'wb'){|io|} unless File.exist?($text)

# stop method

args[:stop] = args[:stop] || (args.args + args.rest).include?('stop')
if args[:stop]
  IO.foreach($conf[:pid]) do |line|
      Process.kill(:KILL, line.strip.to_i) if line.strip.length > 0
  end
  open($conf[:pid], 'wb'){|io| io.puts('')}
  exit
end

# daemon

if args[:reset]
  $conf[:server][:Port] = $conf[:server][:Port]+1
else
  rargs = []
  rargs += [ '-c', args[:config] ] if args[:config]
  rargs << '--reset'
  resetserver = RbConfig.self_invoke_command + ' ' + rargs.join(' ')
  pid = Process.invoke(resetserver).process_id
  open($conf[:pid], 'wb'){|io| io.puts(Process.pid); io.puts(pid)}
  require 'webrick/single_thread_server'
end

srv = WEBrick::HTTPServer.new($conf[:server])

srv.mount_proc('/') do |req, res|
  open($lock, 'wb'){|io| io.puts(Time.now.utc.iso8601)}

  last = File.mtime($text)
  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_NO_CONTENT

  File::Observer.watch_dir($conf[:dir], File::Observer::CHANGE) do
    if File.mtime($text) != last
      res.body = IO.read($text).to_utf8
      res.status = WEBrick::HTTPStatus::RC_OK
    end
  end

  File.unlink($lock)
end

srv.mount_proc('/lock') do |req, res|
  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_NO_CONTENT
  if File.exist?($lock)
    res.status = WEBrick::HTTPStatus::RC_OK
  end
end

srv.mount_proc('/reset') do |req, res|
  open(File.join($conf[:dir], $conf[:reset]), 'wb') do |io|
    io.puts((req.body || '').strip.to_utf8)
  end
  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_OK
end

srv.start
