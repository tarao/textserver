#! /usr/bin/env ruby
$dir = File.dirname($0)
$dir = File.dirname(File.readlink($0)) if File.symlink?($0)
$dir = File.expand_path($dir, File.dirname($0))
$:.unshift($dir)
$:.unshift(File.join($dir, 'lib'))
require 'webrick'
require 'time'
require 'open-uri'
require 'tmpdir'
require 'fileutils'
require 'yaml'
require 'nkf'
require 'rubygems'
require 'getopt'
require 'file/observer'
require 'process/invoke'
require 'rbconfig/command'
require 'net/http'

$p = RbConfig.program_name
$p = File.readlink($p) if File.symlink?($p)
$p = File.expand_path($p, File.dirname(RbConfig.program_name))

class String
  def to_utf8
    return NKF.nkf('-w', self)
  end
end

# options

dargv = {
  :config => File.join(File.dirname($p), "#{File.basename($p, '.*')}.yml"),
}
argv = GetOpt.new($*, %w'
  status
  start
  stop
  reset
  test
  c|config=s
  help
', dargv)
if argv[:help]
  puts <<"EOM"
Usage: #{File.basename($p)} [-h] [-c config] [--stop]
A daemon for synchronize contents of textarea in a browser with that of a file.
Options:
  -c, --config    Configuration file.
  --status        Check if daemon is running.
  --start         Start daemon.
  --stop          Stop daemon.
  -h, --help      Show help.
Default Configuration File:
  #{dargv[:config]}
Configurations:
  :dir       A directory to put :text, :last, :feedback.
  :text      A file which will be send to textarea in a browser.
  :reset     A file to receive reset signal.
  :lock      Lock file.
  :pid       A file to save PIDs.
  :logfile   Log file.
  :loglevel  Log level, minimum 0 (no log) to maximum 5 (debug log).
  :server    WEBrick server options.
EOM
  exit
end

$conf = {
  :dir      => File.join(Dir.tmpdir, File.basename($p, '.*')),
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
if argv[:config] && File.exist?(argv[:config])
  $conf.merge!(YAML.load_file(argv[:config])) do |k,v1,v2|
    v1.is_a?(Hash) ? v1.merge(v2) : v2
  end
  $conf[:server][:Logger] = WEBrick::Log.new($conf[:logfile], $conf[:loglevel])
end
$conf[:pid] ||= File.join($conf[:dir], File.basename($p, '.*') + '.pid')

if !File.exist?($conf[:dir])
  Dir.mkdir($conf[:dir])
  owndir = true
end

$lock = File.join($conf[:dir], $conf[:lock])
File.unlink($lock) if File.exist?($lock)

$text = File.join($conf[:dir], $conf[:text])
open($text,'wb'){|io|} unless File.exist?($text)

$reset = File.join($conf[:dir], $conf[:reset])
open($reset, 'wb'){|io|} unless File.exist?($reset)
$last = ''

# start method

argv[:start] = argv[:start] || (argv.args + argv.rest).include?('start')
if argv[:start]
  server = [ RbConfig.self_invoke_command ]
  server += [ '-c', argv[:config].sub(/.*\s.*/m,'"\&"') ] if argv[:config]
  Process.invoke(server.join(' '))
  exit
end

# stop method

argv[:stop] = argv[:stop] || (argv.args + argv.rest).include?('stop')
if argv[:stop]
  IO.foreach($conf[:pid]) do |line|
      Process.kill(:KILL, line.strip.to_i) if line.strip.length > 0
  end
  open($conf[:pid], 'wb'){|io|}
  exit
end

# status method

def status_check(host, port)
  res = Net::HTTP.get_response(host, '/status', port) rescue nil
  return res && res.code.to_i == 200
end

argv[:status] = argv[:status] || (argv.args + argv.rest).include?('status')
if argv[:status]
  pids = IO.foreach($conf[:pid]).map(&:strip).reject(&:empty?).map(&:to_i)
  status = pids.length > 0 && pids.all? do |pid|
    Process.getpgid(pid) rescue nil
  end

  if status && status_check('localhost', $conf[:server][:Port]+1)
    puts('running')
    exit
  else
    puts('not running')
    exit(1)
  end
end

# daemon

if argv[:reset]
  $conf[:server][:Port] = $conf[:server][:Port]+1
  if argv[:test]
    sleep(5)
    open($reset, 'wb'){|io|}
    exit
  else
    open($conf[:pid], 'ab'){|io| io.puts(Process.pid)}
  end
else
  resetserver = [ RbConfig.self_invoke_command ]
  resetserver += [ '-c', argv[:config].sub(/.*\s.*/m,'"\&"') ] if argv[:config]
  resetserver << '--test' if argv[:test]
  resetserver << '--reset'
  open($conf[:pid], 'wb'){|io| io.puts(Process.pid)} unless argv[:test]
  Process.invoke(resetserver.join(' '))
  require 'webrick/single_thread_server'
end

srv = WEBrick::HTTPServer.new($conf[:server])

listen = proc do |req, res|
  open($lock, 'wb'){|io| io.puts(Time.now.utc.iso8601)}

  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_NO_CONTENT

  File::Observer.watch([$reset, $text], File::Observer::CHANGE) do |x|
    sleep(0.1)
    if x[:name] == $text
      res.body = IO.read($text).to_utf8
      res.status = WEBrick::HTTPStatus::RC_OK
    end
  end

  File.unlink($lock)
end
srv.mount_proc('/', &listen)

srv.mount_proc('/lock') do |req, res|
  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_NO_CONTENT
  if File.exist?($lock)
    res.status = WEBrick::HTTPStatus::RC_OK
  end
end

srv.mount_proc('/reset') do |req, res|
  body = (req.body || '').strip.to_utf8
  if body.length >= 0 && $last != body
    open($reset, 'wb'){|io| io.puts(body)}
    $last = body
  else
    FileUtils.touch($reset)
  end
  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_OK
end

srv.mount_proc('/status') do |req, res|
  res.body = 'running'
  res.content_type = 'text/plain; charset=utf-8'
  res.status = WEBrick::HTTPStatus::RC_OK
end

if argv[:test]
  sleep(1)
  listen.call(nil, WEBrick::HTTPResponse.new({:HTTPVersion => '1.1'}))
  FileUtils.rm_r($conf[:dir]) if owndir
else
  srv.start
end
