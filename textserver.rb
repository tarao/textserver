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

dargs = {
  :config => File.join(File.dirname($0), "#{File.basename($0, '.*')}.yml"),
}
args = GetOpt.new($*, %w'
  stop
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
  :last      Indicator of modification time.
  :feedback  Contents of textarea in a browser will be stored here.
  :logfile   Log file.
  :loglevel  Log level, minimum 0 (no log) to maximum 5 (debug log).
  :server    WEBrick server options.
EOM
  exit
end

$conf = {
  :dir      => Dir.tmpdir,
  :text     => 'text',
  :last     => 'last',
  :feedback => 'feedback',
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

if args[:stop]
  url = "http://#{$conf[:server][:BindAddress]}:#{$conf[:server][:Port]}/stop"
  open(url){}
  exit
end

class String
  def to_utf8
    return NKF.nkf('-w', self)
  end
end

class Last
  attr_reader :time

  def initialize(file)
    @file = file
    @time = Time.at(0).utc
    if File.exist?(@file)
      begin
        @time = Time.iso8601(IO.read(@file).strip).utc
      rescue
        # just leave it
      end
    end
  end

  def update(time = nil)
    File.open(@file, 'wb'){|io| io.puts((time || Time.now.utc).iso8601)}
  end
end

$text = File.join($conf[:dir], $conf[:text])
open(text,'wb'){|io|} unless File.exist?($text)

srv = WEBrick::HTTPServer.new($conf[:server])
srv.mount_proc('/') do |req, res|
  last = Last.new(File.join($conf[:dir], $conf[:last]))
  modified = File.mtime($text).utc
  res.content_type = 'text/plain; charset=utf-8'
  if last.time < modified
    res.body = IO.read($text).to_utf8
    last.update(modified)
  else
    res.status = 204
  end
end
srv.mount_proc('/feedback') do |req, res|
  res.content_type = 'text/plain'
  text = req.body || ''
  text = text.strip.to_utf8
  open(File.join($conf[:dir], $conf[:feedback]), 'wb'){|io| io.puts(text)}
end
srv.mount_proc('/stop'){ srv.stop }
srv.start
