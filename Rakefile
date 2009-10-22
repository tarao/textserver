require 'rake/clean'

TARGET = 'textserver'
BIN = TARGET + '.exe'
RECIPE = TARGET + '.exy'
DIST = 'win32/dist'
CLEAN.include(RECIPE)
CLOBBER.include(BIN)

desc 'produce ' + BIN
task :win32bin => [ BIN ]

rule '.exe' => [ '.exy', DIST ] do |t|
  target = File.join(DIST, t.name)
  sh "exerb -o #{target} #{t.source}"
end

rule '.exy' => [ '.rb' ] do |t|
  sh "mkexy #{t.source} --test"
end

directory DIST
