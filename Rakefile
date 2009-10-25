require 'rake/clean'

TARGET = 'textserver'

WIN32 = 'win32'
DIST = File.join(WIN32, 'dist')

MSI = File.join(WIN32, TARGET + '.msi')
WIXOBJ = File.join(WIN32, TARGET + '.wixobj')
WIXPDB = File.join(WIN32, TARGET + '.wixpdb')

EXE = File.join(DIST, TARGET + '.exe')
YML = File.join(DIST, TARGET + '.yml')
EXY = TARGET + '.exy'

CLEAN.include(WIXOBJ)
CLEAN.include(WIXPDB)
CLEAN.include(EXY)
CLOBBER.include(MSI)
CLOBBER.include(EXE)

desc 'Produce win32 installer.'
task :win32 => [ MSI ]

file MSI => [ EXE, YML, WIXOBJ ]

rule '.msi' => [ '.wixobj' ] do |t|
  sh "light -ext WixUIExtension -o #{t.name} #{t.source}"
end

rule '.wixobj' => [ '.wxs' ] do |t|
  sh "candle -o #{t.name} #{t.source}"
end

desc 'Produce win32 binary.'
task :win32bin => [ EXE ]

file EXE => [ DIST, EXY ] do |t|
  sh "exerb -o #{EXE} #{EXY}"
end

rule '.exy' => [ '.rb' ] do |t|
  sh "mkexy #{t.source} --test"
end

directory DIST
