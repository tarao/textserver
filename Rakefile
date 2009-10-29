require 'rake/clean'

WISUBSTG = File.join('C:', 'Program Files',
                     'Microsoft Platform SDK',
                     'Samples', 'SysMgmt', 'Msi', 'Scripts',
                     'WiSubStg.vbs')

TARGET = 'textserver'

WIN32 = 'win32'
DIST = File.join(WIN32, 'dist')

MSI = File.join(WIN32, TARGET + '.msi')
WIXOBJ = File.join(WIN32, TARGET + '.wixobj')
WIXPDB = File.join(WIN32, TARGET + '.wixpdb')
WXS = File.join(WIN32, TARGET + '.wxs')

LANGS =
  [
   { :name     => 'en-us',
     :language => 1033,
     :codepage => 1252 },
   { :name     => 'ja-jp',
     :language => 1041,
     :codepage =>  932 },
  ]
BASE, TRANSLATES = LANGS.map{|l| l[:name]}
BASEMSI = File.join(WIN32, [TARGET, BASE, 'msi'].join('.'))
MSTS = TRANSLATES.map{|l| File.join(WIN32, [TARGET,l,'mst'].join('.'))}

EXE = File.join(DIST, TARGET + '.exe')
YML = File.join(DIST, TARGET + '.yml')
EXY = TARGET + '.exy'

CLEAN.include('**/*.wixobj')
CLEAN.include('**/*.wixpdb')
CLEAN.include('**/*.mst')
CLEAN.include(BASEMSI)
CLEAN.include(EXY)
CLOBBER.include('**/*.msi')
CLOBBER.include(EXE)

desc 'Produce win32 installer.'
task :win32 => [ MSI ]

# main installer
file MSI => [ BASEMSI ] + MSTS do |t|
  cp BASEMSI, MSI
  ret = MSTS.all? do |mst|
    # embed each .mst into the .msi
    lname = File.extname(File.basename(mst, '.*'))[1..-1]
    lang = LANGS.find{|l| l[:name] == lname}[:language]
    sh "wscript \"#{WISUBSTG}\" #{MSI} #{mst} #{lang}"
  end
  langs = LANGS.map{|l| l[:language]}.join(',');
  ret && sh("msiinfo #{MSI} /p \"Intel;#{langs}\"") # register all languages
end

# transform for each language
rule '.mst' => [ '.msi' ] do |t|
  sh "torch -p -t language #{BASEMSI} #{t.source} -o #{t.name}"
end

# installer for each language
LANGS.each do |lang|
  msi = File.join(WIN32, [TARGET, lang[:name], 'msi'].join('.'))
  wxl = File.join(WIN32, [TARGET, lang[:name], 'wxl'].join('.'))
  wixobj = File.join(WIN32, [TARGET, lang[:name], 'wixobj'].join('.'))
  file msi => [ wixobj, wxl ] do |t|
    sh [
        'light',
        '-ext WixUIExtension',
        '-ext WixUtilExtension',
        "-cultures:#{lang[:name]}",
        "-loc #{wxl}",
        "-o #{t.name}",
        " #{wixobj}",
       ].join(' ')
  end
end

# object file for each language
LANGS.each do |lang|
  wixobj = File.join(WIN32, [TARGET, lang[:name], 'wixobj'].join('.'))
  file wixobj => [ EXE, YML, WXS ] do |t|
    opt = [ :language, :codepage ].map{|k| "-d#{k}=#{lang[k]}"}.join(' ')
    sh "candle #{opt} -o #{t.name} #{WXS}"
  end
end

desc 'Produce win32 binary.'
task :win32bin => [ EXE ]

# binary
file EXE => [ DIST, EXY ] do |t|
  sh "exerb -o #{EXE} #{EXY}"
end

# recipe
rule '.exy' => [ '.rb' ] do |t|
  sh "mkexy #{t.source} --test"
end

directory DIST
