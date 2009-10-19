task :win32 => [ 'textserver.exe' ]

rule '.exe' => [ '.exy' ] do |t|
  sh "exerb -o #{t.name} #{t.source}"
end

rule '.exy' => [ '.rb' ] do |t|
  sh "mkexy #{t.source} --test"
end
