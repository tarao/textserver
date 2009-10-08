class GetOpt
  def self.escape(str, quote=true)
    str = str.to_s
    return str unless str.match(/[\\\"\'\s]/)
    str = str.gsub(/\\/, '\\\\').gsub(/\"/, '\\\"')
    return quote ? '"'+str+'"' : str
  end

  attr_accessor :args, :parsed, :rest, :parser

  def initialize(args, *opts)
    @parsed = {}
    @args = []
    @rest = []

    # argument syntax definitions
    syntax(opts[0] || [], opts[1] || {})

    # custom parser
    @parser = {
      :custom => (opts[1].is_a?(Proc) && opts[1]) || proc{|v| eat(v)}
    }

    pparse_rest       = proc{|n,v,a,r| parse_rest(n,v,a,r)}
    pparse_val        = proc{|n,v,a,r| parse_val(n,v,a,r)}
    pparse_bool_short = proc{|n,v,a,r| parse_bool_short(n,v,a,r)}
    pparse_bool_long  = proc{|n,v,a,r| parse_bool_long(n,v,a,r)}

    # parser for short arguments
    @parser[:short] = {
      'b' => pparse_bool_short,
      'i' => pparse_val,
      'f' => pparse_val,
      's' => pparse_val,
    }

    # parser for long arguments
    @parser[:long] = {
      'b' => pparse_bool_long,
      'i' => pparse_val,
      'f' => pparse_val,
      's' => pparse_val,
      ''  => pparse_rest, # '... -- ...' is a separator
    }

    return parse(args || [])
  end

  def syntax(opts, default={})
    @full = {}
    @type = {}
    @default = {}
    default.each{|k,v| @default[k.to_s] = v}
    opts.each do |opt|
      default_value = nil
      if opt.is_a?(Array)
        default_value = opt[1]
        opt = opt[0]
      end
      if opt =~ /^(?:([^-=])\|)?([^-=]+)(?:=(.+))?$/
        @type[$2] = $3 || 'b'
        @full[$1] = $2 if $1
        @default[$2.to_s] = default_value if default_value
      end
    end
    @type[''] = ''
    return self
  end

  def parse(args)
    args = @parser[:custom].call(args) while args.length != 0
    return self
  end

  def [](opt)
    opt = opt.to_s
    val = @parsed[opt]
    return val == nil ? @default[opt] : val
  end

  def []=(opt,val)
    return @parsed[opt.to_s] = val
  end

  def push(name, val)
    if @type[name] == 'b'
      if !val
        @parsed[name] = false
      elsif !@parsed[name]
        @parsed[name] = true
      elsif @parsed[name].is_a?(Integer)
        @parsed[name] = @parsed[name].succ
      else # @parsed[name] == true
        @parsed[name] = 2 # promote to be an Integer
      end
    else
      if @parsed[name].instance_of?(Array)
        @parsed[name] << val
      elsif @parsed[name]
        @parsed[name] = [ @parsed[name], val ] # promote to be an Array
      else
        @parsed[name] = val
      end
    end
    return self
  end

  def to_s
    arr1 = []
    @parsed.each{|name,value|
      if value.is_a?(TrueClass) || value.is_a?(FalseClass)
        arr1 << ('--' + (value ? '' : 'no-') + name)
      elsif @type[name] == 'b' && value.is_a?(Integer)
        arr1 += (1..value).map{|v| '--' + name}
      elsif value.is_a?(Array)
        value.each{|v| arr1 << ('--' + name + '=' + self.class.escape(v))}
      else
        arr1 << ('--' + name + '=' + self.class.escape(value))
      end
    }
    arr2 = @args
    arr2 += ['--'] + @rest if @rest.length > 0
    return (arr1 + arr2.map{|val| self.class.escape(val)}).join(' ')
  end

  # predefined parsers

  def parse_bool_short(name, val, arg, rest)
    flags = arg[1..-1] # skip '-'
    flags.each_byte do |f| # for each flag
      name = @full[f.chr]
      if name && @type[name] == 'b'
        push(name, true)
      else
        parse_unknown(name, nil, '-'+f.chr, rest)
      end
    end
    return rest
  end

  def parse_bool_long(name, val, arg, rest)
    push(name, !( (val == 'false') ^ (arg =~ /^--no/) ))
    return rest
  end

  def parse_val(name, val, arg, rest)
    val = (val || rest.shift).send('to_' + @type[name]) # to_i, to_f, etc.
    push(name, val)
    return rest
  end

  def parse_rest(name, val, arg, rest)
    @rest = rest
    return []
  end

  def parse_unknown(name, val, arg, rest)
    @args << arg
    return rest
  end

  private

  def get_parser(name, long)
    parser = @parser[ long ? :long : :short ]
    return parser[@type[name]] || proc{|n,v,a,r| parse_unknown(n,v,a,r)}
  end

  def eat(args)
    arg = args.shift

    long = false
    name = nil
    if arg =~ /^--(no-?)?([^=]*)(?:=(.*))?$/
      long = true
      name = (@type[$2] || !$1) ? $2 : $1+$2
      val = $3
    elsif arg =~ /^-([^-])(.+)?$/
      name = @full[$1]
      val = $2
    end

    return get_parser(name, long).call(name, val, arg, args)
  end
end
