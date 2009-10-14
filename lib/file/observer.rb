class File
  class Observer
    ACCESS = 0x0001
    MODIFY = 0x0002
    ATTRIB = 0x0004
#     CLOSE_WRITE   = 0x0008
#     CLOSE_NOWRITE = 0x0010
#     OPEN          = 0x0020
    MOVED_FROM = 0x0040
    MOVED_TO   = 0x0080
    CREATE = 0x0100
    DELETE = 0x0200
#     DELETE_SELF = 0x0400
#     MOVE_SELF   = 0x0800
#     CLOSE = CLOSE_WRITE | CLOSE_NOWRITE
    MOVE = MOVED_FROM | MOVED_TO
    CHANGE = MODIFY | ATTRIB | MOVE | CREATE | DELETE

    class Win32
      def self.installed?
        begin
          require 'rubygems'
          require 'win32/changenotify'
        rescue LoadError
          return false
        end
        return self
      end

      def self.watch(files, events, dir = false)
        mask = 0
        # FIXME: more events
#        mask |= ::Win32::ChangeNotify::ATTRIBUTES if (events & ATTRIB) != 0
        mask |= ::Win32::ChangeNotify::DIR_NAME   if (events & MOVE) != 0
        mask |= ::Win32::ChangeNotify::FILE_NAME  if (events & MOVE) != 0
        mask |= ::Win32::ChangeNotify::LAST_WRITE if (events & CHANGE) != 0
        mask |= ::Win32::ChangeNotify::SIZE       if (events & CHANGE) != 0
        files = File.dirname(files.first) unless dir
        cn = ::Win32::ChangeNotify.new(files, false, mask)
        loop do
          cn.wait do |arr|
            arr.each do |st|
              if dir || files.find{|f| f == st.file_name}
                # FIXME: convert st.action to event
                sleep(0.1)
                yield({ :name => st.file_name, :event => events })
                return true
              end
            end
          end
        end
        return false
      end

      def self.watch_dir(dir, events, &block)
        return self.watch(dir, events, true, &block)
      end
    end

    class Inotify
      def self.installed?
        begin
          require 'inotify'
        rescue LoadError
          return false
        end
        return self
      end

      def self.watch(files, events)
        notifier = ::Inotify.new
        begin
          files.each{|f| notifier.add_watch(f, events)}
          notifier.each_event do |e|
            yield({ :event => e.mask })
            return true
          end
        ensure
          notifier.close
        end
        return false
      end

      def self.watch_dir(dir, events, &block)
        return self.watch(dir, events, &block)
      end
    end

    class Naive
      def self.installed?
        return Naive
      end

      def self.watch(files, events, s=0.1)
        mtime = {}
        files.each{|f| mtime[f] = File.mtime(f)}

        loop do
          # FIXME: other events
          fname = files.find{|f| File.mtime(f) != mtime[f]}
          if fname
            yield({ :name => fname, :event => events })
            return true
          end
          sleep(s)
        end
        return false
      end

      def self.watch_dir(dir, events, s=0.1, &block)
        return self.watch(dir, events, s, &block)
      end
    end

    def self.watch(files, events, &block)
#       klass = Win32.installed? || Inotify.installed? || Naive.installed?
      klass = Win32.installed? || Naive.installed?
      raise 'No file observer' unless klass
      files = [files] unless files.is_a?(Array)
      mtime = {}
      files.each{|f| mtime[f] = File.mtime(f)}
      return klass.watch(files, events) do |x|
        # FIXME: other events
        x[:name] = x[:name] || files.find{|f| File.mtime(f) != mtime[f]}
        block.call(x)
      end
    end

    def self.watch_dir(dir, events, &block)
      klass = Win32.installed? || Inotify.installed? || Naive.installed?
      raise 'No file observer' unless klass
      return klass.watch_dir(dir, events, &block)
    end
  end
end
