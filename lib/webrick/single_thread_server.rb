require 'thread'
require 'socket'
require 'timeout'
require 'webrick/config'
require 'webrick/log'

module WEBrick
  class GenericServer
    def start(&block)
      raise ServerError, "already started." if @status != :Stop
      server_type = @config[:ServerType] || SimpleServer

      server_type.start{
        @logger.info \
          "#{self.class}#start: pid=#{$$} port=#{@config[:Port]}"
        call_callback(:StartCallback)

        @status = :Running
        while @status == :Running
          begin
            if svrs = IO.select(@listeners, nil, nil, 2.0)
              svrs[0].each{|svr|
                @tokens.pop          # blocks while no token is there.
                if sock = accept_client(svr)
                  th = start_thread(sock, &block)
                else
                  @tokens.push(nil)
                end
              }
            end
          rescue Errno::EBADF, IOError => ex
            # if the listening socket was closed in GenericServer#shutdown,
            # IO::select raise it.
          rescue Exception => ex
            msg = "#{ex.class}: #{ex.message}\n\t#{ex.backtrace[0]}"
            @logger.error msg
          end
        end

        @logger.info "going to shutdown ..."
        call_callback(:StopCallback)
        @logger.info "#{self.class}#start done."
        @status = :Stop
      }
    end

    private

    def start_thread(sock, &block)
      begin
        Thread.current[:WEBrickSocket] = sock
        begin
          addr = sock.peeraddr
          @logger.debug "accept: #{addr[3]}:#{addr[1]}"
        rescue SocketError
          @logger.debug "accept: <address unknown>"
          raise
        end
        call_callback(:AcceptCallback, sock)
        block ? block.call(sock) : run(sock)
      rescue Errno::ENOTCONN
        @logger.debug "Errno::ENOTCONN raised"
      rescue ServerError => ex
        msg = "#{ex.class}: #{ex.message}\n\t#{ex.backtrace[0]}"
        @logger.error msg
      rescue Exception => ex
        @logger.error ex
      ensure
        @tokens.push(nil)
        Thread.current[:WEBrickSocket] = nil
        if addr
          @logger.debug "close: #{addr[3]}:#{addr[1]}"
        else
          @logger.debug "close: <address unknown>"
        end
        sock.close
      end
    end
  end    # end of GenericServer
end
