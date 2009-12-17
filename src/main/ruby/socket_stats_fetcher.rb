#!/usr/bin/ruby
#
# == Synopsis
#
# socket_stats_fetcher gets stats from a plain socket with scala-stats
# listening on the other end, and for each key: value pair, inserts them
# into ganglia with an optional prefix.
#
# == Usage
#
# socket_stats_fetcher.rb [OPTION]
#
# -h, --help:
#    show help
#
# --host h
#    host with scala-stats port listening: default localhost
#
# --port p
#    port where scala-stats listens: default 4321
#
# --prefix x, -p x:
#    prefix all names with this value when inserting into ganglia.
#
# --n:
#    Do not insert into ganglia, simply print to stdout.

require 'getoptlong'
require 'rdoc/ri/ri_paths'
require 'rdoc/usage'
require 'socket'

host = "localhost"
port = "4321"
not_really = nil
prefix = nil
stat_timeout = 86400

opts = GetoptLong.new(
  [ '-h', '--help', GetoptLong::NO_ARGUMENT ],
  [ '-u', '--url', GetoptLong::OPTIONAL_ARGUMENT ],
  [ '-n', GetoptLong::NO_ARGUMENT ],
  [ '-p', '--prefix', GetoptLong::OPTIONAL_ARGUMENT ],
  [ '--port', GetoptLong::OPTIONAL_ARGUMENT ],
  [ '--host', GetoptLong::OPTIONAL_ARGUMENT ]
  )

opts.each do |opt, arg|
  case opt
  when '-h'
    RDoc::usage
    exit 0
  when '--host'
    host = arg
  when '--port'
    port = arg
  when '-n'
    not_really = true
  when '-p'
    prefix = arg
  end
end

socket = TCPSocket.new(host, port)
socket.puts("stats")
stats = socket.read
items = stats.split("\n").map { |stat| stat.split(": ") }

commands = items.map { |a| "gmetric -t float -n #{prefix}#{a[0]} -v #{a[1]} -u items -d #{stat_timeout}" }

if not_really
  commands.each { |cmd| puts cmd }
else
  commands.each { |cmd| system(cmd) }
end
