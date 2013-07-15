#!/usr/bin/ruby
# exe ='../bin/write_model_datasets'
exe ='/home/graham_s/VirtualWorlds/projects/oscr/model/bin/oscr_server'
ARGV.each{
        |pos|
        command = "addr2line #{pos} --exe=#{exe} --demangle=gnat functions"
        system command
}
