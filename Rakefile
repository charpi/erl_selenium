#-*-ruby-*-
require 'rake'

task :start_server do
  sh "java -jar lib/selenium_remote/priv/selenium-server-1.0.1.jar -log /tmp/selenium_server.log > /dev/null 2>& 1 &\
      echo $! > /tmp/selenium_server.pid;"
end

task :stop_server do
  sh "kill `cat /tmp/selenium_server.pid`"
end

task :selenium_build => ["erlang:applications"] do
  sh "#{ERL_TOP}/bin/escript lib/selenium_remote/priv/make_api.esh api lib/selenium_remote/ebin lib/selenium_remote/priv/iedoc.xml"
  sh "#{ERL_TOP}/bin/escript lib/selenium_remote/priv/make_api.esh session lib/selenium_remote/ebin lib/selenium_remote/priv/iedoc.xml"
end

task :tests => [:selenium_build, "erlang:tests"]

task :doc => ["erlang:edoc"] do
  sh "#{ERL_TOP}/bin/escript lib/selenium_remote/priv/make_api.esh doc lib/selenium_remote/doc lib/selenium_remote/priv/iedoc.xml -- lib/selenium_remote/ebin"
end

task :deliver => [] do
  version = `git rev-list HEAD | wc -l`.strip
  sh "git archive master | bzip2 > selenium_src_#{version}.tar.bz2"
end

task :default => :selenium_build
