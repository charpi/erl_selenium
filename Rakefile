#-*-ruby-*-
require 'rake'

file 'lib/selenium_remote/ebin/selenium_api.beam' => 
  ['lib/selenium_remote/ebin',
   'lib/selenium_remote/priv/iedoc.xml',
   'lib/selenium_remote/ebin/make_api.beam',
   'lib/selenium_remote/priv/make_api.esh'] do
  sh "#{ERL_TOP}/bin/escript lib/selenium_remote/priv/make_api.esh api lib/selenium_remote/ebin lib/selenium_remote/priv/iedoc.xml"
end

file 'lib/selenium_remote/ebin/selenium_session.beam' => 
  ['lib/selenium_remote/ebin',
   'lib/selenium_remote/priv/iedoc.xml',
   'lib/selenium_remote/ebin/make_api.beam',
   'lib/selenium_remote/priv/make_api.esh'] do
  sh "#{ERL_TOP}/bin/escript lib/selenium_remote/priv/make_api.esh session lib/selenium_remote/ebin lib/selenium_remote/priv/iedoc.xml"
end

task :start_server do
  sh "java -jar lib/selenium_remote/priv/selenium-server-1.0-beta-2.jar -log /tmp/selenium_server.log > /dev/null 2>& 1 &\
      echo $! > /tmp/selenium_server.pid;"
end

task :stop_server do
  sh "kill `cat /tmp/selenium_server.pid`"
end

task :default => ['lib/selenium_remote/ebin/selenium_api.beam',
                  'lib/selenium_remote/ebin/selenium_session.beam',
                  "erlang:tests"]
