Control selenium server from Erlang
#tags erlang,selenium,agile

In a previous post, I was arguing that `selenium-rc`_ missed an erlang
library, so I did it.

The communication protocol is very simple, so it was very easy to
implement it with erlang_. At the time of the writing the only things
I didn't tested (and implemented) are unicode support and exponential
number in response.
I know that the number of code line isn't a 'real' good metric, but all
the code for this binding is about 400 lines of code (code, unit test
and acceptance test). I think that it's quite small...

All selenium server commands are described in a xml files, so OpenQA
used a XSL spreadsheet to generate all client bindings. For my erlang
version, I didn't' use this strategy for 2 reasons.
I only want to have erlang code. XSL is a bit complex so I tried
erlang first but it's not the main reason.

Functional programming offers us another way of thinking so I tried
to explore it a little more. For example, classic client bindings have
a function (or method) for each selenium command. I don't think that
it's useful to have such a list, we can use simple erlang atoms to
choose the command to send. With my strategy I only need one function
to be able to handle all seleenium commands. By extension, why limit
the command that an user could send ? The protocol can be used with
any kind of test server.

The library isn't yet packaged but you can download an archive of the 
source code here `erlang selenium client binding`_ .

.. _`selenium-rc`: http://www.openqa.org/selenium-rc
.. _erlang: http://www.erlang.org
.. _`erlang selenium client binding`: http://charpi.net/cgi-bin/download.cgi?file=erl_selenium.tgz
