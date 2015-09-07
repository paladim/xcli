xcli - it client
======

Server
--------
[xser](https://github.com/paladim/xser)

Run
-----
rebar compile

erl -pa ebin

application:start(xcli).

Api
-----
xcli:connect("you name").

xcli:list().

xcli:history("name").

xcli:send("name", "text message").

xcli:i().


