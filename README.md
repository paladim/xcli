xcli - it client
======
test 

Run
-----
rebar compile

erl -pa ebin

application:start(xcli).

Api
-----
xcli:connect("you name").

xcli:list().

xcli:histrory("name").

xcli:send("name", "text message").

xcli:i().

Server
--------
[xser](https://github.com/paladim/xser).

