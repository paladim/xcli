-module(xcli).

-behaviour(gen_server).

-compile(export_all).

-record(user, {name, pid = no, id = no, status = up}).

start_link() ->
    {Part1, Part2, Part3} = erlang:now(),
    Name1 = lists:flatten(io_lib:format("~p~p~p", [Part1,Part2,Part3])),
    Name2 = list_to_atom(Name1 ++ "@localhost"),
    net_kernel:start([Name2, shortnames]),
    erlang:set_cookie(node(), 'Hello'),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    process_flag(trap_exit, true),
    valid_connect_server(),
    {ok, []}.

connect(Name) when is_list(Name) -> 
    gen_server:call(?MODULE,{reg_name, Name});
connect(_) -> 
    message("Error", "Name no list").

i() -> 
    gen_server:call(?MODULE,i).

list() -> 
    gen_server:call(?MODULE, get_list_users).


send(Name, Data) when is_list(Data) -> 
    gen_server:call(?MODULE,{send_msg, Name, Data});
send(_,_) -> 
    message("Error", "Message no list").


history(Name) -> 
    gen_server:call(?MODULE,{get_history, Name}).


%% callbacks

handle_call({reg_name, Name}, _From, State) ->
    try gen_server:call({global, xser},{reg_name, Name}) of
        {name, no} -> 
            message("Connect to server", "No connect server as " ++ Name);
        {name, Reply} -> 
            message("Connect to server", "Yes, connect server as " ++ Reply#user.name)
    catch _:_ ->
            error_valid_connection("Connect to server as " ++ Name)
    end,
    {reply, ok, State};

handle_call(i, _From, State) ->
    try gen_server:call({global, xser}, i) of
        {i, no} -> 
            message("My Name", "No name");
        {i, User} ->    
            message("My Name", User#user.name)
    catch _:_ ->
            error_valid_connection("My Name")
    end,
    {reply, ok, State};

handle_call(get_list_users, _From, State) ->
    try gen_server:call({global, xser}, get_list_users) of
        {list_users, ListUsers} ->
            message_header("List users"),
            list_users_proc(ListUsers),
            message_footer()
    catch _:_ ->
            error_valid_connection("List users")
    end,
    {reply, ok, State};

handle_call({send_msg, Name, Data}, _From, State) ->
    try gen_server:call({global, xser}, {send_msg, Name, Data}) of
        {send_msg, no} ->    
            message("Send message (" ++ Name ++ ")", "No, error");
        {send_msg, ok} ->    
            message("Send message (" ++ Name ++ ")", "Ok")
    catch _:_ -> 
            error_valid_connection("Send message (" ++ Name ++ ")")
    end,
    {reply, ok, State};

handle_call({get_history, Name}, _From, State) ->
    try gen_server:call({global, xser}, {get_history, Name}) of
        {history, List} ->
            message_header("History (" ++ Name ++ ")"),
            list_history_proc(List),
            message_footer()
    catch _:_ -> 
            error_valid_connection("History (" ++ Name ++ ")")
    end,
    {reply, ok, State}.

handle_cast({name, _D}, State) ->
    {noreply, State}.

handle_info({msg, From, Data}, State) ->
    message("Message from " ++ From#user.name, Data),
    {noreply, State};

handle_info(valid_connect_server, State) ->
    valid_connect_server(), 
    {noreply, State};

handle_info({'DOWN', Ref, process, _Pid, _}, State) ->
    erlang:demonitor(Ref),
    valid_connect_server(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

valid_connect_server() ->
    case net_kernel:connect_node('xser@localhost') of
        true -> 
            try gen_server:call({global, xser}, get_pid_server) of
                {pid_server, PidServer} ->
                    put(pid_server, PidServer),
                    erlang:monitor(process, PidServer),
                    io:format("~nServer is run~n", [])
            catch _:_ ->
                timer:send_after(1000, self(), valid_connect_server)
            end;
        false -> 
            timer:send_after(1000, self(), valid_connect_server)
    end.

error_valid_connection(H)->
            valid_connect_server(),
            message(H, "Server no connect!").

message(H,B) ->
    message_header(H),
    message_body(B),
    message_footer().

message_header(D)->
    io:format("~n ******* ~p", [D]).
message_footer()->
    io:format("~n ******* ~n", []).
message_body(D)->
    io:format("~n ~p", [D]).

list_users_proc([]) ->
    ok;
list_users_proc(List) ->
    [ User | L] = List,
    message_body(User#user.name ++ ", status " ++ atom_to_list(User#user.status)),
    list_users_proc(L).


list_history_proc([]) ->
    ok;
list_history_proc(List) ->
    [{Name, Msg} | L] = List,
    message_body(Name ++ " say -  " ++ Msg),
    list_history_proc(L).
