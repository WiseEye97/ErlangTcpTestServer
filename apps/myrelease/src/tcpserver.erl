-module(tcpserver).

-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/2,tcpserver/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {users = []}).

start(Name) ->
   start_link(Name,5).

stop(Name) ->
   gen_server:call(Name, stop).

start_link(Name,LS) ->
   gen_server:start_link({local, Name}, ?MODULE, [LS], []).

init(LS) ->
    start_servers(5,LS),
    {ok, #state{users = []}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    {Reply,NewState} =
        case Request of
            {message,register,UserName} ->
                Users = State#state.users,
                UpdatedUsers = [{UserName,From} | Users],
                {ok,State#state{users = UpdatedUsers}} 
        end,
    {reply, Reply, NewState}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

start_servers(0,_) ->
  ok;

start_servers(Num,LS) ->
  Nm = list_to_atom([Num | "tserv"]),  
  register(Nm,spawn(?MODULE,tcpserver,[LS,Nm])),  
  start_servers(Num-1,LS).

process(Decoded) ->
    #{<<"type">> := Tp} = Decoded,
    case Tp of
        <<"register">> ->
            #{<<"name">> := UserName} = Decoded,
            {message,register,UserName}
    end.        
    

loop(S,Nm) ->
  inet:setopts(S,[{active,once}]),
  receive
    {tcp,S,Data} ->
        Decoded = jsx:decode(Data,[return_maps]),
        Answer = process(Decoded),
        gen_server:call(srv,Answer), 

        receive
            ok -> 
                3
        end,
        
        io:format("Date -> ~p",[Answer]),

        % Not implemented in this example
        % gen_tcp:send(S,Answer),
        loop(S,Nm);
    {tcp_closed,S} ->
        io:format("Socket ~w closed [~w]~n",[S,self()]),
        ok
  end.

tcpserver(LS,Nm) ->
  case gen_tcp:accept(LS) of
    {ok,S} ->
      loop(S,Nm),
      tcpserver(LS,Nm);
    Other ->
      io:format("accept returned ~w - goodbye!~n",[Other]),
      ok
  end.





