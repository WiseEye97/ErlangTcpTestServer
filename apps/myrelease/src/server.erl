-module(server).


-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([tcpserver/1]).

-record(state, {dummy}).


start(Name) ->
   start_link(Name).

stop(Name) ->
   gen_server:call(Name, stop).

start_link(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
   starttcp(5,7878),
   {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% inner functions

process(Date) ->
  binary_to_term(Date).

loop(S) ->
  inet:setopts(S,[{active,once}]),
  receive
    {tcp,S,Data} ->
      Answer = process(Data),
      io:format("Date -> ~p",[Answer]),
      % Not implemented in this example
      % gen_tcp:send(S,Answer),
      loop(S);
    {tcp_closed,S} ->
      io:format("Socket ~w closed [~w]~n",[S,self()]),
      ok
  end.

tcpserver(LS) ->
  case gen_tcp:accept(LS) of
    {ok,S} ->
      loop(S),
      tcpserver(LS);
    Other ->
      io:format("accept returned ~w - goodbye!~n",[Other]),
      ok
  end.

start_servers(0,_) ->
  ok;

start_servers(Num,LS) ->
  spawn(?MODULE,tcpserver,[LS]),
  start_servers(Num-1,LS).

starttcp(Num,LPort) ->
  case gen_tcp:listen(LPort,[{active, false},binary]) of
    {ok, ListenSock} ->
      start_servers(Num,ListenSock),
      {ok, Port} = inet:port(ListenSock),
      Port;
    {error,Reason} ->
      {error,Reason}
  end.