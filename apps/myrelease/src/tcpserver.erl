-module(tcpserver).

-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/2,tcpserver/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(pos,{side,cord = 50}).
-record(player,{name,from,position}).
-record(game,{player1,player2}).
-record(state, {users = [],games = []}).

start(Name) ->
   start_link(Name,5).

stop(Name) ->
   gen_server:call(Name, stop).

start_link(Name,LS) ->
   gen_server:start_link({local, Name}, ?MODULE, [LS], []).

init(_) ->
    {ok,LS} =
        case gen_tcp:listen(7878,[{active, false},binary]) of
            {ok, ListenSock} ->
                {ok, _} = inet:port(ListenSock),
                {ok,ListenSock};
            {error,Reason} ->
                {error,Reason}
        end,
    start_servers(5,LS),
    {ok, #state{users = []}}.

find_user([]) ->
  no_user;
find_user([User | Rest]) ->
  {found , User, Rest}.

init_game({_,{Pid,_}},Side) ->
  Pid!{from_server,start_game,Side}.

create_position(up) -> #pos{side = up};
create_position(down) -> #pos{side = down}.

create_game({{Name1,{Pid1,_}},Side1},{{Name2,{Pid2,_}},Side2}) ->
  P1 = #player{name = Name1,from = Pid1,position = create_position(Side1)},
  P2 = #player{name = Name2,from = Pid2,position = create_position(Side2)},
  #game{player1 = P1,player2 = P2}.


handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    {Reply,NewState} =
        case Request of
            {message,register,UserName} ->

              Users = State#state.users,
              NewUser = {UserName,From},

              case find_user(Users) of
                no_user ->
                  UpdatedUsers = [NewUser | Users],
                  {ok,State#state{users = UpdatedUsers}};
                {found , User,Rest} ->
                  Games = State#state.games,
                  init_game(User,up),
                  init_game(NewUser,down),
                  NewGame = create_game({User,up},{NewUser,down}),
                  {ok,State#state{users = Rest,games = [NewGame | Games]}}
              end
  
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
    #{<<"tp">> := Tp,<<"content">> := Ct} = Decoded,

    case Tp of
        <<"register">> ->
            io:format("Content -> ~p",[Ct]),
            #{<<"name">> := UserName} = Ct,
            {message,register,UserName}
    end.        
    

loop(S,Nm) ->
  inet:setopts(S,[{active,once}]),
  receive
    {from_server,start_game,Side} ->
        Resp = jsx:encode(#{<<"tp">> => <<"game_init">>,<<"content">> => #{<<"side">> => atom_to_list(Side)}}),
        gen_tcp:send(S,Resp),
        loop(S,Nm);
    {tcp,S,Data} ->
        Decoded = jsx:decode(Data,[return_maps]),
        Answer = process(Decoded),
        gen_server:call(srv,Answer), 
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





