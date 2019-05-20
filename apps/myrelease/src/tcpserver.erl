-module(tcpserver).

-behaviour(gen_server).

%% API
-export([start/1, stop/1, start_link/2,tcpserver/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(pos,{side,cord = 50}).
-record(player,{name,from,position}).
-record(game,{player1,player2}).
-record(state, {current = 0,users = [],games = #{},ids = #{}}).

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
    {ok, #state{}}.

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

get_opponent_pid(Pid,#game{player1 = #player{from = Pid},player2 = #player{from = Res}}) -> Res;
get_opponent_pid(Pid,#game{player1 = #player{from = Res},player2 = #player{from = Pid}}) -> Res.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    {Reply,NewState} =
        case Request of
            {message,register,UserName} ->

              io:format("i am here ~p",[UserName]),

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
                  GameId = State#state.current,
                  {_,{Pid1,_}} = NewUser,
                  {_,{Pid2,_}} = User,
                  io:format("Found ~p",[GameId]),
                  {ok,State#state{current = GameId + 1,users = Rest,games = maps:put(GameId,NewGame,Games),ids = maps:put(Pid1,GameId,maps:put(Pid2,GameId,State#state.ids))}}
              end;
            {message,move,Pos} ->
              {Pd,_} = From,
              GameId = maps:get(Pd,State#state.ids),
              Game = maps:get(GameId,State#state.games),
              OppPd = get_opponent_pid(Pd,Game),
              OppPd ! {from_server,move_req,Pos},
              {ok,State}
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

rev (<<>>, Acc) -> Acc;
rev (<<H:1/binary, Rest/binary>>, Acc) ->
    rev(Rest, <<H/binary, Acc/binary>>).

fetch_jsons(<<"}", Tail/binary>>, Accu,Res,1) ->
  Reversed = rev(<<<<"}">>/binary , Accu/binary>> , <<>>),
  R = [Reversed | Res],
  fetch_jsons(Tail,<<>>,R,0);
fetch_jsons(<<"}", Tail/binary>>, Accu,Res,Lvl) ->
  fetch_jsons(Tail,<<<<"}">>/binary , Accu/binary>>,Res,Lvl - 1); 
fetch_jsons(<<"{", Tail/binary>>, Accu,Res,Lvl) ->
  fetch_jsons(Tail,<<<<"{">>/binary , Accu/binary>>,Res,Lvl + 1);    
fetch_jsons(<<C, Tail/binary>>, Accu,Res,Lvl) ->
  fetch_jsons(Tail,<<<<C>>/binary , Accu/binary>>,Res,Lvl);
fetch_jsons(_, _,Res,_) ->
  Res.

process(Decoded) ->
    #{<<"tp">> := Tp,<<"content">> := Ct} = Decoded,

    case Tp of
        <<"register">> ->
          io:format("Content -> ~p",[Ct]),
          #{<<"name">> := UserName} = Ct,
          {message,register,UserName};
        <<"move_req">> ->
          #{<<"pos">> := Pos} = Ct,
          {message,move,Pos}
    end.        
    

loop(S,Nm) ->
  inet:setopts(S,[{active,once}]),
  receive
    {from_server,start_game,Side} ->
        Resp = jsx:encode(#{<<"tp">> => <<"game_init">>,<<"body">> => #{<<"side">> => atom_to_binary(Side,latin1)}}),
        gen_tcp:send(S,Resp),
        loop(S,Nm);
    {from_server,move_req,Pos} ->
        Resp = jsx:encode(#{<<"tp">> => <<"move_req">>,<<"body">> => #{<<"pos">> => Pos}}),
        gen_tcp:send(S,Resp),
        loop(S,Nm);
    {tcp,S,Data} ->
        Jsons = fetch_jsons(Data,<<>>,[],0),
        Messages = lists:map(fun (X) -> process(jsx:decode(X,[return_maps])) end,Jsons),
        lists:foreach(fun(Answer) -> gen_server:call(srv,Answer) end,Messages), 
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





