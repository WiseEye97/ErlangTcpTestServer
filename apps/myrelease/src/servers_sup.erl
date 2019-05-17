-module(servers_sup).

-export([start_link/0]).

-export([init/1]).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Childs = 
        case gen_tcp:listen(7878,[{active, false},binary]) of
            {ok, ListenSock} ->
                %%start_servers(Num,ListenSock),
                {ok, Port} = inet:port(ListenSock),
                [#{id => serv1,start => {tcpserver,start_link,[srv]},type => worker}];
            {error,Reason} ->
                {error,Reason},
                []
        end,
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    {ok, {SupFlags, Childs}}.