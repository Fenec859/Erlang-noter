-module(noter).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).
-export([start/0, stop/0, restart/0, start_mod/1, stop_mod/1, restart_mod/1]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, handle_info/2, terminate/2]). % Callbacks
-export([file_open/0, file_open_new/0, file_save/0, file_save_as/0]).  %% Private functions
 
%% Client API
start() ->

	wx_object:start({local, noter}, ?MODULE, [], []).

 
stop() ->
	wx_object:cast(noter, stop).

restart() ->
	try stop() of
		_ -> ok
	catch
		error:{noproc, _Body} -> ok;
		error:{normal, _Body} -> ok
	end,
	start().
 

start_mod(Mod) ->
	wx_object:call(noter, {start, Mod}).
stop_mod(Mod) ->
	wx_object:call(noter, {stop, Mod}).
restart_mod(Mod) -> 
 	stop_mod(Mod),
 	start_mod(Mod).


%% Server Implementation ala gen_server
init(_Args) ->
	io:format("Noter starts ~n"),
	% process_flag(trap_exit, true),
	% wx:debug([verbose]),

	Server = wx:new(),
	Env = wx:get_env(),
	MainWindow = wxFrame:new(Server, ?wxID_ANY, "Noter", [{size, {800, 600}}]),

	_TextEditor = text_editor:start(Env, MainWindow),
	_MainBar = menu_bar:start(Env, MainWindow),

	wxFrame:show(MainWindow),

	io:format("State: ~p~n ~p~n ~p~n", [Server, Env, MainWindow]),
	{MainWindow, {Server, Env, MainWindow}}.




handle_call({start, Mod}, _From, State = {_S, Env, MainWindow}) ->
	io:format("Noter starting: ~p ~n", [Mod]),
	Mod:start(Env, MainWindow),
	{reply, ok, State};




handle_call(Message, _From, State) ->
	io:format("Noter recieved a call: ~p ~n", [Message]),
	{reply, ok, State}.



handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Message, State) ->
	io:format("Noter recieved a cast: ~p ~n", [Message]),
	{noreply, State}.
 
 
handle_event(Event, State) ->
	io:format("Noter recieved an event: ~p ~n", [Event]),
	{noreply, State}.

handle_info(Info, State) ->
	io:format("Noter recieved an info: ~p ~n", [Info]),
	{noreply, State}.
	
terminate(_Reason, _State = {_S, _Env, MainWindow}) ->
	destroy_children([menu_bar, text_editor]),
	io:format("Noter stops ~n"),
	wxWindow:destroy(MainWindow),
	init:stop().




% Private functions
destroy_children([]) ->
	ok; 
destroy_children([Mod|Children]) -> 
	Mod:stop(),
	destroy_children(Children).


file_open_new() -> 
	text_editor:file_open_new().

file_open() -> 
	text_editor:file_open().

file_save() -> 
	text_editor:file_save().

file_save_as() -> 
	text_editor:file_save_as().