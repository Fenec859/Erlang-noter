-module(menu_bar).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).
-export([start/2, stop/0, fill/0, clear/0]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, terminate/2]). % Callbacks
 
%% Client API
start(Env, ParentWindow) ->
io:format("menu bar starts ~n"),
	wx_object:start({local, menu_bar}, ?MODULE, [ParentWindow, Env], []).

 
stop() ->
	wx_object:cast(menu_bar, stop).

fill() ->
	wx_object:call(menu_bar, fill).

clear() ->
	wx_object:call(menu_bar, clear).




%% Server Implementation ala gen_server
init([ParentWindow, Env]) ->
	io:format("Menu bar starts~n"),
	wx:set_env(Env),


	MenuBar = wxMenuBar:new(),
	wxFrame:setMenuBar(ParentWindow, MenuBar),

	{FileMenu, Buttons} = make_contents(),
	set_contents(MenuBar, FileMenu),

	{MenuBar, {ParentWindow, Env, MenuBar, Buttons}}.



handle_call(clear, _From, State = {_PW, _Env, MenuBar, _B}) ->
	clear_contents(MenuBar),
	{reply, ok, State};

handle_call(fill, _From, {PW, Env, MenuBar, _B}) ->
	{FileMenu, Buttons} = make_contents(),
	set_contents(MenuBar, FileMenu),
	{reply, ok, {PW, Env, MenuBar, Buttons}};
 
handle_call(Message, _From, State) ->
	io:format("Menu bar recieved a call: ~p ~n", [Message]),
	{reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Message, State) ->
	io:format("Menu bar recieved a cast: ~p ~n", [Message]),
	{noreply, State}.

handle_event({wx, Id, _O, _UD, _E}, State = {_PW, _Env, MenuBar, _B}) ->
	% io:format("Menu bar recieved an event: ~p ~n", [RawEvent]),
	% io:format("Buttons: ~p ~n", [Buttons]),
	Button = wxMenuBar:findItem(MenuBar, Id),
	Label = wxMenuItem:getLabel(Button),
	% io:format("Button found: ~p ~n ~p~n", [Button, Label]),
	dispatch_button(Label, State),
	{noreply, State};

 
handle_event(Event, State) ->
	io:format("Menu bar recieved an event: ~p ~n", [Event]),
	{noreply, State}.

	
terminate(_Reason, _State) ->
	ok.



%% Internal functions
make_contents() -> 

	FileMenu = wxMenu:new(),

	NewFile = wxMenu:append(FileMenu, ?wxID_ANY, "New"),
	OpenFile = wxMenu:append(FileMenu, ?wxID_ANY, "Open..."),
	SaveFile = wxMenu:append(FileMenu, ?wxID_ANY, "Save"),
	SaveAsFile = wxMenu:append(FileMenu, ?wxID_ANY, "Save as..."),
	Quit = wxMenu:append(FileMenu, ?wxID_ANY, "Quit"),

	Buttons = {NewFile, OpenFile, SaveFile, SaveAsFile, Quit},
	io:format("Buttons: ~p~n", [Buttons]),
	{FileMenu, Buttons}.

set_contents(MenuBar, FileMenu) ->

	wxMenuBar:append(MenuBar, FileMenu, "File"),
	wxMenuBar:connect(MenuBar, command_menu_selected).

clear_contents(MenuBar) ->
	wxMenuBar:remove(MenuBar, 0).





dispatch_button("New", _S) ->
	io:format("New was clicked ~n"),
	noter:file_open_new();


dispatch_button("Open...", _S) ->
	noter:file_open();

dispatch_button("Save", _S) ->
	noter:file_save();


dispatch_button("Save as...", _S) ->
	noter:file_save_as();


dispatch_button("Quit", _S) ->
	noter:stop().

