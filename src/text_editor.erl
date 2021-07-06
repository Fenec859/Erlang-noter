-module(text_editor).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).
-export([start/2, stop/0, file_open/0, file_open_new/0, file_save/0, file_save_as/0]).  %% API
-export([init/1, handle_call/3, handle_cast/2, handle_event/2, terminate/2]). % Callbacks
 
%% Client API
start(Env, ParentWindow) ->
	wx_object:start({local, text_editor}, ?MODULE, [ParentWindow, Env], []).

 
stop() ->
	wx_object:cast(text_editor, stop).
 

file_open_new() ->
	wx_object:cast(text_editor, file_open_new).

file_open() -> 
	wx_object:cast(text_editor, file_open).

file_save() -> 
	wx_object:cast(text_editor, file_save).

file_save_as() -> 
	wx_object:cast(text_editor, file_save_as).


%% Server Implementation ala gen_server
init([ParentWindow, Env]) ->
	wx:set_env(Env),

	Panel = wxPanel:new(ParentWindow, []),
	wxWindow:setBackgroundColour(Panel, {100, 100, 100}),
	Editor = wxTextCtrl:new(Panel, ?wxID_ANY, [{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
	

	VerSizer = wxBoxSizer:new(?wxVERTICAL),
	HorSizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(HorSizer, Editor, [{flag, ?wxEXPAND}, {proportion, 1}]),
	wxSizer:add(VerSizer, HorSizer, [{flag, ?wxEXPAND}, {proportion, 1}]),

	wxPanel:setSizer(Panel, VerSizer),

	CurrentFilePath = none,
	
	{Editor, {ParentWindow, Env, Editor, CurrentFilePath}}.
 



handle_call(Message, _From, State) ->
	io:format("Text editor recieved a call: ~p ~n", [Message]),
	{reply, ok, State}.






handle_cast(file_open_new, {ParentWindow, Env, Editor, _CurrenFilePath}) ->
	wxTextCtrl:clear(Editor),
	{noreply, {ParentWindow, Env, Editor, none}};

handle_cast(file_open, {ParentWindow, Env, Editor, _CurrenFilePath}) ->
	FilePath = get_file_path_open(ParentWindow),
	wxTextCtrl:loadFile(Editor, FilePath),
	{noreply, {ParentWindow, Env, Editor, FilePath}};

handle_cast(file_save, State = {_PW, _Env, _E, none}) ->
	file_save_as(),
	{noreply, State};

handle_cast(file_save, State = {_PW, _Env, Editor, CurrentFilePath}) ->
	wxTextCtrl:saveFile(Editor, [{file, CurrentFilePath}]),
	{noreply, State};

handle_cast(file_save_as, {ParentWindow, Env, Editor, _CurrentFilePath}) ->
	FilePath = get_file_path_save(ParentWindow),
	wxTextCtrl:saveFile(Editor, [{file, FilePath}]),
	{noreply, {ParentWindow, Env, Editor, FilePath}};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Message, State) ->
	io:format("Text editor recieved a cast: ~p ~n", [Message]),
	{noreply, State}.



 
handle_event(Event, State) ->
	io:format("Text editor recieved an event: ~p ~n", [Event]),
	{noreply, State}.

	
terminate(_Reason, _State) ->
	ok.


get_file_path_save(ParentWindow) -> 
	FileDialog = wxFileDialog:new(ParentWindow, [{style, ?wxFD_SAVE bor ?wxFD_CHANGE_DIR bor ?wxFD_OVERWRITE_PROMPT}]),
	wxDialog:showModal(FileDialog),
	wxFileDialog:getPath(FileDialog).

get_file_path_open(ParentWindow) -> 
	FileDialog = wxFileDialog:new(ParentWindow, [{style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST bor ?wxFD_CHANGE_DIR}]),
	wxDialog:showModal(FileDialog),
	wxFileDialog:getPath(FileDialog).