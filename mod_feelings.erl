%% mod_js is a prototype module that uses erlang_js 
%% to allow filtering stanza's through JavaScript 
%% for altering, dropping, etc.
%%
%% NOTE: The JavaScript path is hardcoded to "test.js" 
%% at the end of this file.
%%
-module(mod_feelings).
-author('dev@falk-petersen.no').

-behaviour(gen_mod).
-export([start/2, stop/1, filter_packet/1, loop/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

%% Default path to load JavaScript files 
%% from.
-define(DEFAULT_JS_PATH, "/tmp/").

%% We run the JavaScript VM and code 
%% in a (single for now) separate process.
-define(JS_SERVICE, ?MODULE).

%% Global configuration
%% The js_path indicates where the module 
%% will look for JavaScript files. We need a 
%% way or convention to load the files and 
%% apply them in some order.
-record(config, {js_path}).

start(_Host, Opts) ->
	case whereis(?JS_SERVICE) of
		undefined ->
			?INFO_MSG("Loading module 'mod_feelings'", []),
			application:start(erlang_js),	
			JsPath = gen_mod:get_opt(js_path, Opts, ?DEFAULT_JS_PATH),
			ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 90),
			register(?JS_SERVICE, spawn(?MODULE, loop, [#config{js_path = JsPath}]));
		_ -> ok
    end.

stop(_Host) ->
	?INFO_MSG("Unloading module 'mod_feelings'", []),	
	ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 90),
	application:stop(erlang_js),
	ok.

%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
filter_packet(drop) -> drop;

filter_packet({From, To, Packet} = Input) ->
	?DEBUG("filter_packet(From: ~p, To: ~p, Packet: ~p, Result: ~p)", [From, To, Packet, Input]),
	
	%% It probably doesn't make any sense to block packets to oneself.
	%% If this is not out packet or ourselves we'll check_stanza and 
	%% either return drop, allow, or send another stanza instead
  	R = if From#jid.luser == To#jid.luser, From#jid.lserver == To#jid.lserver -> Input;
		true -> check_stanza(Input)
		end,

	case R of
		{drop, _} -> drop;
		{drop, _, _} -> drop;
		_ -> R
	end.

%% For now we only deal with "message" stanzas. We pull out the 
%% body and send it off to JavaScript so it can be dealt with. 
%% The message is then send with the new body.
%% 
check_stanza({From, To, {xmlelement, StanzaType, Attrs, Els} = Packet} = Input) ->
	case StanzaType of
		"message" ->						
			{_Subject, Body} = {
				case xml:get_path_s(Packet, [{elem, "subject"}, cdata]) of
               		false -> "";
               		Text -> Text
               	end,
               	xml:get_path_s(Packet, [{elem, "body"}, cdata])
			},
			
			Args = {
					struct, [
						{<<"from">>, unicode:characters_to_binary(jlib:jid_to_string(From))},
						{<<"to">>, unicode:characters_to_binary(jlib:jid_to_string(To))},
						{<<"body">>, unicode:characters_to_binary(Body)}
					]
			},
			
			?JS_SERVICE ! {self(), {<<"filter">>, [Args]}},
			receive
				{_Sender, {Result}} ->
					{struct, [{_, _}, {_, _}, {_, NewBody}]} = Result,
					[{xmlelement, Type2, Attrs2, Els2}] = Els,
					NewEls = lists:keyreplace(xmlcdata, 1, Els2, {xmlcdata, NewBody}),
					{From, To, {xmlelement, StanzaType, Attrs, [{xmlelement, Type2, Attrs2, NewEls}]}};
				_ ->
					?INFO_MSG("Failed to get JS reponse!", []),
					Input
			end;
		_ -> Input 
	end.

loop(Config) ->
	receive		
		{Sender, {Function, Data}} ->			
			{ok, JS} = js_driver:new(),
			{ok, Contents} = file:read_file(filename:join([Config#config.js_path, "test.js"])),
			js:define(JS, Contents),			
			{ok, Result} = js:call(JS, Function, Data),			
			Sender ! {self(), {Result}}, 
			loop(Config);
		stop ->
			?DEBUG("Stopping mod_feelings JavaScript service process.", []),
			exit(normal)
	end.
