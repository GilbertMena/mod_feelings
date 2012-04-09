%%%----------------------------------------------------------------------
%%% File    : mod_shit.erl
%%% Author  : Magnus Henoch <henoch@dtek.chalmers.se>
%%% Purpose : remove bad words from messages
%%% Created : 13 Oct 2006 by Magnus Henoch <henoch@dtek.chalmers.se>
%%% Id      : $Id$
%%%----------------------------------------------------------------------

%% To compile this module, put it in the ejabberd/src directory,
%% adjust the bad_words function, and compile ejabberd as usual.

%% To use this module, add it to the modules section of the
%% configuration, just like most other modules.

-module(mod_shit).
-author('henoch@dtek.chalmers.se').
-vsn('$Revision$ ').

-behaviour(gen_mod).

-export([start/2, stop/1,
	filter_packet/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

bad_words() ->
    ["fuck", "shit"].

start(_Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 100).

stop(_Host) ->
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 100).

%% Return drop to drop the packet, or the original input to let it through.
%% From and To are jid records.
filter_packet(drop) ->
    drop;
filter_packet({From, To, Packet}) ->
    NewPacket =
	case Packet of
	    {xmlelement, "message", Attrs, Els} ->
		{xmlelement, "message", Attrs, filter_message(Els)};
	    _ ->
		Packet
	end,
    {From, To, NewPacket}.

filter_message([{xmlelement, Name, Attrs, Els} | Tail]) ->
    NewEls =
	case Name of
	    "subject" ->
		[{xmlcdata, filter_string(xml:get_cdata(Els))}];
	    "body" ->
		[{xmlcdata, filter_string(xml:get_cdata(Els))}];
	    _ ->
		Els
	end,
    [{xmlelement, Name, Attrs, NewEls} | filter_message(Tail)];
filter_message([_ | Tail]) ->
    filter_message(Tail);
filter_message([]) ->
    [].

filter_string(String) ->
    BadWords = bad_words(),
    lists:foldl(fun filter_out_word/2, String, BadWords).

filter_out_word(Word, String) ->
    {ok, NewString, _} = regexp:gsub(String, Word, string:chars($\*, length(Word))),
    NewString.
