-module(server).

-export([start/0]).

-export([listen/1]).
-export([handle/1]).

-include("domain.hrl").

% Iestartē HTTP serveri.
start() ->
	io:format("Starting server...~n"),
	io:format("Opening socket...~n"),
	{ok, Socket} = gen_tcp:listen(8696, [{active, false}, list]),
	spawn(server, listen, [Socket]).

% Sagaida TCP savienojumus no socketa.
listen(Socket) ->
	io:format("Waiting for a connection...~n"),
	
	% gaida kad atnāks jauns savienojums
	{ok, Accept} = gen_tcp:accept(Socket),
	
	% palaiž jaunu procesu, kurš tālāk gaidīs nākamos savienojumus
	spawn(server, listen, [Socket]),
	
	% apstrādā šo savienojumu
	handle(Accept).
	
% Apstrādā TCP savienojumu.
handle(Socket) ->
	io:format("Accepted connection. Preparing response...~n"),
	
	% iestatām lai uzreiz atnāk viss teksts
	inet:setopts(Socket, [{active, once}]),
	
	% saņemam HTTP pieprasījumu caur TCP socketu
	receive
		{tcp, Socket, Msg} ->
			
			% sadalām HTTP pieprasījumu rindiņās
			Split = re:split(Msg, "\r\n|\n|\r", [{return, list}]),
			
			erlang:display(Split),
			
			% izvelkam ārā informāciju no headera
			Header = extract_header(Split, nil, nil, nil, nil),
			
			% sagatavojam HTTP atbildi
			Response = make_http(Header),
			
			% aizūtām atbildi atpakaļ un aizveram TCP savienojumu
			gen_tcp:send(Socket, Response),
			gen_tcp:close(Socket),
			
			io:format("Responded!.~n")
	end.

% Noparsē HTTP headeri.
extract_header([], Method, Path, Httpver, Req) -> {Method, Path, Httpver, Req};
extract_header([Line|Rest], Method, Path, Httpver, Req) ->
	Splt = re:split(Line, " ", [{return, list}]),
	
	[Param|_] = Splt,
	
	case Param of
		"GET" -> 
			extract_header(Rest, Param, lists:nth(2, Splt), lists:nth(3, Splt), Req);
		"POST" -> 
			extract_header(Rest, Param, lists:nth(2, Splt), lists:nth(3, Splt), lists:last(Rest));
		_ -> 
			extract_header(Rest, Method, Path, Httpver, Req)
	end.

% Sagatavo HTTP atbildi.
make_http({Method, Path, Httpver, Req}) ->
	case Path of
		"/" ->
			{ok, File} = file:read_file("web/index.html"),
			Code = "200 OK",
			Page = binary_to_list(File);
		"/jobs/" ->
			Code = "200 OK",
			Text = io_lib:format("~p",[optimizer:get_job_status({0, 100})]),
			Page = lists:flatten(Text);
		"/post/" ->
			Code = "200 OK",
			Page = "Thank you come again!",
			{ok,Tokens,_EndLine} = erl_scan:string(Req),
			{ok,AbsForm} = erl_parse:parse_exprs(Tokens),
			{value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),	
			optimizer:start_job(Value),
			io:format("Req:~p~n", [Value]);
		"/result/" ->
			Code = "200 OK",
			{ResultIndex, _} = string:to_integer(Req),
			#solution{snuksti=S, schedules=Sc} = optimizer:get_job_result(ResultIndex),
			
			SF = lists:concat(lists:map(fun(Sn) -> io_lib:format("~s,~s;",[Sn#snuksts.id, Sn#snuksts.name]) end, S)),
			
			_ = SF,
			SL = lists:map(fun({Snuk, Activ}) ->
				io:format("DOING A MAP!!!~n"),
			
				Act = lists:map(fun(#activity{type=T,building=B,index=I})->
					io_lib:format("~s,~s,~p;",[T,B,I])
				end, Activ),
				
				io_lib:format("~s:~s/",[Snuk, lists:concat(Act)]) 
			end, Sc),
			
			Page = SF ++ "///" ++ SL;
			%Page = io_lib:format("~p ; ~p",[SF, Sc]);
		_ ->
			{ok, File} = file:read_file("web/not_found.html"),
			Code = "404 OK",
			Page = binary_to_list(File)
	end,
	
	_ = Httpver,
	_ = Method,
	
	"HTTP/1.1 " ++ Code ++ "\nContent-Type: text/html; charset=utf-8\n\n" ++ Page.