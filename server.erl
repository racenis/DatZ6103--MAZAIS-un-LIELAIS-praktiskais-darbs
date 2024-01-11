-module(server).

-export([start/0]).
-export([stop/0]).

-export([listen/1]).
-export([handle/1]).

start() ->
	io:format("Starting server...~n"),
	io:format("Opening socket...~n"),
	{ok, Socket} = gen_tcp:listen(8696, [{active, false}, list]),
	spawn(server, listen, [Socket])
	%Server = spawn(server, listen, [Socket])
	%register(web_server, Server)
.
stop() ->
	exit(whereis(web_server)).



listen(Socket) ->
	io:format("Waiting for a connection...~n"),
	
	{ok, Accept} = gen_tcp:accept(Socket),
	
	spawn(server, listen, [Socket]),
	
	handle(Accept).
	
	
handle(Socket) ->
	io:format("Awating response...~n"),
	
	inet:setopts(Socket, [{active, once}]),
	
	receive
		{tcp, Socket, Msg} ->
			Splt = re:split(Msg, "\r\n|\n|\r", [{return, list}]),
			%io:format("~p~n", [Splt]),
			io:format("~p~n", [extract_header(Splt, nil, nil, nil, nil)]),
			Response = make_http(extract_header(Splt, nil, nil, nil, nil)),
			%io:format("Sending response: ~p~n", [Response]),
			gen_tcp:send(Socket, Response),
			gen_tcp:close(Socket)
	end,
	
	handle(Socket).

extract_header([], Method, Path, Httpver, Req) -> {Method, Path, Httpver, Req};
extract_header([Line|Rest], Method, Path, Httpver, Req) ->
	Splt = re:split(Line, " ", [{return, list}]),
	
	io:format("Line:~p~n", [Splt]),
	
	[Param|_] = Splt,
	
	case Param of
		"GET" -> 
			extract_header(Rest, Param, lists:nth(2, Splt), lists:nth(3, Splt), Req);
		"POST" -> 
			extract_header(Rest, Param, lists:nth(2, Splt), lists:nth(3, Splt), lists:last(Rest));
		_ -> 
			extract_header(Rest, Method, Path, Httpver, Req)
	end.

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
			Text = io_lib:format("~p",[optimizer:get_job_result(string:to_integer(Req))]),
			Page = lists:flatten(Text);
		_ -> Code = "200 OK", Page = "ES ESMU CHUNGUUUS"
	end,
	
	_ = Httpver,
	_ = Method,
	
	"HTTP/1.1 " ++ Code ++ "\nContent-Type: text/html; charset=utf-8\n\n" ++ Page.