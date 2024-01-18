-module(app).

-export([start/0]).

% Aplikācijas palaidējs, lai varētu palaist visu.
% Tas ir domāts tikai lai tad kad no konsoles taisa visu viss sanāktu un tā.
start() ->
	optimizer:start_manager(),
	server:start(),
	io:format("Started!~n"),
	timer:sleep(infinity).