-module(optimizer).

-export([start_manager/0]).
-export([start_job/1]).
-export([stop_job/1]).

% šis tikai lai varētu palaist procesu
-export([job_manager/2]).

-include("optimizer.hrl").

% +----------------------------------------------------------------------------+
% |                                                                            |
% |                      OPTIMIZĀCIJAS DARBU PĀRVALDNIEKS                      |
% |                                                                            |
% +----------------------------------------------------------------------------+

start_manager() ->
	erlang:display("startging manaager"),
	
	% pārbaudām vai darbu pārvaldnieks ir jau iestartēts, un ja ir, tad izslēdzam
	ExistingManager = whereis(job_manager),
	if
		ExistingManager == undefined -> ok;
		true -> ExistingManager ! kill
	end,
	
	% iestartējam darbu pārvaldnieku
	JobManager = spawn(optimizer, job_manager, [[], []]),
	
	% piereģistrējam to
	register(job_manager, JobManager),
	erlang:display(registered()),
	erlang:display(JobManager),
	erlang:display(registered()),
	erlang:display("oke").


%get_test_snuksti()
	
start_job(Job) ->
	whereis(job_manager) ! {start_job, Job}.
	
stop_job(JobID) ->
	whereis(job_manager) ! {stop_job, JobID}.

% TODO: pielikt saņemšanu
get_job_status(Range) ->
	whereis(job_manager) ! {get_job_status, self(), Range}.

% TODO: pielikt saņemšanu
get_job_result(JobID) ->
	whereis(job_manager) ! {get_job_result, self(), JobID}.
	
% Darbu pārvaldes process.
job_manager(Running, Finished) ->
	erlang:display("This is forom hob manager"),
	erlang:display(Running),

	% neļausim nogalināt pārvaldnieku ja kāds no viņa darbiem nomirst
	process_flag(trap_exit, true),
	
	receive
		{start_job, #job_params{optimizer=Opt, iters=Iter, snuksti=S, desc=D}} -> 
			io:format("Starting job: ~p~n", [D]),
			
			InitialSolution = domain:get_initial_solution(S),
			ProcessFunction = fun() -> apply(get_optimizer(Opt), [Iter, InitialSolution]) end,
			
			Job = {123, link(spawn(ProcessFunction)), D},
			
			job_manager([Job|Running], Finished);
			
		{stop_job, JobID} ->
			io:format("Stopping job: ~p~n", [JobID]),
			
			{_, Process, _} = lists:keyfind(JobID, 1, Running),
			
			exit(Process, terminated);
			
			
			
			%job_manager(lists:keydelete(JobID, 1, Running), [NewJob|Finished]);
		{get_job_status, Sender, {From, To}} ->
			io:format("Getting job statuses: ~p--~p~n", [From, To])
			
			;
		{get_job_result, Sender, JobID} -> io:format("Getting job result: ~p~n", [JobID]);
		
		
		{'EXIT', From, Reason} ->
			io:format("Finished job: ~p ~p~n", [From, Reason])
		
	end,
	
	% spawn_link()
	
	job_manager(Running, Finished).
	
	
get_optimizer(Type) ->
	case Type of
		default -> fun hill_climber/2
	end.
	
hill_climber(Iterations, Initial) ->
	ok.