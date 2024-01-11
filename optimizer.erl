-module(optimizer).

-export([start_manager/0]).
-export([start_job/1]).
-export([stop_job/1]).
-export([get_job_status/1]).
-export([get_job_result/1]).

% šis tikai lai varētu palaist procesu
-export([job_manager/3]).

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
		true -> exit(ExistingManager, yeet)
	end,
	
	% iestartējam darbu pārvaldnieku
	JobManager = spawn(optimizer, job_manager, [[], [], []]),
	
	% piereģistrējam to
	register(job_manager, JobManager),
	%erlang:display(registered()),
	%erlang:display(JobManager),
	%erlang:display(registered()),
	erlang:display("oke").


%get_test_snuksti()
	
start_job(Job) ->
	whereis(job_manager) ! {start_job, Job}, ok.
	
stop_job(JobID) ->
	whereis(job_manager) ! {stop_job, JobID}, ok.

% TODO: pielikt saņemšanu
get_job_status(Range) ->
	whereis(job_manager) ! {get_job_status, self(), Range}.

% TODO: pielikt saņemšanu
get_job_result(JobID) ->
	whereis(job_manager) ! {get_job_result, self(), JobID}.
	
% Darbu pārvaldes process.
job_manager(Running, Finished, Cancelled) ->
	%erlang:display("This is forom job manager"),
	%erlang:display(Running),

	% neļausim nogalināt pārvaldnieku ja kāds no viņa darbiem nomirst
	process_flag(trap_exit, true),
	
	receive
		{start_job, #job_params{optimizer=Opt, iters=Iter, snuksti=S, desc=D}} -> 
			io:format("Starting job: ~p~n", [D]),
			
			InitialSolution = domain:get_initial_solution(S),
			ProcessFunction = fun() -> apply(get_optimizer(Opt), [Iter, InitialSolution]) end,
			Process = spawn(ProcessFunction),
			
			link(Process),
			
			Job = {123, Process, Iter, D},
			
			job_manager([Job|Running], Finished, Cancelled);
			
		{stop_job, JobID} ->
			io:format("Stopping job: ~p~n", [JobID]),
			
			{_, Process, _, _} = lists:keyfind(JobID, 1, Running),
			
			exit(Process, terminated);
			
			
			
			%job_manager(lists:keydelete(JobID, 1, Running), [NewJob|Finished]);
		{get_job_status, Sender, {From, To}} ->
			io:format("Getting job statuses: ~p--~p~n", [From, To])
			
			;
		{get_job_result, Sender, JobID} -> io:format("Getting job result: ~p~n", [JobID]);
		
		{job_finished, Sender, Solution, Score} ->
			io:format("Finished job: ~p Score: ~p~n", [Sender, Score]),
			
			{JobID, _, Iter, D} = lists:keyfind(Sender, 2, Running),
		
			Job = {JobID, Iter, D, Solution, Score},
			
			job_manager(Running, [Job|Finished], Cancelled);
			
		{job_iterated, Sender, Iters} -> 
			{JobID, _, _, D} = lists:keyfind(Sender, 2, Running),
			Job = {JobID, Sender, Iters, D},
			job_manager(lists:keyreplace(Sender, 2, Running, Job), Finished, Cancelled);
		
		{'EXIT', From, Reason} ->
			io:format("Exited job: ~p ~p~n", [From, Reason]),
			
			{JobID, _, Iter, D} = lists:keyfind(From, 2, Running),
			
			case Reason == finished of
				true -> 
					job_manager(lists:keydelete(JobID, 1, Running), Finished, Cancelled);
				false ->
					Job = {JobID, Iter, D, Reason},
					job_manager(lists:keydelete(JobID, 1, Running), Finished, [Job|Cancelled])
			end
		
	end,
	
	% spawn_link()
	
	job_manager(Running, Finished, Cancelled).
	
	
get_optimizer(Type) ->
	case Type of
		default -> fun hill_climber/2
	end.
	
hill_climber(Iterations, Initial) ->
	hill_climber(Iterations, Initial, domain:get_solution_cost(Initial)).
hill_climber(0, Solution, Score) ->
	whereis(job_manager) ! {job_finished, self(), Solution, Score},
	exit(finished);
%hill_climber(Iterations, Solution, Score) ->
%	hill_climber(Iterations - 1, Solution, Score).
hill_climber(Iterations, Solution, Score) ->
	io:format("Hill climber! Iterations: ~p Score: ~p~n", [Iterations, Score]),
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	case NewScore < Score of
		true -> hill_climber(Iterations - 1, NewSolution, NewScore);
		false -> hill_climber(Iterations - 1, Solution, Score)
	end.
		