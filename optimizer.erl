-module(optimizer).

-export([start_manager/0]).
-export([start_job/1]).
-export([stop_job/1]).
-export([get_job_status/1]).
-export([get_job_result/1]).

% šis tikai lai varētu palaist procesu
-export([job_manager/4]).

-include("optimizer.hrl").

% +----------------------------------------------------------------------------+
% |                                                                            |
% |                      OPTIMIZĀCIJAS DARBU PĀRVALDNIEKS                      |
% |                                                                            |
% +----------------------------------------------------------------------------+

start_manager() ->
	io:format("Starting manager...~n"),
	
	% pārbaudām vai darbu pārvaldnieks ir jau iestartēts, un ja ir, tad izslēdzam
	ExistingManager = whereis(job_manager),
	if
		ExistingManager == undefined -> ok;
		true -> exit(ExistingManager, yeet)
	end,
	
	% iestartējam darbu pārvaldnieku
	JobManager = spawn(optimizer, job_manager, [[], [], [], 1]),
	
	% piereģistrējam to
	register(job_manager, JobManager).

start_job(Job) ->
	whereis(job_manager) ! {start_job, Job}, ok.
	
stop_job(JobID) ->
	whereis(job_manager) ! {stop_job, JobID}, ok.

get_job_status(Range) ->
	whereis(job_manager) ! {get_job_status, self(), Range},
	receive
		{job_status, Result} -> Result
	after 1000 -> [] end.

get_job_result(JobID) ->
	whereis(job_manager) ! {get_job_result, self(), JobID},
	receive
		{job_result, Result} -> Result
	after 1000 -> [] end.
	
% Darbu pārvaldes process.
job_manager(Running, Finished, Cancelled, LastID) ->

	% neļausim nogalināt pārvaldnieku ja kāds no viņa darbiem nomirst
	process_flag(trap_exit, true),
	
	receive
		{start_job, #job_params{optimizer=Opt, iters=Iter, snuksti=S, desc=D}} -> 
			io:format("Starting job: ~p~n", [D]),
			
			InitialSolution = domain:get_initial_solution(S),
			ProcessFunction = fun() -> apply(get_optimizer(Opt), [Iter, InitialSolution]) end,
			Process = spawn(ProcessFunction),
			
			link(Process),
			
			Job = {LastID, Process, Iter, D},
			
			job_manager([Job|Running], Finished, Cancelled, LastID + 1);
			
		{stop_job, JobID} ->
			io:format("Stopping job: ~p~n", [JobID]),
			
			{_, Process, _, _} = lists:keyfind(JobID, 1, Running),
			
			exit(Process, terminated);
			
		{get_job_status, Sender, {From, To}} ->
			io:format("Getting job statuses: ~p--~p~n", [From, To]),
			
			R = lists:map(fun({ID, _, Iter, D})->{running, ID, Iter, D, -1} end, Running),
			F = lists:map(fun({ID, Iter, D, _, S})->{finished, ID, Iter, D, S} end, Finished),
			C = lists:map(fun({ID, Iter, D, Re})->{Re, ID, Iter, D, -1} end, Cancelled),
			
			Sender ! {job_status, R ++ F ++ C};
		{get_job_result, Sender, JobID} ->
			io:format("Getting job result: ~p~n", [JobID]),
			
			{_, _, _, Solution, _} = lists:keyfind(JobID, 1, Finished),
			
			Sender ! {job_result, Solution};
		
		{job_finished, Sender, Solution, Score} ->
			io:format("Finished job: ~p Score: ~p~n", [Sender, Score]),
			
			{JobID, _, Iter, D} = lists:keyfind(Sender, 2, Running),
		
			Job = {JobID, Iter, D, Solution, Score},
			
			job_manager(Running, [Job|Finished], Cancelled, LastID);
			
		{job_iterated, Sender, Iters} -> 
			{JobID, _, _, D} = lists:keyfind(Sender, 2, Running),
			Job = {JobID, Sender, Iters, D},
			job_manager(lists:keyreplace(Sender, 2, Running, Job), Finished, Cancelled, LastID);
		
		{'EXIT', From, Reason} ->
			io:format("Exited job: ~p ~p~n", [From, Reason]),
			
			{JobID, _, Iter, D} = lists:keyfind(From, 2, Running),
			
			case Reason == finished of
				true -> 
					job_manager(lists:keydelete(JobID, 1, Running), Finished, Cancelled, LastID);
				false ->
					Job = {JobID, Iter, D, Reason},
					job_manager(lists:keydelete(JobID, 1, Running), Finished, [Job|Cancelled], LastID)
			end
		
	end,
	
	job_manager(Running, Finished, Cancelled, LastID).
	
% +----------------------------------------------------------------------------+
% |                                                                            |
% |                                OPTIMIZATORI                                |
% |                                                                            |
% +----------------------------------------------------------------------------+
	
get_optimizer(Type) ->
	case Type of
		default -> fun hill_climber/2
	end.
	
hill_climber(Iterations, Initial) ->
	hill_climber(Iterations, Initial, domain:get_solution_cost(Initial)).
hill_climber(0, Solution, Score) ->
	whereis(job_manager) ! {job_finished, self(), Solution, Score},
	exit(finished);
hill_climber(Iterations, Solution, Score) ->
	%io:format("Hill climber! Iterations: ~p Score: ~p~n", [Iterations, Score]),
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	case NewScore < Score of
		true -> hill_climber(Iterations - 1, NewSolution, NewScore);
		false -> hill_climber(Iterations - 1, Solution, Score)
	end.
		