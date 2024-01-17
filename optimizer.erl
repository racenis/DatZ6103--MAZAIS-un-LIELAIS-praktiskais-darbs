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
			
			Job = {LastID, Process, Iter, -1, D},
			
			job_manager([Job|Running], Finished, Cancelled, LastID + 1);
			
		{stop_job, JobID} ->
			io:format("Stopping job: ~p~n", [JobID]),
			
			{_, Process, _, _, _} = lists:keyfind(JobID, 1, Running),
			
			exit(Process, terminated);
			
		{get_job_status, Sender, {From, To}} ->
			io:format("Getting job statuses: ~p--~p~n", [From, To]),
			
			R = lists:map(fun({ID, _, Iter, V, D})->{running, ID, Iter, D, V} end, Running),
			F = lists:map(fun({ID, Iter, D, _, S})->{finished, ID, Iter, D, S} end, Finished),
			C = lists:map(fun({ID, Iter, D, Re})->{Re, ID, Iter, D, -1} end, Cancelled),
			
			Sender ! {job_status, R ++ F ++ C};
		{get_job_result, Sender, JobID} ->
			io:format("Getting job result: ~p~n", [JobID]),
			
			{_, _, _, Solution, _} = lists:keyfind(JobID, 1, Finished),
			
			Sender ! {job_result, Solution};
		
		{job_finished, Sender, Solution, Score} ->
			io:format("Finished job: ~p Score: ~p~n", [Sender, Score]),
			
			{JobID, _, Iter, _, D} = lists:keyfind(Sender, 2, Running),
		
			Job = {JobID, Iter, D, Solution, Score},
			
			job_manager(Running, [Job|Finished], Cancelled, LastID);
			
		{job_iterated, Sender, Iters, Score} -> 
			{JobID, _, _, _, D} = lists:keyfind(Sender, 2, Running),
			Job = {JobID, Sender, Iters, Score, D},
			job_manager(lists:keyreplace(Sender, 2, Running, Job), Finished, Cancelled, LastID);
		
		{'EXIT', From, Reason} ->
			io:format("Exited job: ~p ~p~n", [From, Reason]),
			
			{JobID, _, Iter, _, D} = lists:keyfind(From, 2, Running),
			
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
		default -> fun hill_climber/2;
		stochastic -> fun stochastic/2;
		stochastic_parallel -> fun stochastic_parallel/2;
		metropolis -> fun metropolis/2;
		simulated_annealing -> fun simulated_annealing/2;
		genetic -> fun genetic/2
	end.
	
	
% Kalnā kāpējs.
hill_climber(Iterations, Initial) ->
	hill_climber(Iterations, Iterations, Initial, domain:get_solution_cost(Initial)).
hill_climber(0, FinalIterations, Solution, Score) ->
	whereis(job_manager) ! {job_iterated, self(), FinalIterations, Score},
	whereis(job_manager) ! {job_finished, self(), Solution, Score},
	exit(finished);
hill_climber(Iterations, FinalIterations, Solution, Score) ->
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	
	whereis(job_manager) ! {job_iterated, self(), Iterations, Score},
	
	case NewScore < Score of
		true -> hill_climber(Iterations - 1, FinalIterations, NewSolution, NewScore);
		false -> hill_climber(Iterations - 1, FinalIterations, Solution, Score)
	end.
		
% Stohastiskais	
stochastic(Iterations, Initial) ->
	stochastic(Iterations, Iterations, Initial, Initial, domain:get_solution_cost(Initial)).
stochastic(0, FinalIterations, _, BestSolution, BestScore) ->
	whereis(job_manager) ! {job_iterated, self(), FinalIterations, BestScore},
	whereis(job_manager) ! {job_finished, self(), BestSolution, BestScore},
	exit(finished);
stochastic(Iterations, FinalIterations, Solution, BestSolution, BestScore) ->
	whereis(job_manager) ! {job_iterated, self(), Iterations, BestScore},
	
	{NewScore, NewSolution} = stochastic_iterate(Solution),
	
	case NewScore < BestScore of
		true -> stochastic(Iterations - 1, FinalIterations, NewSolution, NewSolution, NewScore);
		false -> stochastic(Iterations - 1, FinalIterations, NewSolution, BestSolution, BestScore)
	end.
stochastic_iterate(Solution) ->
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	{NewScore, NewSolution}.

% Stohastiskais paralēlais
stochastic_parallel(Iterations, Initial) ->
	Normalized = Iterations div 4, % dalīts ar 4, jo katrā iterācijā būs 4 procesi
	stochastic_parallel(Normalized, Iterations, Initial, Initial, domain:get_solution_cost(Initial)).
stochastic_parallel(0, FinalIterations, E, BestSolution, BestScore) ->
	stochastic(0, FinalIterations, E, BestSolution, BestScore);
stochastic_parallel(Iterations, FinalIterations, Solution, BestSolution, BestScore) ->
	whereis(job_manager) ! {job_iterated, self(), Iterations*4, BestScore},
	
	Self = self(),
	
	spawn(fun()-> apply(fun stochastic_parallel_iterate/2, [Solution, Self]) end),
	spawn(fun()-> apply(fun stochastic_parallel_iterate/2, [Solution, Self]) end),
	spawn(fun()-> apply(fun stochastic_parallel_iterate/2, [Solution, Self]) end),
	spawn(fun()-> apply(fun stochastic_parallel_iterate/2, [Solution, Self]) end),

	stochastic_parallel_receive(Iterations, FinalIterations, Solution, BestSolution, BestScore, 4).
stochastic_parallel_receive(Iterations, FinalIterations, Solution, BestSolution, BestScore, 0)->
	stochastic_parallel(Iterations-1, FinalIterations, Solution, BestSolution, BestScore);
stochastic_parallel_receive(Iterations, FinalIterations, Solution, BestSolution, BestScore, Need)->
	{NewScore, NewSolution} = receive Any -> Any end,
	case NewScore < BestScore of
		true -> stochastic_parallel_receive(Iterations, FinalIterations, NewSolution, NewSolution, NewScore, Need-1);
		false -> stochastic_parallel_receive(Iterations, FinalIterations, Solution, BestSolution, BestScore, Need-1)
	end.
stochastic_parallel_iterate(Solution, Parent) ->
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	Parent ! {NewScore, NewSolution},
	ok.

% Metropolisa
metropolis(Iterations, Initial) ->
	InitialCost = domain:get_solution_cost(Initial),
	metropolis(Iterations, Iterations, Initial, InitialCost, {Initial, InitialCost}).
metropolis(0, FIter, _, _, {BestScore, BestSolution}) ->
	whereis(job_manager) ! {job_iterated, self(), FIter, BestScore},
	whereis(job_manager) ! {job_finished, self(), BestSolution, BestScore},
	exit(finished);
metropolis(Iterations, FIter, Solution, Score, {BestScore, BestSolution}) ->
	whereis(job_manager) ! {job_iterated, self(), Iterations, BestScore},
	
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	
	case NewScore < BestScore of
		true -> NewBest={NewScore, NewSolution};
		false -> NewBest={BestScore, BestSolution}
	end,
	
	case (Score/NewScore) > rand:uniform() of
		true -> metropolis(Iterations - 1, FIter, NewSolution, NewScore, NewBest);
		false -> metropolis(Iterations - 1, FIter, Solution, Score, NewBest)
	end.
	
% Simulētā apsaldēšana
simulated_annealing(Iterations, Initial) ->
	InitialCost = domain:get_solution_cost(Initial),
	simulated_annealing({1.0, 1.0/Iterations}, Iterations, Iterations, Initial, InitialCost, {Initial, InitialCost}).
simulated_annealing(_, 0, FIter, _, _, {BestScore, BestSolution}) ->
	whereis(job_manager) ! {job_iterated, self(), FIter, BestScore},
	whereis(job_manager) ! {job_finished, self(), BestSolution, BestScore},
	exit(finished);
simulated_annealing({Temp, Drop}, Iterations, FIter, Solution, Score, {BestScore, BestSolution}) ->
	whereis(job_manager) ! {job_iterated, self(), Iterations, BestScore},
	
	NewSolution = domain:get_modified_solution(Solution),
	NewScore = domain:get_solution_cost(NewSolution),
	
	case NewScore < BestScore of
		true -> NewBest={NewScore, NewSolution};
		false -> NewBest={BestScore, BestSolution}
	end,
	
	case ((Score/NewScore)*Temp) > rand:uniform() of
		true -> simulated_annealing({Temp - Drop, Drop}, Iterations - 1, FIter, NewSolution, NewScore, NewBest);
		false -> simulated_annealing({Temp - Drop, Drop}, Iterations - 1, FIter, Solution, Score, NewBest)
	end.
	
% Ģenētiskais algoritms
genetic(Iterations, Initial) ->
	genetic(Iterations div 8, Iterations, genetic_generate(32, [], Initial)).
genetic_generate(0, Solutions, _) ->
	Solutions;
genetic_generate(Left, Solutions, Initial) ->
	Solution = domain:get_modified_solution(Initial),
	Score = domain:get_solution_cost(Solution),
	genetic_generate(Left - 1, [{Score, Solution}|Solutions], Initial).
	
genetic(0, FinalIterations, Solutions) ->
	Sorted = lists:keysort(1, Solutions),
	{BestScore, BestSolution} = lists:nth(1, Sorted),
	whereis(job_manager) ! {job_iterated, self(), FinalIterations, BestScore},
	whereis(job_manager) ! {job_finished, self(), BestSolution, BestScore},
	exit(finished);
genetic(Iterations, FinalIterations, Solutions) ->
	Sorted = lists:keysort(1, Solutions),
	Yeeted = lists:sublist(Sorted, 8),

	NewSolutions = 
		genetic_generate(4, [], element(2, lists:nth(1, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(2, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(3, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(4, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(5, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(6, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(7, Yeeted))) ++
		genetic_generate(4, [], element(2, lists:nth(8, Yeeted))),
	
	{BestScore, _} = lists:nth(1, Yeeted),
	whereis(job_manager) ! {job_iterated, self(), Iterations*8, BestScore},
	
	genetic(Iterations - 1, FinalIterations, NewSolutions).