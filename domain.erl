-module(domain).

-export([get_all_buildings/0]).
-export([get_building/1]).
-export([get_distance/2]).
-export([get_mined/2]).
-export([get_solution_cost/1]).
-export([get_test_solution/0]).
-export([get_test_snuksti/0]).
-export([get_modified_solution/1]).
-export([get_initial_solution/1]).

-include("domain.hrl").

% Domēna apraksts, izmaksu funkcijas.

% Atdod sarakstu ar visiem korpusiem.
get_all_buildings() ->	[admin, corp1, corp2, corp2a].

% Atdod korpusa informāciju.
get_building(admin) ->	#building{id=admin,name="Administrācija",frogslots=0};
get_building(corp1) ->	#building{id=corp1,name="Korpuss 1",frogslots=10};
get_building(corp2) ->	#building{id=corp2,name="Korpuss 2",frogslots=20};
get_building(corp2a) ->	#building{id=corp2a,name="Korpuss 2A",frogslots=5}.

% Atdod attālumus starp korpusiem.
get_distance(admin, corp1) -> 50;
get_distance(admin, corp2) -> 50 + get_distance(corp1, corp2);
get_distance(admin, corp2a) ->  50 + get_distance(corp1, corp2a);

get_distance(corp1, corp2) -> 15;
get_distance(corp1, corp2a) -> 25;

get_distance(corp2, corp2a) -> 5;

get_distance(A, B) when A == B -> 1;
get_distance(A, B) -> get_distance(B, A).

% Atdod vai ceļš starp korpusiem ir mīnēts.
get_mined(corp2, corp2a) -> true;
get_mined(_, _) -> false.



get_test_solution() ->
	Snuksts1 = #snuksts{id=pootis, name="Pootis", unavailability=[{1000, 1200}, {1500, 1700}]},
	Snuksts2 = #snuksts{id=painis, name="Painis", unavailability=[{0, 500}, {500, 750}]},
	Snuksti = [Snuksts1, Snuksts2],
	Activity1 = #activity{type=eat, building=admin, index=100},
	
	Schedule1 = {pootis, [Activity1]},
	Schedule2 = {painis, [Activity1]},
	Schedules = [Schedule1, Schedule2],
	#solution{snuksti=Snuksti, schedules=Schedules}.

get_test_snuksti() ->
	Snuksts1 = #snuksts{id=pootis, name="Pootis", unavailability=[{1000, 1200}, {1500, 1700}]},
	Snuksts2 = #snuksts{id=painis, name="Painis", unavailability=[{0, 500}, {500, 750}]},
	[Snuksts1, Snuksts2].
	
	
% Izmaksu funkcija.
get_solution_cost(#solution{snuksti=Snuksti, schedules=Schedules}) ->
	get_unavailability_cost(Snuksti, Schedules).

% aprēķina nepieejamības izmaksu
get_unavailability_cost(Snuksti, Schedules) ->
	%erlang:display(Snuksti), erlang:display(Schedules), 
	match_snuksts_to_schedule(Snuksti, Schedules, Schedules).

% šņūkstu pieejamības palīgfunkcija, vispārējais gadījums
match_snuksts_to_schedule([Snuksts | Snuksti], [Schedule | Schedules], AllSchedules) ->
	SnukstsName = Snuksts#snuksts.id,
	{ScheduleName, ScheduleActivities} = Schedule,
	
	% pārbauda vai kārtējais grafiks pieder kārtējam šņūkstam
	if	SnukstsName == ScheduleName ->
			% aprēķinam pieejamību
			ScheduleCost = compute_schedule(Snuksts, ScheduleActivities),
			% ņemam nākamo šņūkstu un pieskaitām tā pieejamību
			Total = ScheduleCost + match_snuksts_to_schedule(Snuksti, AllSchedules, AllSchedules);
		true ->
			Total = match_snuksts_to_schedule([Snuksts | Snuksti], Schedules, AllSchedules)
	end,
	
	Total;
	
% gadījums ja šņūkstam nav darba grafiks
match_snuksts_to_schedule(_, [], _) -> 0;
% gadījums ja visi šņūksti ir apskatīti
match_snuksts_to_schedule([], _, _) -> 0.

activity_length(eat) -> 100;
activity_length(feed) -> 10.

compute_schedule(Snuksts, Activities) ->
	compute_schedule(Snuksts#snuksts.unavailability, Activities, 0, admin, 0).
compute_schedule(_, [], Time, _, Mines) ->
	case Time > 2000 of
		true -> Overtime = Time - 2000;
		false -> Overtime = 0
	end,
	
	case Time < 2000 of
		true -> Undertime = 2000 - Time;
		false -> Undertime = 0
	end,
	
	2 * Overtime + Undertime + 10 * Mines;
compute_schedule(Unavailability, [Activity | OtherActivites], Time, Location, Mines) ->
	ActivityType = Activity#activity.type,
	ActivityLocation = Activity#activity.building,

	TravelTime = get_distance(Location, ActivityLocation),
	ActivityTime = activity_length(ActivityType),
	EndTime = Time + TravelTime + ActivityTime,
	
	case Unavailability /= [] of
		true ->	[{From, To} | Rest] = Unavailability;
		false -> From = nil, To = nil, Rest = nil
	end,
	
	case get_mined(Location, ActivityLocation) of
		true -> EndMines = Mines + 1;
		false -> EndMines = Mines
	end,
	
	if 
		(From /= nil) andalso (From < EndTime) ->
			compute_schedule(Rest, [Activity | OtherActivites], To, Location, EndMines);
		true ->
			compute_schedule(Unavailability, OtherActivites, EndTime, ActivityLocation, EndMines)
	end.

get_modified_solution(#solution{snuksti=Snuksti, schedules=Schedules}) ->
	FirstN = rand:uniform(length(Schedules)),
	SecondN = rand:uniform(length(Schedules)),
	
	erlang:display([FirstN, SecondN]),
	
	case FirstN == SecondN of
		true->
			{FirstSnuksts, FirstSched} = lists:nth(FirstN, Schedules),
			
			Moved = lists:nth(rand:uniform(length(FirstSched)), FirstSched),
			RemovedSched = FirstSched -- [Moved],
			
			case RemovedSched == [] of
				true -> Split1 = [], Split2 = [];
				false -> {Split1, Split2} = lists:split(rand:uniform(length(RemovedSched)), RemovedSched)
			end,
			
			FinishedSched = (Split1 ++ [Moved]) ++ Split2,
			ScheduleList = (Schedules -- [{FirstSnuksts, FirstSched}]) ++ [{FirstSnuksts,FinishedSched}],
			
			#solution{snuksti=Snuksti, schedules=ScheduleList};
		false->
			{FirstSnuksts, FirstSched} = lists:nth(FirstN, Schedules),
			{SecondSnuksts, SecondSched} = lists:nth(SecondN, Schedules),
			
			Moved = lists:nth(rand:uniform(length(FirstSched)), FirstSched),
			
			case SecondSched == [] of
				true -> Split1 = [], Split2 = [];
				false -> {Split1, Split2} = lists:split(rand:uniform(length(SecondSched)), SecondSched)
			end,
			
			FinishedSched1 = FirstSched -- [Moved],
			FinishedSched2 = (Split1 ++ [Moved]) ++ Split2,
			ScheduleList = (((Schedules -- [{FirstSnuksts, FirstSched}]) -- [{SecondSnuksts, SecondSched}]) ++ [{FirstSnuksts, FinishedSched1}]) ++ [{SecondSnuksts, FinishedSched2}],
			
			#solution{snuksti=Snuksti, schedules=ScheduleList}
	end.

get_initial_solution(Snuksti) ->
	[{FirstName, FirstSched} | Rest] = get_initial_schedules(Snuksti),
	ModSched = FirstSched ++ get_initial_frog_feeds(get_all_buildings()),
	#solution{snuksti=Snuksti, schedules=[{FirstName, ModSched} | Rest]}.

get_initial_schedules([]) -> [];
get_initial_schedules([#snuksts{id=Snuksts} | Rest]) ->
	Schedule = [{Snuksts, [#activity{type=eat, building=admin, index=100}]}],
	Schedule ++ get_initial_schedules(Rest).

get_initial_frog_feeds([]) -> [];
get_initial_frog_feeds([Building | Rest]) ->
	This = get_initial_frog_feeds(Building, 1),
	Next = get_initial_frog_feeds(Rest),
	This ++ Next.
get_initial_frog_feeds(Building, N) ->
	#building{frogslots=Slots} = get_building(Building),
	case N > Slots of
		true -> [];
		false ->
			This = [#activity{type=feed, building=Building, index=N}],
			Next = get_initial_frog_feeds(Building, N+1),
			This ++ Next
	end.