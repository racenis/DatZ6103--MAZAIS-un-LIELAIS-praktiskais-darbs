-module(domain).

-export([get_all_buildings/0]).
-export([get_building/1]).
-export([get_distance/2]).
-export([get_mined/2]).
-export([get_solution_cost/1]).
-export([get_solution_cost_full/1]).
-export([get_solution_soft_cost/1]).
-export([get_solution_hard_cost/1]).
-export([get_test_solution/0]).
-export([get_test_snuksti/0]).
-export([get_modified_solution/1]).
-export([get_initial_solution/1]).

-include("domain.hrl").

% +----------------------------------------------------------------------------+
% |                                                                            |
% |                            DOMĒNA KONFIGURĀCIJA                            |
% |                                                                            |
% +----------------------------------------------------------------------------+

% Atdod sarakstu ar visiem korpusiem.
get_all_buildings() ->	[admin, corp1, corp2, corp2a].

% Atdod korpusa informāciju.
get_building(admin) ->	#building{id=admin,name="Administrācija",frogslots=0};
get_building(corp1) ->	#building{id=corp1,name="Korpuss 1",frogslots=125};
get_building(corp2) ->	#building{id=corp2,name="Korpuss 2",frogslots=200};
get_building(corp2a) ->	#building{id=corp2a,name="Korpuss 2A",frogslots=25}.

% Atdod attālumus starp korpusiem.
get_distance(admin, corp1) -> 50;
get_distance(admin, corp2) -> 50 + get_distance(corp1, corp2);
get_distance(admin, corp2a) ->  50 + get_distance(corp1, corp2a);

get_distance(corp1, corp2) -> 15;
get_distance(corp1, corp2a) -> 25;

get_distance(corp2, corp2a) -> 5;

get_distance(A, B) when A == B -> 1;
get_distance(A, B) -> get_distance(B, A).

% Atdod to vai ceļš starp korpusiem ir mīnēts.
get_mined(corp2, corp2a) -> true;
get_mined(_, _) -> false.

% Atdod aktivitāšu ilgumus.
activity_length(eat) -> 100;
activity_length(feed) -> 10.




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
	
% +----------------------------------------------------------------------------+
% |                                                                            |
% |                            IZMAKSU APRĒĶINĀTĀJI                            |
% |                                                                            |
% +----------------------------------------------------------------------------+
	
% Izmaksu funkcija.
get_solution_cost(Solution) ->
	Estimate = get_solution_cost_full(Solution),
	
	SoftCost = get_solution_soft_cost(Estimate),
	HardCost = get_solution_hard_cost(Estimate),
	
	SoftCost + 5 * HardCost.

% Pilnās izmaksas
get_solution_cost_full(#solution{snuksti=Snuksti, schedules=Schedules}) ->
	match_snuksts_to_schedule(Snuksti, Schedules, Schedules).
	
% Mīkstās izmaksas
get_solution_soft_cost(#estimate{overtime=OWT, undertime=UWT, lunch_penalty=LUN, mine_fields_crossed=MIN, unorderedness=UND})->
	2*OWT + UWT + 1*LUN + 10*MIN + UND.

% Cietās izmaksas
get_solution_hard_cost(#estimate{forbidden_time=FBT})->
	FBT.
	
% Šņūkstu pieejamības palīgfunkcija, vispārējais gadījums.
match_snuksts_to_schedule([Snuksts | Snuksti], [Schedule | Schedules], AllSchedules) ->
	SnukstsName = Snuksts#snuksts.id,
	{ScheduleName, ScheduleActivities} = Schedule,
	
	% pārbauda vai kārtējais grafiks pieder kārtējam šņūkstam
	if	SnukstsName == ScheduleName ->
			
			% sarēķinām šitajam šņūkstam un nākamajiem viņu izmaksas
			ThisEstimate = compute_schedule(Snuksts, ScheduleActivities),
			NextEstimate = match_snuksts_to_schedule(Snuksti, AllSchedules, AllSchedules),

			% saplusojam kopā
			Total = #estimate{
				total_worked_time=ThisEstimate#estimate.total_worked_time+NextEstimate#estimate.total_worked_time,
				forbidden_time=ThisEstimate#estimate.forbidden_time+NextEstimate#estimate.forbidden_time,
				overtime=ThisEstimate#estimate.overtime+NextEstimate#estimate.overtime,
				undertime=ThisEstimate#estimate.undertime+NextEstimate#estimate.undertime,
				lunch_penalty=ThisEstimate#estimate.lunch_penalty+NextEstimate#estimate.lunch_penalty,
				mine_fields_crossed=ThisEstimate#estimate.mine_fields_crossed+NextEstimate#estimate.mine_fields_crossed,
				unorderedness=ThisEstimate#estimate.unorderedness+NextEstimate#estimate.unorderedness
			};
		true ->
			Total = match_snuksts_to_schedule([Snuksts | Snuksti], Schedules, AllSchedules)
	end,
	
	Total;
% Gadījums ja šņūkstam nav darba grafiks.
match_snuksts_to_schedule(_, [], _) -> {estimate, 0, 0, 0, 0, 0, 0, 0};
% Gadījums ja visi šņūksti ir apskatīti.
match_snuksts_to_schedule([], _, _) -> {estimate, 0, 0, 0, 0, 0, 0, 0}.


% Šņūksta darba grafika izmaksu aprēķins.
compute_schedule(Snuksts, Activities) ->
	% katra šņūksta darba diena sāksies laika momentā nulle, savukārt fiziski
	% šņūksta diena sāksies administrācijas ēkā
	compute_schedule(Snuksts#snuksts.unavailability, Activities, 0, admin, 0, 0, 0).
% apstājamais gadījums
compute_schedule(_, [], Time, _, Mines, Lunch, Order) ->
	% teiksim ka ja šņūksts ir strādājis ilgāk par 2000 laika vienībām, viņš ir pārstrādājies
	case Time > 2000 of
		true -> Overtime = Time - 2000;
		false -> Overtime = 0
	end,
	
	% savukārt ja šņūksts ir strādājis mazāk par 2000 laika vienībām, viņš ir zemstrādājies
	case (Time < 2000) and (Time < 120) of
		true -> Undertime = 2000 - Time;
		false -> Undertime = 0
	end,
	
	% un ja šņūksts ir strādājis ilgāk par 4000 stundām, viņš ir aizliegtstrādājies
	case Time > 4000 of
		true -> Forbiddentime = Time - 4000;
		false -> Forbiddentime = 0
	end,
	
	#estimate{
		total_worked_time = Time,
		forbidden_time = Forbiddentime,
		overtime = Overtime,
		undertime = Undertime,
		lunch_penalty = abs(1000 - Lunch),
		mine_fields_crossed = Mines,
		unorderedness = Order
	};

% vispārīgais gadījums, iterējās cauri visām aktivitātēm un skaita cik daudz laika
% tiek patērēts katrā aktivitātē un cik reizes mīnu laukam izies cauri šņūksts
compute_schedule(Unavailability, [Activity | OtherActivites], Time, Location, Mines, Lunch, Order) ->
	ActivityType = Activity#activity.type,
	ActivityLocation = Activity#activity.building,
	
	% aprēķinam iztērēto laiku lai nokļūtu uz kārtējo aktivitāti no iepriekšējās,
	% papildus aprēķināsim laika momentu kad kārtējā aktivitāte beigsies
	TravelTime = get_distance(Location, ActivityLocation),
	ActivityTime = activity_length(ActivityType),
	EndTime = Time + TravelTime + ActivityTime,
	
	% paskatamies vai kaut kad šņūksts būs aizņemts
	case Unavailability /= [] of
		true ->	[{From, To} | Rest] = Unavailability;
		false -> From = nil, To = nil, Rest = nil
	end,
	
	% pārbaudām vai šņūkstam bija jāiziet cauri mīnu laukam, lai nokļūtu šajā aktivitātē
	case get_mined(Location, ActivityLocation) of
		true -> EndMines = Mines + 1;
		false -> EndMines = Mines
	end,
	
	% pārbaudām vai šī ir šņūksta iešņūkstēšanās
	case ActivityType == eat of
		true -> EndLunch = Time + TravelTime; % aktivitātes sākuma laiks
		false -> EndLunch = Lunch
	end,
	
	% pārbaudām vai nākamā aktivitāte ir kārtīga
	case OtherActivites == [] of
		true ->
			EndOrder = Order;
		false ->
			[NextActivity | _] = OtherActivites,
			case NextActivity == [] of
				true ->
					EndOrder = Order;
				false ->
					case abs(Activity#activity.index - NextActivity#activity.index) > 1 of
						true ->
							EndOrder = Order + 1;
						false ->
							EndOrder = Order
					end
			end
	end,
	
	if 
		% ja šņūksts nespēj pabeigt aktivitāti un paspēt būt aizņemts, tad izlaižam
		% šo aktivitāti un mēģinām to izpildīt pēc tam kad šņūksts atbrīvojās
		(From /= nil) andalso (From < EndTime) ->
			compute_schedule(Rest, [Activity | OtherActivites], To, Location, EndMines, EndLunch, EndOrder);
		% citādi turpinām ar nākamo aktivitāti
		true ->
			compute_schedule(Unavailability, OtherActivites, EndTime, ActivityLocation, EndMines, EndLunch, EndOrder)
	end.

% +----------------------------------------------------------------------------+
% |                                                                            |
% |                           RISINĀJUMU ĢENERATORI                            |
% |                                                                            |
% +----------------------------------------------------------------------------+

% No risinājuma uzģenerē jaunu risinājumu.
get_modified_solution(#solution{snuksti=Snuksti, schedules=Schedules}) ->
	%io:format("Schedules: ~p~n~n", [Schedules]),

	FirstN = rand:uniform(length(Schedules)),	% izlozē pirmo darba grafiku
	SecondN = rand:uniform(length(Schedules)),	% izlozē otro darba grafiku
	
	case FirstN == SecondN of
		true->
			% ja izlozējās tas pats grafiks divreiz
			{FirstSnuksts, FirstSched} = lists:nth(FirstN, Schedules),
			
			% izraujam ārā kaut kādu nejaušu aktivitāti no grafika
			Moved = lists:nth(rand:uniform(length(FirstSched)), FirstSched),
			RemovedSched = FirstSched -- [Moved],
			
			% nejauši pārdalām atlikušo grafiku divās daļās
			case RemovedSched == [] of
				true -> Split1 = [], Split2 = [];
				false -> {Split1, Split2} = lists:split(rand:uniform(length(RemovedSched)), RemovedSched)
			end,
			
			% ielīmējam aktivitāti starp pārdalītajām daļām
			FinishedSched = (Split1 ++ [Moved]) ++ Split2,
			ScheduleList = (Schedules -- [{FirstSnuksts, FirstSched}]) ++ [{FirstSnuksts,FinishedSched}],
			
			#solution{snuksti=Snuksti, schedules=ScheduleList};
		false->
			% ja izlozējām divus dažādus grafikus
			{FirstSnuksts, FirstSched} = lists:nth(FirstN, Schedules),
			{SecondSnuksts, SecondSched} = lists:nth(SecondN, Schedules),
			
			% izraujam kaut kādu aktivitāti no pirmā grafika
			Moved = lists:nth(rand:uniform(length(FirstSched)), FirstSched),
			
			% nejauši pārdalām otro grafiku divās daļās
			case SecondSched == [] of
				true -> Split1 = [], Split2 = [];
				false -> {Split1, Split2} = lists:split(rand:uniform(length(SecondSched)), SecondSched)
			end,
			
			% salīmējam visu atpakaļ kopā
			FinishedSched1 = FirstSched -- [Moved],
			FinishedSched2 = (Split1 ++ [Moved]) ++ Split2,
			ScheduleList = (((Schedules -- [{FirstSnuksts, FirstSched}]) -- [{SecondSnuksts, SecondSched}]) ++ [{FirstSnuksts, FinishedSched1}]) ++ [{SecondSnuksts, FinishedSched2}],
			
			% pārbaudām vai ir atļauts šo aktivitāti pārvietot starp šnūkstiem
			case Moved#activity.type of
				eat -> #solution{snuksti=Snuksti, schedules=Schedules};
				_ -> #solution{snuksti=Snuksti, schedules=ScheduleList}
			end
	end.

% Uzģenerē sākotnējo risinājumu.
get_initial_solution(Snuksti) ->
	[{FirstName, FirstSched} | Rest] = get_initial_schedules(Snuksti),
	ModSched = FirstSched ++ get_initial_frog_feeds(get_all_buildings()),
	#solution{snuksti=Snuksti, schedules=[{FirstName, ModSched} | Rest]}.

% Uzģenerē sākotnējās aktivitātes (iešņūkšana).
get_initial_schedules([]) -> [];
get_initial_schedules([#snuksts{id=Snuksts} | Rest]) ->
	Schedule = [{Snuksts, [#activity{type=eat, building=admin, index=100}]}],
	Schedule ++ get_initial_schedules(Rest).

% Uzģenerē sākotnējās bruņuvaržu aktivitātes (piebarošana).
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