
% Domēna apraksts, definīcijas.

% Šņūksta ieraksts
%	id : atom
% 		Šņuksta identifikators programmkodā.
% 	name : string
% 		Šņūksta vārds.
% 	unavailability : [{int, int}, ...]
% 		Laiki, kuros šņūksts ir aizņemts. Saraksts ar 2-daļīgiem tūpļiem.
% 		Laiks no-uz.
-record(snuksts, {id, name, unavailability}).

% Korpusa ieraksts
%	id : atom
% 		Korpusa identifikators programmkodā.
% 	name : string
%		Korpusa formālais nosaukums.
%	frogslots: int
%		Bruņuvaržu krātiņu skaits. Skaitlis
%
% Optimizators pieņems ka visi krātiņi vienmēr ir piepildīti. Reālā bruņuvaržu
% fabrikā tas nevienmēr būs patiesi.
-record(building, {id, name, frogslots}).

% Risinājuma ieraksts
%	solution : [#snuksts, ...]
%		Saraksts ar visiem šņūkstiem, kas strādās tajā dienā.
%	schedules : [{atom, [#activity, ...]}, ...]
% 		Saraksts ar šņūkstu darba grafikiem. Sastāv no tūpļu saraksta, pirmais
% 		tūplī ir šņūksta identifikators un otrais ir saraksts ar aktivitātēm.
-record(solution, {snuksti, schedules}).

% Aktivitātes ieraksts
%	type : atom
% 		Aktivitātes tips. Pašlaik var būt 'eat', jeb iešņūkstēties, vai 'feed',
% 		jeb pabarot bruņuvardi.
%	building : atom
% 		Ēkas identifikators, kurā notiks aktivitāte.
%	index : int
% 		Aktivitātes indekss. Pašlaik derīgs tikai 'feed' aktivitāte, kurai šis
% 		indekss nosaka bruņuvardes krātiņa numuru.
-record(activity, {type, building, index}).