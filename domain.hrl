
% Domēna apraksts, definīcijas.

% Šņūksta ieraksts
%	Name: 
%		Šņūksta vārds.
%		Simbolu virkne.
%	Unavailability:
%		Laiki, kuros šņūksts ir aizņemts.
%		Saraksts ar 2-daļīgiem tūpļiem. Laiks no-uz.
-record(snuksts, {id, name, unavailability}).

% Korpusa ieraksts
% 	Name:
%		Korpusa nosaukums.
%		Simbolu virkne.
%	FrogSlots:
%		Bruņuvaržu krātiņu skaits.
%		Skaitlis.
%
% Optimizators pieņems ka visi krātiņi vienmēr ir piepildīti. Reālā bruņuvaržu
% fabrikā tas nevienmēr būs patiesi.
-record(building, {id, name, frogslots}).

-record(solution, {snuksti, schedules}).

-record(activity, {type, building, index}).