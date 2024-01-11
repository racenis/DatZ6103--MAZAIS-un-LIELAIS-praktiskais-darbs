
% Parametri priekš optimizācijas darba.
%	optimizer : atom
%		Optimizētāja nosaukums.
%	iters : int
%		Iterācijas cik daudz optimizēsies.
%	snuksti : snuksts
%		Saraksts ar šņūkstiem.
%	desc : string
%		Darba apraksts/nosaukums.
-record(job_params, {optimizer, iters, snuksti, desc}).

