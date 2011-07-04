/* 	Part of NoTube Linked Data Recommender service - http://notube.tv

	Author:		Balthasar Schopman
	E-mail:		schopman@cs.vu.nl / bschopmanvu@gmail.com
	WWW:		http://www.cs.vu.nl/~schopman
	
	Copyright 2011, Balthasar Schopman, VU University Amsterdam
	
	Licensed under the Apache License, Version 2.0 (the "License");
	you may not use this file except in compliance with the License.
	You may obtain a copy of the License at
	
		http://www.apache.org/licenses/LICENSE-2.0
	
	Unless required by applicable law or agreed to in writing, software
	distributed under the License is distributed on an "AS IS" BASIS,
	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
	See the License for the specific language governing permissions and
	limitations under the License.
*/

:- module(findSkosPathUnidirectional, [findSkosPath/3]).
:- use_module(library('semweb/rdf_db')).

% NB: Paths always begin with Interests and end with Enrichment.

%% findSkosPath(+Interest, +Enrichment, -Path)
findSkosPath(Same, Same, path(0, [Same])) :- 
	!.
findSkosPath(I, E, path(L, Path)) :-
	rdf_reachable(I, skos:broader, E, 3, L),
	!,
	findBroaderPath(I, E, L, Path).
findSkosPath(I, E, path(L, Path)) :-
	rdf_reachable(E, skos:broader, I, 3, L),
	findNarrowerPath(I, E, L, Path).

% We already know there is a path, now we just need to find it...
%% findBroaderPath(+A, +Z, +Length, -Path)
findBroaderPath(A, Z, 1, [A, 'http://www.w3.org/2004/02/skos/core#broader', Z]) :- 
	!.
findBroaderPath(A, Z, L, Path) :-
	rdf(A, skos:broader, B),
	L1 is L - 1,
	findBroaderPath(B, Z, L1, Subpath),
	append([A, 'http://www.w3.org/2004/02/skos/core#broader'], Subpath, Path).

%% findNarrowerPath(+A, +Z, +L, -Path)
findNarrowerPath(A, Z, 1, [A, 'http://www.w3.org/2004/02/skos/core#narrower', Z]) :- 
	!.
findNarrowerPath(A, Z, L, Path) :-
	rdf(B, skos:broader, A),
	L1 is L - 1,
	findNarrowerPath(B, Z, L1, Subpath),
	append([A, 'http://www.w3.org/2004/02/skos/core#narrower'], Subpath, Path).
