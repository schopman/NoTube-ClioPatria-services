/* 	Part of NoTube Linked Data Recommender & Enriched EPG services - http://notube.tv

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

:- module(load_if_needed, [load_if_needed/1]).

:- use_module(library(http/http_open)).

%% load_if_needed(+ResourceOrList)
load_if_needed(ResourceOrList) :-
	is_list(ResourceOrList),
	!,
	findall(a, (member(Resource, ResourceOrList), load_if_needed_r(Resource)), _).
load_if_needed(ResourceOrList) :-
	load_if_needed_r(ResourceOrList).

%% load_if_needed_r(+Resource)
load_if_needed_r(R) :-
	rdf_graph(R),
	!.
load_if_needed_r(R) :-
	rdf_load(R),
	!.
load_if_needed_r(R) :-
	print_message(warning, 'Error fetching: '+R).
