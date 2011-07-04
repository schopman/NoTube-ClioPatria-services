/* 	Part of NoTube Enriched EPG service - http://notube.tv

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

:- module(enrichOpenGraphEntity, [enrichOpenGraphEntity/3]).

:- use_module(library(http/json_convert)).

:- use_module(mappings_og).
:- use_module(enrich_lupedia).

%% enrichOpenGraphEntity(+Name, +Category, -EnrichedOGEntity)
enrichOpenGraphEntity(Name, Category, Result) :-
	getLupediaExactMatchEnrichmentsWTypes(Name, List),
	findResult(List, Category, Result).

%% findResult(+ListEnrichments, +Category, -EnrichedOGEntity)
findResult([], _, 'error: found nothing') :- !.
findResult([enrichment(Result, _)], _, Result) :- !.
findResult(List, Category, Result) :-
	chooseCorrectEnrichment(List, Category, Result).

%% chooseCorrectEnrichment(+List_DBP_URI, +Category, -Result)
chooseCorrectEnrichment([], _, 'error: could not choose correct enrichment from list').
chooseCorrectEnrichment([H|_], Category, E) :-
	H = enrichment(E, Type, _Source),
	matchCategoryType(Category, Type),
	!.
chooseCorrectEnrichment([_|T], Category, Result) :-
	chooseCorrectEnrichment(T, Category, Result).

%% matchCategoryType(+Category, +Type)
matchCategoryType(CategoryCase, Type) :- 
	downcase_atom(CategoryCase, Category),
	og_mapping(Category, Type), 
	!.
matchCategoryType(Category, Type) :- 
	og_mapping(Category, Subtype),
	rdf_reachable(Type, rdf:type, Subtype).
