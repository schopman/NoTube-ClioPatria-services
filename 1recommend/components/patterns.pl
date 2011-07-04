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

:- module(patterns, [pattern_path/5]).

:- use_module(library('semweb/rdf_db')).
:- use_module('../../common_components/labels').
:- use_module('sameAs').

/**************
* patternPath
**************/
%% pattern_path(+Interest, +Enrichment, -Pattern, -PatternRelevanceWeight, -Explanation)
pattern_path(Same, Same, _, _, _) :- 
	!, fail. % exact match (failsafe, redundant functionality @ recommend_api:findPatternMatch/6)
pattern_path(Interest0, Enrichment0, Pattern, PatternRelevanceWeight, Explanation) :-
	toLinkedmdbURI(Interest0, Interest),
	toLinkedmdbURI(Enrichment0, Enrichment),
	pattern_path_lmdb(Interest, Enrichment, Pattern, PatternRelevanceWeight, Explanation).
pattern_path(Interest0, Enrichment0, Pattern, PatternRelevanceWeight, Explanation) :-
	toDbpediaURI(Interest0, Interest),
	toDbpediaURI(Enrichment0, Enrichment),
	pattern_path_dbpedia(Interest, Enrichment, Pattern, PatternRelevanceWeight, Explanation).
	
/****************************
* patterns in LinkedMDB
****************************/
%% pattern_path_lmdb(+Interest, +Enrichment, -Path, -Weight, -Explanation)
pattern_path_lmdb(Interest, Enrichment, Path, 1.0, Explanation) :-
	rdf_equal(P, lmdb:actor),
	rdf(Interest, P, Enrichment, 'linkedmdb'), 
	lmdbPath(Interest, P, Enrichment, Path),
	lmdbExplanation(Interest, Enrichment, 'acted in', Explanation).

pattern_path_lmdb(Interest, Enrichment, Path, 0.9, Explanation) :-
	rdf_equal(P, lmdb:director),
	rdf(Interest, P, Enrichment, 'linkedmdb'), 
	lmdbPath(Interest, P, Enrichment, Path),
	lmdbExplanation(Interest, Enrichment, 'directed', Explanation).

pattern_path_lmdb(Interest, Enrichment, Path, 0.8, Explanation) :-
	rdf_equal(P, lmdb:producer),
	rdf(Interest, P, Enrichment, 'linkedmdb'), 
	lmdbPath(Interest, P, Enrichment, Path),
	lmdbExplanation(Interest, Enrichment, 'produced', Explanation).

pattern_path_lmdb(Interest, Enrichment, Path, 0.7, Explanation) :-
	rdf_equal(P, lmdb:writer),
	rdf(Interest, P, Enrichment, 'linkedmdb'), 
	lmdbPath(Interest, P, Enrichment, Path),
	lmdbExplanation(Interest, Enrichment, 'wrote', Explanation).


pattern_path_lmdb(Interest, Enrichment, Path, 0.6, Explanation) :-
	rdf_equal(P, lmdb:music_contributor),
	rdf(Interest, P, Enrichment, 'linkedmdb'), 
	lmdbPath(Interest, P, Enrichment, Path),
	lmdbExplanation(Interest, Enrichment, 'constributed to the score of', Explanation).

% sequel: M1 lmdb:sequel M2 -> M1 has sequel M2 (disabled)
% pattern_path_lmdb(Interest, Enrichment, Path, 0.8, Explanation) :-
% 	rdf_equal(P, lmdb:sequel),
% 	rdf(Enrichment, P, Interest, 'linkedmdb'), 
% 	lmdbPath(Interest, P, Enrichment, Path),
% 	getLabel(Interest, InterestL),
% 	format(atom(Explanation), 'This is the sequel to ~p, which you saw recently.', [InterestL]).

%% lmdbPath(+Interest, +Predicate, +Enrichment, -Path)
lmdbPath(Interest, P, Enrichment, [I, P, E]) :-
	toDbpediaURI(Interest, I),
	toDbpediaURI(Enrichment, E).

%% lmdbExplanation(+InterestNode, +EnrichmentNode, +PersonRole, -Explanation)
lmdbExplanation(InterestN, EnrichmentN, PersonRole, Explanation) :-
	getLabel(InterestN, Interest),
	getLabel(EnrichmentN, Enrichment),
	format(atom(Explanation), 
		'This item features ~p, who ~w ~p (a movie you saw recently).', 
		[Enrichment, PersonRole, Interest]).

/****************************
* patterns in DBpedia
****************************/
% P1='http://dbpedia.org/resource/Russell_Brand', P2='http://dbpedia.org/resource/Bill_Hicks', patterns:pattern_path_dbpedia(P1, P2, Path, Weight).
%% pattern_path_dbpedia(+Interest, +Enrichment, -Path, -Weight, -Explanation)
pattern_path_dbpedia(Interest, Enrichment, Path, 1, Explanation) :-
	influencedBy(Interest, Enrichment, Path, IR), % IR = influence relation
	getLabel(Interest, InterestL),
	getLabel(Enrichment, EnrichmentL),
	inverse(IR, IIR),							% IIR = inverse influence relation
	getLabel(IIR, IIRL),
	format(atom(Explanation), 
		'This item features ~p, who\'s ~p ~p (who is in your user profile).', 
		[EnrichmentL, IIRL, InterestL]).
pattern_path_dbpedia(Interest, Enrichment, Path, 0.7, Explanation) :- % via (wo)man-in-the-middle
	not(Interest=Enrichment),
	influencedBy(Interest, MiddleMan, Path1, IR1),
	influencedBy(MiddleMan, Enrichment, Path2, IR2),
	mergePaths(Path1, Path2, Path), 
	getLabel(Interest, InterestL),
	getLabel(MiddleMan, MiddleManL),
	getLabel(Enrichment, EnrichmentL),
	inverse(IR1, IIR1),
	inverse(IR2, IIR2),
	getLabel(IIR1, IIR1L),
	getLabel(IIR2, IIR2L),
	format(atom(Explanation), 
		'This item features ~p, who\'s ~p ~p, who in turn ~p ~p (who is in your user profile).', 
		[EnrichmentL, IIR2L, MiddleManL, IIR1L, InterestL]).

%% influencedBy(+Interest, +Enrichment, -Path, -InfluencePredicate)
influencedBy(S, O, Path, Pred) :-
	influencedBy(S, O, 'dbpedia_infobox_mappingbased', Path, Pred).

%% influencedBy(+Interest, +Enrichment, +Graph, -Path, -InfluencePredicate)	
influencedBy(S, O, Graph, [S,P,O], P) :-
	rdf_equal(P, 'http://dbpedia.org/ontology/influencedBy'),
	rdf(S, P, O, Graph),
	!.
influencedBy(S, O, Graph, [S,PI,O], PI) :-
	rdf_equal(P, 'http://dbpedia.org/ontology/influencedBy'),
	rdf(O, P, S, Graph),
	inverse(P, PI),
	!.
influencedBy(S, O, Graph, [S,P,O], P) :-
	rdf_equal(P, 'http://dbpedia.org/ontology/influenced'),
	rdf(S, P, O, Graph),
	!.
influencedBy(S, O, Graph, [S,PI,O], PI) :-
	rdf_equal(P, 'http://dbpedia.org/ontology/influenced'),
	rdf(O, P, S, Graph),
	inverse(P, PI).

%% mergePaths(+Path1, +Path2, -Result)
mergePaths(Path1, [H|T], Result) :-
	last(Path1, H),
	append(Path1, T, Result).

%% inverse(+P, -IP)
inverse(P, IP) :-
	inverse_p(P, IP),
	!.
inverse(P, IP) :-
	inverse_p(IP, P).

%% inverse_p(+P, -IP)
inverse_p(P, IP) :-
	rdf_equal(P,  'http://dbpedia.org/ontology/influencedBy'),
	rdf_equal(IP, 'http://dbpedia.org/ontology/influenced').
