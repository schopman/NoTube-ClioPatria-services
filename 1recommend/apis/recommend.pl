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

:- module(recommend_api, []).

/***************************************************
* load_modules
***************************************************/
:- use_module('../../common_components/labels').
:- use_module('../../common_components/fetch_json_data').
:- use_module('../components/findSkosPath').
:- use_module('../components/patterns').

:- use_module(library(http/http_open)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(dialect/hprolog)).
:- use_module(library(http/html_write)).

/***************************************************
* http handlers
***************************************************/
:- http_handler(root(patternRecommender), patternRecommender_api, []).
:- http_handler(root(patternRecommenderForInterest), patternRecommenderForInterest_api, []).
:- http_handler(root(skosRecommender), skosRecommender_api, []).

/***************************************************
* handle reply
***************************************************/
%%%% Patterns %%%%
patternRecommender_api(Request) :-
	http_parameters(Request,
		[	up(UP_URI, 	[default('http://www.cs.vu.nl/~schopman/static/patterns_up.php')]),
			items(ItemsURIencoded, [default('http://eculture2.cs.vu.nl:53021/sliceOfTestset')])
			% 'http://eculture2.cs.vu.nl:53021/enrichProgramme?pid=b0074fpm b0074d95 b00t3tlp b00xyds9 b00747pc b0074g6n b0074g8t b0074gbx b00786yd b0079316 b007cbmv b008d33g'
		]	% jackie chan: http://eculture2.cs.vu.nl:53021/enrichProgramme?pid=b00t3tlp
	),
	uri_encoded(query_value, ItemsURI, ItemsURIencoded),
	doPatternRecommendations(UP_URI, ItemsURI, _).

patternRecommenderForInterest_api(Request) :-
	http_parameters(Request,
		[	interest(Interest, 	[]), % eg: dbpedia:'Rowan_Atkinson'
			items(ItemsURIencoded, [default('http://eculture2.cs.vu.nl:53021/testset')])
		]
	),
	uri_encoded(query_value, ItemsURI, ItemsURIencoded),
	fetchItems(ItemsURI, _, _, Items),
	UP = [Interest=1],
	pattern_recommend(UP, Items, Result),
	reply_json(json([recommendations=Result])).

% trace, recommend_api:doPatternRecommendations('http://www.cs.vu.nl/~schopman/static/patterns_up.php', 'http://eculture2.cs.vu.nl:53021/enrichProgramme?pid=b0074gbx', Result).
doPatternRecommendations(UP_URI, ItemsURI, Result) :- 
	fetchUserProfile(UP_URI, UP),
	fetchItems(ItemsURI, _, _, Items),
	pattern_recommend(UP, Items, Result),
	reply_json(json([
		'status'='ok', 
		'user profile URI (up)'=UP_URI, 
		'Items URI (items)'=ItemsURI, 
		'recommender strategy'='patterns',
		recommendations=Result
	])).

skosRecommender_api(Request) :-
	http_parameters(Request,
		[	up(UP_URI, 		[default('http://www.cs.vu.nl/~schopman/static/up.php')]),
			epg(EPG_URI, 	[default('http://eculture2.cs.vu.nl:53021/epg')]),
			% epg(EPG_URI, 	[default('http://www.cs.vu.nl/~schopman/static/epg/enrichEPG.txt')]),
			epg_param(EPG_params, 	[optional(true)])
		]
	),
	doRecommendSKOS(UP_URI, EPG_URI, EPG_params).

% trace, recommend_api:doRecommendSKOS('http://www.cs.vu.nl/~schopman/static/up.php', 'http://www.cs.vu.nl/~schopman/static/epg/enrichEPG.txt', _).
doRecommendSKOS(UP_URI, EPG_URI, EPG_params) :-
	fetchUserProfile(UP_URI, UP),
	fetchItems(EPG_URI, EPG_params, EPG_Call, Items),
	skos_recommend(UP, Items, Result),
	reply_json(json([
		'status'='ok', 
		'user profile URI (up)'=UP_URI, 
		'EPG URI (epg)'=EPG_Call, 
		recommendations=Result
		])).

/***************************************************
* RDF meta & JSON object declarations
***************************************************/
:- json_object
	broadcast(uri, pid, title, 'title URIs', 'episode title', format, start, end, channel, genre, synopsis, depiction, credits),
	setEnrichments(enrichedEntity, enrichedEntityType, enrichedFieldLabel, enrichedFieldValue, enrichments),
	userProfile1(entry),
	userProfile2(id, appData).

/***************************************************
* EPG
***************************************************/
% NOTE: different timezones should be taken into account somehow
%% fetchItems(+URI_BASE, +URI_PARAM, -Call, -EPG)
fetchItems(BASE, PARAM, Call, EPG) :-
	atom(PARAM),
	!,
	format(atom(Call), '~w?~w', [BASE, PARAM]),
	fetch_json_data(Call, EPG).
fetchItems(URI, _, URI, EPG) :-
	fetch_json_data(URI, EPG).

%% fetchItems(+Call, -Result)
fetchItems(Call, Result) :-
	fetch_json_data(Call, Result).

/***************************************************
* User Profile
***************************************************/
%% fetchUserProfile(+URI, -UP)
fetchUserProfile(URI, UP) :-
	fetch_json_data(URI, UP1_JSON),
	json_to_prolog(UP1_JSON, UP1),
	UP1 = userProfile1(Entry),
	Entry = userProfile2(_, UP_JSON),
	UP_JSON = json(UP).

/***************************************************
* Recommend SKOS
***************************************************/
%% skos_recommend(+UP, +EPG, -Result)
skos_recommend(UP, EPG, Result) :-
	findall(RItem,
		(	member(ItemJ, EPG),
			json_to_prolog(ItemJ, Item),
			skos_recommendItem(UP, Item, RItem)
		), 
	Result).
	
%% skos_recommendItem(+UP, +Item, -Recommendation)
skos_recommendItem(UP, Item, Recommendation) :-
	useBroadcast(Item, URI, PID, Title, BrandTitle, TitleURIs, Format, Start, End, Channel, Genre, Synopsis, Depiction, _Credits),
	rdf_equal(InvSubjPred, dcterms:hasSubject),
	UILink = [	'http://example.org/foaf/you.rdf', 					'http://xmlns.notu.be/wi#preference', 
				'http://example.org/beancounter/you.rdf#interest0', 'http://xmlns.notu.be/wi#topic'],
	findall([Subscore, Match],
		(	member(TitleU, TitleURIs),
			findall(Annotation,
				rdf(TitleU, dcterms:subject, Annotation, 'dbpedia_category_annotations'),
				Annotations),
			member(A, Annotations),
			member(Interest, UP),
			disassemble_interest(Interest, I, IWeight),
			findSkosMatch(I, IWeight, A, Match1),
			Match1 = match(Type, Subscore, Path1),
			append(Path1, [InvSubjPred, TitleU], Path2),
			append(UILink, Path2, Path),
			Match = match(Type, Subscore, Path)
		),
		Matches),
	distilScoresReasons(Matches, Score, Reasons),
	Score @> 0,
	printItemData(URI, PID, Title, BrandTitle, Format, Genre, Start, End, Channel, Synopsis, Depiction, PrintItemData),
	findall(RJ, (
			member(R, Reasons), 
			R=match(Type, Subscore, Path), 
			RJ=json([type=Type, subscore=Subscore, path=Path]) ), 
		ReasonsJSON),
	Recommendation = json([
		uri=URI, 
		broadcasts=[PrintItemData], 
		'recommendation score'=Score, 
		reasons=ReasonsJSON])
	.

/************************
* Pattern Recommendation
************************/
%% pattern_recommend(+UP, +EPG, -Result)
pattern_recommend(_, [], []).
pattern_recommend(UP, [Item_JSON|T], Result) :-
	json_to_prolog(Item_JSON, Item),
	useBroadcast(Item, URI, PID, Title, BrandTitle, TitleURIs, Format, Start, End, Channel, Genre, Synopsis, Depiction, Credits),
	interest_enrichment_link(URI, IELink),
	ItemData = itemdata(TitleURIs, Credits, Format, Genre, IELink),
	p_recommendItem(UP, ItemData, Score, Reasons_),
	Score @> 0, 
	!,
	printItemData(URI, PID, Title, BrandTitle, Format, Genre, Start, End, Channel, Synopsis, Depiction, PrintItemData),
	rm_empty_lists(Reasons_, Reasons),
	% constructSingleReasonRecommendation(Reasons, SingleReason),
	Recommendation = json([
		uri=URI, 
		broadcasts=[PrintItemData], 
		'recommendation score'=Score,
		% 'reason'=SingleReason,
		reasons=Reasons]),
	pattern_recommend(UP, T, Subresult),
	Result = [Recommendation | Subresult].
pattern_recommend(UP, [_|T], Result) :- 
	pattern_recommend(UP, T, Result).

%% interest_enrichment_link(+URI, -IELink)
interest_enrichment_link(URI, [Pred, URI]) :-
	rdf_equal(Pred, po:credit).

%% pathsWithSameObject(+Path1, +Path2)
pathsWithSameObject([Path | RestOfPaths], [Path | Similar]) :-
	last(Path, S),
	findall(P, ( member(P, RestOfPaths), last(P, S) ), Similar).

%% getpath(+Reason, -Path)
getpath(ReasonJ, Path) :-
	ReasonJ = json(ReasonL),
	member('pattern path'=Path, ReasonL).

%% p_recommendItem(+UP, +ItemData, -Score, -Reasons)
p_recommendItem(UP, ItemData, Score, Reasons) :-
	p_recommendItemTitle(  UP, ItemData, Score1, Reasons1),
	p_recommendItemCredits(UP, ItemData, Score2, Reasons2),
	Score is Score1+Score2, 
	append(Reasons1, Reasons2, Reasons).

%% p_recommendItemTitle(+UP, +IELink, -Score, -Reasons)
p_recommendItemTitle(UP, ItemData, Score, Reasons) :-
	ItemData = itemdata(TitleURIs, _, _, _, _),
	findall([S,R], 
		( 	member(I, UP), 
			member(T, TitleURIs), 
			findPatternMatch(I, T, 'N/A', 1, ItemData, S, R) ),
		Results),
	distilScoresReasons(Results, Score, Reasons).

%% p_recommendItemCredits(+UP, +ItemData, -Score, -Reasons)
p_recommendItemCredits(UP, ItemData, Score, Reasons) :-
	ItemData = itemdata(_, Credits, _, _, _),
	findall([S,R],
		( 	member(I, UP), 
			member(CJ, Credits), 
			CJ = json(C),
			member(role=Role, C),
			member(nameURIs=NameURIs, C),
			member(NU, NameURIs),
			findPatternMatch(I, NU, Role, 1, ItemData, S, R) ),
		Results),
	distilScoresReasons(Results, Score, Reasons).

%% distilScoresReasons(+ResultList, -Score, -Reasons)
distilScoresReasons(ResultList, Score, Reasons) :-
	maplist(first, ResultList, ScoresList),
	maplist(last,  ResultList, Reasons),
	sumlist(ScoresList, Score).

%% first(+List, -FirstElement)
first([H|_], H).

/***************************************************
* Generic stuff
***************************************************/
%% printItemData(+URI, +PID, +Title, +BrandTitle, +Format, +Start, +End, +Channel, +Synopsis, +Depiction, -Result)
printItemData(URI, PID, Title, BrandTitle, Format, Genre, Start, End, Channel, Synopsis, Depiction, Result) :-
	Result = json([
		uri=URI, 
		pid=PID, 
		title=Title, 
		brandTitle=BrandTitle,
		format=Format, 
		genre=Genre,
		start=Start, 
		end=End, 
		channel=Channel, 
		synopsis=Synopsis, 
		depiction=Depiction]).

%% useBroadcast(+Broadcast, -URI, -PID, -Title, -BrandTitle, -TitleURIs, -Format, -Start, -End, -Channel, -Synopsis, -Credits)
useBroadcast(B, URI, PID, Title, BrandTitle, TitleURIs, Format, Start, End, Channel, Genre, Synopsis, Depiction, Credits) :- 
	B=broadcast(URI, PID, BrandTitle, TitleURIs, Title, Format, Start, End, Channel, Genre, Synopsis, Depiction, Credits).

/***************************************************
* Match making
***************************************************/
%% findSkosMatch(+Interest, +WeightOfInterest, +Enrichment, -Result)
findSkosMatch(Interest, IWeight, Interest, Result) :- % -> direct match
	Score is IWeight,
	Result = match('direct match', Score, [Interest]),
	!.
findSkosMatch(Interest, IWeight, Enrichment, Result) :-
	findSkosPath(Interest, Enrichment, path(Length, Path)),
	Score is IWeight / (Length * Length),
	Result = match('match via skos path', Score, Path).

%% findPatternMatch(+Interest, +Enrichment, +WeightEnr, +ItemEnrichmentLink, -Score, -Reason)
findPatternMatch(I, Enrichment, _WeightEnr, _ItemData, _Score, _Reason) :-
	disassemble_interest(I, Enrichment, _WeightInterest), % -> direct match (trivial result)
	!,
	fail.
	% Score is WeightEnr * WeightInterest,
	% 	Reason = json(['type'='exact match',
	% 		'interest'=Enrichment,
	% 		'enrichment'=Enrichment,
	% 		'weight of interest'=WeightInterest,
	% 		'weight of enrichment'=WeightEnr,
	% 		'subscore'=Score]).
findPatternMatch(I, Enrichment, PersonRole, WeightEnr, ItemData, Score, Reason) :-
	ItemData = itemdata(_, _, _ItemFormat, _ItemGenre, IELink),
	disassemble_interest(I, Interest, WeightInterest),
	pattern_path(Interest, Enrichment, Path, WeightPath, Explanation),
	Score is WeightEnr * WeightInterest * WeightPath,
	assemble_full_path(Path, IELink, FullPath),
	Reason = json([
		'type'='pattern match',
		'score'=Score,
		'interest'=Interest,
		'enrichment'=Enrichment,
		'person role'=PersonRole,
		'weight of interest'=WeightInterest,
		'weight of enrichment'=WeightEnr,
		'weight of pattern (relevance)'=WeightPath,
		'full path'=FullPath,
		'explanation'=Explanation
		]).

%% disassemble_interest(+Construct, -Interest, -Weight)
disassemble_interest(Construct, Interest, Weight) :-
	Construct =.. [=, Interest, Weight].

%% assemble_full_path(+Path, +IELink, -FullPath)
assemble_full_path(Path, IELink, FullPath) :-
	rdf_equal(User, 'http://example.org/~you/foaf.rdf'),
	rdf_equal(UserInterestLink1, wi:preference),
	rdf_equal(WI_Node, 'http://example.org/you/up.n3#interest0'),
	rdf_equal(UserInterestLink2, wi:topic),
	IULink = [User, UserInterestLink1, WI_Node, UserInterestLink2],
	append(IULink, Path, Path1),
	append(Path1, IELink, FullPath).

%% rm_empty_lists(+Lists, -NonemptyLists)
rm_empty_lists([], []).
rm_empty_lists([[]|T], Result) :-
	rm_empty_lists(T, Result),
	!.
rm_empty_lists([H|T], [H|Subresult]) :-
	rm_empty_lists(T, Subresult).
