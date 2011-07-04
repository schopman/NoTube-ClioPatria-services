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

:- module(enrich, [bbcResource/2, fetchAllBBCResources/1]).

/***************************************************
* load_modules
***************************************************/
% http library modules
:- use_module(library(http/http_open)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_wrapper)).

:- use_module('../../common_components/labels').
:- use_module('../../common_components/subset').
:- use_module('../../common_components/load_if_needed').
:- use_module('../../common_components/fetch_json_data').
:- use_module('../components/enrich_lupedia').
:- use_module('../components/enrich_linkedmdb').
:- use_module('../components/openGraphEnrich').
:- use_module('../components/mappings_roles_bbc').
:- use_module('../components/testset').

/***************************************************
* http handlers
***************************************************/
:- http_handler(root(epg), epg_api, []).
:- http_handler(root(enrichProgramme), enrichProgramme_api, []).
:- http_handler(root(enrichOpenGraphEntity), enrichOpenGraphEntity_api, []).
:- http_handler(root(testset), testset_api, []).
:- http_handler(root(sliceOfTestset), sliceOfTestset_api, []).

/***************************************************
* reply handlers
***************************************************/
%% epg_api(+Request)
epg_api(Request) :-
	http_parameters(Request,
		[	epg_uri(EPG_URI, [optional(true)]),
			cc(CC, [default(uk)]),
			nrHours(NrHours, [integer, default(24)])
		]
	),
	(nonvar(EPG_URI)
	->	fetch_json_data(EPG_URI, EPG)
	; 	fetchEpgData(CC, NrHours, EPG)
	),
	enrichEPG(EPG, EnrichedEPG),
	reply_json(EnrichedEPG).

%% enrichProgramme_api(+Request)
enrichProgramme_api(Request) :-
	http_parameters(Request,
		[	pid(PID, [])
		]
	),  % PID='b00zj99d',enrich:
	enrichProgramme(PID, EnrichedProgramme),
	reply_json([EnrichedProgramme]).

%% enrichOpenGraphEntity_api(+Request)
enrichOpenGraphEntity_api(Request) :-
	http_parameters(Request,
		[	name(Name, []),
			category(Category, [optional(true)])
		]
	),
	enrichOpenGraphEntity(Name, Category, EnrichedOGEntity),
	reply_json(EnrichedOGEntity).

%%% testset %%%
%% testset_api(+Request)
testset_api(_Request) :-
	listTestPids(TestsetPIDS),
	enrichEPG(TestsetPIDS, EnrichedTestset),
	reply_json(EnrichedTestset).

%% sliceOfTestset_api(+Request)
sliceOfTestset_api(_Request) :-
	listTestPids(Testset),
	subset(0, 1000, Testset, Slice),
    enrichEPG(Slice, Enriched),
	reply_json(Enriched).

/***************************************************
* RDF meta & JSON object declarations
***************************************************/
:- rdf_meta 
	getSynopsis(r,?,?),
	synopsisField(r).
:- json_object 
	broadcast(uri:atom, pid:atom, title:atom, start:atom, stop:atom, score:atom, votes:atom, source:atom, channelname:atom, enrichments:list, enrichmentstats:list).

/***************************************************
* fetch data from EPG Data Warehouse
***************************************************/
% NOTE: Do we need to adjust the time for different timezones, eg a request from the UK?
%% fetchEpgData(+CC, +NrHours, -EPG)
fetchEpgData(CC, NrHours, EPG) :- 
	%CC='uk', NrHours=24,
	generateDWHQuery(CC, NrHours, URI),
	fetch_json_data(URI, EPG).

%% generateDWHQuery(+CC, +NrHours, -URI)
generateDWHQuery(CC, NrHours, URI_Encoded) :- 
	get_time(TS), 
	TS2 is TS + NrHours*3600,
	format_time(atom(Now), '%Y-%m-%d %H:%M', TS),
	format_time(atom(Later), '%Y-%m-%d %H:%M', TS2),
	StartURI = 'http://services.notube.tv/epg/datawarehouse.php?service=programmeskosperiodenriched&start=',
	getChannelsArg(CC, Channels),
	format(atom(URI), '~w~w~w~w~w', [StartURI, Now, '&stop=', Later, Channels]),
	uri_encoded('fragment', URI, URI_Encoded).

%% getChannelsArg(+Region, -PartialURI).
getChannelsArg(uk, '&channelid=1,2,3,4').
getChannelsArg(nl, '&channelid=1,2,1001,1002,1003').

/***************************************************
* Top lvl
***************************************************/
%% enrichEPG(+ListBroadcasts, -EnrichedBroadcasts)
enrichEPG(ListBroadcasts, EnrichedBroadcasts) :-
	findall(EnrichedB,
		(	member(B, ListBroadcasts),
			getPid(B, PID),
			enrichProgramme(PID, EnrichedB)
		), EnrichedBroadcasts).

%% getPid(+BroadcastJSON, -PID)
getPid(BroadcastJ, PID) :-
	json_to_prolog(BroadcastJ, Broadcast),
	Broadcast = broadcast(_,PID,_,_,_,_,_,_,Channel,_,_),
	atom_concat('BBC ', _, Channel), % only use bbc metadata
	!.
getPid(PID, PID).

%% enrichProgramme(+PID, -Result)
enrichProgramme(PID, Result) :-
	bbcResource(PID, Resource),
	fetchAllBBCResources(Resource),
	getBasicData(Resource, Title, EpisodeTitle, Start, End, Channel, Genre, Synopsis, Depiction),
	enrichTitle(Resource, TitleURIs),
	getFormat(Resource, Format),
	getCredits(Resource, Credits),
	printProgramme(Resource, PID, Title, TitleURIs, EpisodeTitle, Format, Start, End, Channel, Genre, Synopsis, Depiction, Credits, Result).

/***************************************************
* EPG metadata
***************************************************/
%% fetchAllBBCResources(+Resource)
fetchAllBBCResources(Resource) :-
	fetchBBCProgrammeData(Resource),
	findall(R, allBBCResources(Resource, R), AllResources),
	forall(member(E, AllResources), fetchBBCProgrammeData(E)).

%% fetchBBCProgrammeData(+Resource)
fetchBBCProgrammeData(Resource) :- 
	resource2graph(Resource, Graph),
	load_if_needed(Graph).

%% resource2graph(+Resource, -Graph)
resource2graph(Resource, Graph) :-
	atom_concat(Mid, '#programme', Resource),
	atom_concat(Mid, '.rdf', Graph).

%% getFormat(+Resource, -Format)
getFormat(Resource, Format) :-
	findall(R1, allBBCResources(Resource, R1), AllResources),
	findall(F, ( member(R, AllResources), rdf(R, po:format, F) ), FormatList),
	sort(FormatList, Format).

%% printProgramme(+Resource, +PID, +Title, +TitleURIs, +EpisodeTitle, +Format, +Start, +End, +Channel, +Genre, +Synopsis, +Depiction, +Credits, -Result)
printProgramme(Resource, PID, Title, TitleURIs, EpisodeTitle, Format, Start, End, Channel, Genre, Synopsis, Depiction, Credits, Result) :-
	Result = json([
		uri=Resource, 
		pid=PID, 
		title=Title, 
		'title URIs'=TitleURIs,
		'episode title'=EpisodeTitle,
		format=Format,
		start=Start, 
		end=End, 
		channel=Channel,
		genre=Genre,
		synopsis=Synopsis, 
		depiction=Depiction,
		credits=Credits]).

%% getBasicData(+Resource, -Title, -EpisodeTitle, -Start, -End, -Channel, -Genre, -Synopsis, -Depiction)
getBasicData(Resource, Title, EpisodeTitle, Start, End, Channel, Genre, Synopsis, Depiction) :-
	getTitle(Resource, Title),
	getEpisodeTitle(Resource, EpisodeTitle),
	getStartEndChannel(Resource, Start, End, Channel),
	getGenre(Resource, Genre),
	getSynopsis(Resource, Synopsis),
	getDepiction(Resource, Depiction).

%% getTitle(-Resource, +Title)
getTitle(Resource, Title) :-
	getTitle(Resource, _, Title).

brandType(Type) 	:- rdf_equal(Type, po:'Brand').
seriesType(Type) 	:- rdf_equal(Type, po:'Series').
episodeType(Type) 	:- rdf_equal(Type, po:'Episode').

%% getEpisodeTitle(+Resource, -EpisodeTitle)
getEpisodeTitle(Resource, Result) :-
	brandType(B),
	seriesType(S),
	episodeType(E),
	getTitle(Resource, B, BT),
	getTitle(Resource, S, ST),
	getTitle(Resource, E, ET),
	getEpisodeTitle(BT, ST, ET, Result).

%% getEpisodeTitle(+BrandTitle, +SeriesTitle, +EpisodeTitle, -Result)
getEpisodeTitle(BT, ST, ET, ET) :-
	atom_length(BT, 0), 
	atom_length(ST, 0),
	!.
getEpisodeTitle(BT, ST, ET, Result) :-
	atom_length(ST, 0),
	concat_atom([BT, ET], ' - ', Result),
	!.
getEpisodeTitle(BT, ST, ET, Result) :-
	atom_length(BT, 0),
	concat_atom([ST, ET], ' - ', Result),
	!.
getEpisodeTitle(BT, ST, ET, Result) :-
	concat_atom([BT, ST, ET], ' - ', Result).

%% getTitle(-Resource, ?EnrichedEntityType, +Title)
getTitle(Resource, Type, Title) :-
	brandType(Type),
	rdf(Brand, po:episode, Resource),
	rdf(Brand, rdf:type, Type),
	getTitle2(Brand, Title),
	!.
getTitle(Resource, Type, Title) :-
	seriesType(Type),
	rdf(Series, po:episode, Resource),
	rdf(Series, rdf:type, Type),
	getTitle2(Series, Title),
	!.
getTitle(Resource, Type, Title) :-
	getTitle2(Resource, Title),
	rdf(Resource, rdf:type, Type),
	!.
getTitle(_,_,'').

%% getTitle2(+Resource, -Title)
getTitle2(Resource, Title) :-
	rdf(Resource, dc:title, TitleL),
	value2text(TitleL, Title).

%% getGenre(+Resource, -Genre)
getGenre(Resource, Genre) :-
	rdf(Resource, po:genre, Genre),
	!.
getGenre(_, '').

%% getStartEndChannel(+Resource, -Start, -End, -Channel)
getStartEndChannel(Resource, Start, End, Channel) :-
	findLatestBroadcast(Resource, Broadcast),
	rdf(Broadcast, 'http://purl.org/NET/c4dm/event.owl#time', Time),
	rdf(Time, 'http://purl.org/NET/c4dm/timeline.owl#start', StartL),
	rdf(Time, 'http://purl.org/NET/c4dm/timeline.owl#end', EndL),
	value2text(StartL, Start),
	value2text(EndL, End),
	rdf(Broadcast, po:broadcast_on, Channel),
	!.
getStartEndChannel(_, '', '', '').

%% findLatestBroadcast(+Resource, -LatestBroadcast)
findLatestBroadcast(Resource, LatestBroadcast) :-
	rdf(Resource, po:version, V),
	findall(B, rdf(B, po:broadcast_of, V), Broadcasts),
	Broadcasts = [H|T],
	chooseLatest(H, T, LatestBroadcast).

%% chooseLatest(+ViableChoice, +ListBroadcasts, -LatestBroadcast)
chooseLatest(Current, [H|T], LatestBroadcast) :-
	rdf(Current,po:schedule_date, Date1),
	rdf(H, 		po:schedule_date, Date2),
	later(Date1, Date2),
	!,
	chooseLatest(Current, T, LatestBroadcast).
chooseLatest(_, [H|T], LatestBroadcast) :-
	chooseLatest(H, T, LatestBroadcast).
chooseLatest(LatestBroadcast, [], LatestBroadcast).

% true is Date1 is later than or same as Date2
%% later(+Date1, +Date2)
later(date(Y1, _, _), 	date(Y2, _, _)) 	:- Y1 > Y2, !.
later(date(Y1, M1, _), 	date(Y2, M2, _)) 	:- Y1 = Y2, M1 > M2, !.
later(date(Y1, M1, D1), date(Y2, M2, D2)) 	:- Y1 = Y2, M1 = M2, D1 >= D2, !.
later(Date, Date) :- !.
later(Date1, Date2) :-
	value2text(Date1, D1A),
	value2text(Date2, D2A),
	atom_to_date(D1A, D1),
	atom_to_date(D2A, D2),
	later(D1, D2).

%% strToDate(+In, -Date)
atom_to_date(In, date(Y, M, D)) :-
	atom(In),
	atomic_list_concat([YA,MA,DA], -, In),
	atom_to_term(YA, Y, _),
	atom_to_term(MA, M, _),
	atom_to_term(DA, D, _).

%% bbcResource(+PID, -Resource)
bbcResource(PID, Resource) :-
	format(atom(Resource), '~w~w~w', ['http://www.bbc.co.uk/programmes/',PID,'#programme']).

%% getSynopsis(+Resource, -Synopsis)
getSynopsis(R, Synopsis) :-
	allBBCResources(R, Resource),
	getSynopsis2(Resource, SynopsisLit),
	value2text(SynopsisLit, Synopsis), 
	!.
getSynopsis(_,'').

%% getSynopsis2(+Resource, -Synopsis)
getSynopsis2(Resource, Synopsis) :- rdf(Resource, po:short_synopsis, Synopsis), !.
getSynopsis2(Resource, Synopsis) :- rdf(Resource, po:medium_synopsis, Synopsis), !.
getSynopsis2(Resource, Synopsis) :- rdf(Resource, po:long_synopsis, Synopsis).

%% getDepiction(+Resource, -Depiction)
getDepiction(R, Depiction) :-
	allBBCResources(R, Resource),
	rdf(Resource, foaf:depiction, Depiction),
	!.
getDepiction(_, '').

/***************************************************
* Collecting enrichments
***************************************************/
%% enrichTitle(+Resource, -TitleEnrichments)
enrichTitle(Resource, Enrichments) :-
	getTitle(Resource, _ResourceType, Title),
	getLupediaExactMatchEnrichments(Title, TitleEnr),
	makeListURIs(TitleEnr, Enrichments),
	!.
enrichTitle(_, []).

getCredits(Resource, Result) :-
	findall(R1, allBBCResources(Resource, R1), AllResources),
	findall(EC, (member(R2, AllResources), getCredits2(R2, EC)), EnrichedCredits1),
	sort(EnrichedCredits1, EnrichedCredits2),
	people2json(EnrichedCredits2, Result).
	
%% getCredits2(+Resource, -Result)
getCredits2(Resource, Result) :-
	rdf(Resource, po:credit, Credit),
	getPersonName(Credit, Name, NameURIs),
	getPersonRole(Credit, RoleV, RoleURI),
	defaultPersonEnrichmentWeight(W),
	Result = person(Name, NameURIs, RoleV, RoleURI, W).

%% getPersonName(+Credit, -Name, -NameURIs)
getPersonName(Credit, Name, NameURIs) :-
	rdf(Credit, po:participant, Person),
	getLabel(Person, Name),
	getNameURIs(Name, NameEnrichments),
	findall(NE, member(enrichment(NE), NameEnrichments), NameURIs).

%% getPersonRole(+Credit, -RoleV, -RoleURI)
getPersonRole(Credit, RoleV, RoleURI) :-
	rdf(Credit, po:role, Role), 
	rdf(Role, rdfs:label, RoleL),
	value2text(RoleL, RoleV),
	getRoleURI(RoleV, RoleURI),
	!.
getPersonRole(Credit, RoleV, RoleURI) :-
	rdf(Credit, po:role, Role), 
	rdf(Role, rdf:type, po:'Character'),
	RoleV='Actor', % god forgive me
	getRoleURI(RoleV, RoleURI),
	!.
getPersonRole(_, '', '').

%% removeDuplicateCredits(+Enrichments, -Result)
removeDuplicateCredits(E, Result) :-
	removeDuplicateCredits(E, [], Result).

%% removeDuplicateCredits(+Enrichments, +Sorted, -Result)
removeDuplicateCredits([H|T], Sorted, Result) :-
	not(member(H, Sorted)),
	removeDuplicateCredits(T, [H|Sorted], Result),
	!.
removeDuplicateCredits([_|T], Sorted, Result) :-
	removeDuplicateCredits(T, Sorted, Result).
removeDuplicateCredits([], Sorted, Sorted).

%% people2json(+ListEnrichments, -JSON)
people2json([H|T], JSON) :- 
	H = person(Name, NameURIs, Role, RoleURI, Weight),
	HJSON = json([name=Name, nameURIs=NameURIs, role=Role, roleURI=RoleURI, weight=Weight]),
	people2json(T, TJSON),
	JSON = [HJSON | TJSON].
people2json([], []).

%% getNameURIs(+Name, -Enrichments)
getNameURIs(Name, Enrichments) :-
	getLupediaExactMatchEnrichments(Name, Enrichments),
	!.
getNameURIs(Name, Enrichments) :-
	getLinkedmdbEnrichments(Name, Enrichments).

%% getRoleURI(+Role, -URIs)
getRoleURI(Role, URI) :-
	bbc_role_mapping(Role, URI), 
	!.
getRoleURI(_, '').

%% defaultPersonEnrichmentWeight(-Weight)
defaultPersonEnrichmentWeight(0.8).

%% makeListURIs(+Enrichments, -Result)
makeListURIs(Enrichments, Result) :-
	is_list(Enrichments),
	findall(Enrichment, member(enrichment(Enrichment), Enrichments), Result).

%% findDBpediaEnrichents(+ListEnrichments, -Result)
findDBpediaEnrichents([], []).
findDBpediaEnrichents([enrichment(_P,V,W)|T], Result) :-
	Weight is W/2,
	findall(
		enrichment(Prop, Value, Weight), 
		(rdf(V, Prop, Value), not(rdf_equal(Prop, rdf:type)) ), 
		Enrichments),
	findDBpediaEnrichents(T, MoreEnrichments),
	append(Enrichments, MoreEnrichments, Result).

%% getSynopsis(+Resource, -FieldLabel, -Synopsis)
getSynopsis(Resource, FieldLabel, Synopsis) :- 
	synopsisField(Field),
	rdf(Resource, Field, Synopsis), 
	getLabel(Field, FieldLabel1),
	value2text(FieldLabel1, FieldLabel).

%% synopsisField(-Field)
synopsisField(Field) :- rdf_equal(Field, po:long_synopsis).
synopsisField(Field) :- rdf_equal(Field, po:medium_synopsis).
synopsisField(Field) :- rdf_equal(Field, po:short_synopsis).

%% allBBCResources(+Resource, -XR)
allBBCResources(Resource, Resource).
allBBCResources(Resource, Series) :-
	rdf(Series, po:episode, Resource),
	rdf(Series, rdf:type, po:'Series').
allBBCResources(Resource, Brand) :-
	rdf(Brand, po:episode, Resource),
	rdf(Brand, rdf:type, po:'Brand').
allBBCResources(Resource, XR) :-
	rdf(Resource, po:version, XR).
