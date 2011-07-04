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

:- module(enrich_lupedia, [getLupediaExactMatchEnrichments/2, getLupediaExactMatchEnrichmentsWTypes/2]).
:- dynamic cachedLupediaData/2.

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module('../../common_components/labels').

%% getLupediaExactMatchEnrichments(+QueryLiteral, -Enrichments)
getLupediaExactMatchEnrichments(QueryLiteral, Result) :- 
	value2text(QueryLiteral, QueryText),
	getLupediaEnrichments(QueryText, Enrichments),
	makeList(Enrichments, Result).
%% getLupediaExactMatchEnrichmentsWTypes(+QueryLiteral, -Enrichments)
getLupediaExactMatchEnrichmentsWTypes(QueryLiteral, Result) :- 
	value2text(QueryLiteral, QueryText),
	getLupediaEnrichments(QueryText, Enrichments),
	makeListWTypes(Enrichments, Result).

%% getLupediaEnrichments(+QueryText, -Enrichments)
getLupediaEnrichments(QueryText, Enrichments) :- 
	cachedLupediaData(QueryText, Enrichments),
	!.
getLupediaEnrichments(QueryText, Enrichments) :- 
	callLupedia(QueryText, Enrichments),
	assert(cachedLupediaData(QueryText, Enrichments)).

%% callLupedia(+QueryText, -Enrichments)
callLupedia(QueryTextUnenc, Enrichments) :-
	uri_encoded('query_value', QueryTextUnenc, QueryText),
	QueryBase = 'http://lupedia.ontotext.com/lookup/text2json?skip_sh3=false&single_match=true&lookupText=',
	format(atom(Query), '~w~w', [QueryBase, QueryText]),
	setup_call_cleanup(
		http_open(Query, Stream, []),
		json_read(Stream, Response),
		close(Stream)),
	parseLupediaResponse(Response, Enrichments).

%% parseLupediaResponse(+ListEnrichments, -ParsedResult)
parseLupediaResponse([], []).
parseLupediaResponse([H|T], Result) :-
	H = json(Response),
	member(instanceUri=EnrichmentEnc, Response),
	member(instanceClass=TypeEnc, Response),
	uri_encoded('fragment', Enrichment, EnrichmentEnc),
	uri_encoded('fragment', Type, TypeEnc),
	% member(weight=Weight, Response), % (is always 1 with Lupedia in exact match-mode)
	parseLupediaResponse(T, MoreResults),
	Result = [ lupediaEnr(Enrichment, Type) | MoreResults].

%% makeList(+Enrichments, Result)
makeList(Enrichments, Result) :-
	findall(
		enrichment(Enrichment),
		member(lupediaEnr(Enrichment, _Type), Enrichments),
		Result
		).
%% makeListWTypes(+Enrichments, Result)
makeListWTypes(Enrichments, Result) :-
	findall(
		enrichment(Enrichment, Type),
		member(lupediaEnr(Enrichment, Type), Enrichments),
		Result
		).

/*************
* persistency
*************/
%% persistent_cache_file(-File)
persistent_cache_file('lupedia_cache.pl').

%% write2persistent
write2persistent :-
	persistent_cache_file(File),
	open(File, 'write', FD),
	forall(cachedLupediaData(N1, E1), (
		encodeName(N1, N),
		encodeEnrichments(E1, E),
		format(atom(Unit), ':- assert(cachedLupediaData(\'~w\', ~w)).~n', [N, E]),
		write(FD, Unit)
	)),
	close(FD).

%% read_persistent_cache
read_persistent_cache :-
	format('Loading Lupedia cache ... '),
	retractall(cachedLupediaData(_,_)),
	persistent_cache_file(File),
	[File],
	unload_file(File),
	unencode_assertions.

%% unencode_assertions
unencode_assertions :-
	forall( (
		cachedLupediaData(Name, Enrichments),
		sub_atom(Name, _, _, _, '`'),
		decodeName(Name, NewName),
		decodeEnrichments(Enrichments, NewEnrichments),
		retract(cachedLupediaData(Name, Enrichments)),
		assert(cachedLupediaData(NewName, NewEnrichments))
		), true).

%% encodeName(+Name, -EncodedName)
encodeName(Name, EncName) :-
	atomic_list_concat(L, '\'', Name),
	atomic_list_concat(L, '`', EncName).

%% decodeName(+EncodedName, -Name)
decodeName(EncName, Name) :-
	atomic_list_concat(L, '`', EncName),
	atomic_list_concat(L, '\'', Name).

%% encodeEnrichments(+EList, -Result)
encodeEnrichments(EList, Result) :-
	findall(Subresult,
		(	member(E, EList), 
			E = lupediaEnr(EnrichmentU, TypeU),
			encodeName(EnrichmentU, Enrichment),
			encodeName(TypeU, Type),
			format(atom(Subresult), 'lupediaEnr(\'~w\', \'~w\')', [Enrichment, Type])
		),
		Result).

%% decodeEnrichments(+EList, -Result)
decodeEnrichments(EList, Result) :-
	findall(Subresult,
		(	member(E, EList), 
			E = lupediaEnr(EnrichmentEnc, TypeEnc),
			decodeName(EnrichmentEnc, Enrichment),
			decodeName(TypeEnc, Type),
			Subresult = lupediaEnr(Enrichment, Type)
		),
		Result).
