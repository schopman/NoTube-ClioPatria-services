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

:- module(enrich_linkedmdb, [getLinkedmdbEnrichments/2]).
:- dynamic cached/2.

:- use_module('../../common_components/labels').

clearLinkedmdbEnrichmentsCache :-
	retractall(linkedmdbEnricher:cached(_,_)).

%% getLinkedmdbEnrichments(+Name, -Enrichment)
getLinkedmdbEnrichments(Name, Result) :-
	cached(Name, Enrichments),
	makeList(Enrichments, Result),
	!.
getLinkedmdbEnrichments(Name, Result) :-
	findall(
		Enrichment, 
		(rdf(Enrichment,_,NameL, 'lmdb_names_and_titles'), value2text(NameL, Name)),
		Enrichments),
	assert(cached(Name, Enrichments)),
	makeList(Enrichments, Result).


%% makeList(+Enrichments, -Result)
makeList(Enrichments, Result) :-
	findall(
		enrichment(Enrichment, 'VU LinkedMDB enricher'),
		member(Enrichment, Enrichments),
		Result
		).
