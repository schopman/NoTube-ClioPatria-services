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

:- module(sameAs, [toFreebaseURI/2, toDbpediaURI/2, toLinkedmdbURI/2]).
:- use_module(library('semweb/rdf_db')).
:- use_module('freebaseGuidMid').

%% toFreebaseURI(+AnyURI, -FreebaseURI)
toFreebaseURI(U, U) :-
	isFreebaseURI(U),
	!.
toFreebaseURI(DbpU, U) :-
	isDbpediaURI(DbpU),
	dbpedia2freebase(DbpU, U),
	!.
toFreebaseURI(LmdbU, U) :-
	isLinkedmdbURI(LmdbU),
	freebase2linkedmdb(U, LmdbU).

%% toDbpediaURI(+AnyURI, -DbpediaURI)
toDbpediaURI(U, U) :-
	isDbpediaURI(U),
	!.
toDbpediaURI(FbU, U) :-
	isFreebaseURI(FbU),
	!,
	dbpedia2freebase(FbU, U).
toDbpediaURI(LmdbU, U) :-
	isLinkedmdbURI(LmdbU),
	freebase2linkedmdb(FbU, LmdbU),
	dbpedia2freebase(U, FbU).

%% toLinkedmdbURI(AnyURI, LinkedmdbURI)
toLinkedmdbURI(U, U) :-
	isLinkedmdbURI(U),
	!.
toLinkedmdbURI(FbU, U) :-
	isFreebaseURI(FbU),
	freebase2linkedmdb(FbU, U),
	!.
toLinkedmdbURI(DbpU, U) :-
	isDbpediaURI(DbpU),
	dbpedia2freebase(DbpU, FbU),
	freebase2linkedmdb(FbU, U).

/************************
* URI conversion
************************/
%% dbpedia2freebase(?DbpediaURI, ?FreebaseURI)
dbpedia2freebase(DbpediaURI, FreebaseURI) :-
	rdf(DbpediaURI, owl:sameAs, FreebaseURI, 'dbpedia_freebase_links').

%% freebase2linkedmdb(?Freebase_URI, ?LinkedMDB_URI)
freebase2linkedmdb(Freebase_URI, LinkedMDB_URI) :-
	nonvar(Freebase_URI),
	guid_uri2mid_uri(Freebase_GUID_URI, Freebase_URI),
	rdf(LinkedMDB_URI, foaf:page, Freebase_GUID_URI, 'linkedmdb').
freebase2linkedmdb(Freebase_URI, LinkedMDB_URI) :-
	nonvar(LinkedMDB_URI),
	rdf(LinkedMDB_URI, foaf:page, Freebase_GUID_URI, 'linkedmdb'),
	guid_uri2mid_uri(Freebase_GUID_URI, Freebase_URI).

%% guid2mid_graph(-GraphName)
guid2mid_graph('linkedmdb_guid2mid').

/************************
* guid2mid conversion
************************/
%% convertLinkedmdbFreebaseMappings
convertLinkedmdbFreebaseMappings :-
	guid2mid_graph(GraphName),
	rdf_graph(GraphName),
	!.
convertLinkedmdbFreebaseMappings :-
	guid2mid_graph(GraphName),
	findall(O, ( rdf(_, foaf:page, O, 'linkedmdb'),  freebaseURI(O)), GUIDS),
	forall(member(GUID_URI, GUIDS), 
		(	guid_uri2mid_uri(GUID_URI, MID_URI), 
			rdf_assert(GUID_URI, owl:sameAs, MID_URI, GraphName) )).

/************************
* URI type checks
************************/
%% isDbpediaURI(+URI)
isDbpediaURI(URI) :-
	atom_concat('http://dbpedia.org/resource/', _, URI).

%% isFreebaseURI(+URI)
isFreebaseURI(URI) :-
	sub_atom(URI, _, _, _, 'freebase.com/').

%% isLinkedmdbURI(+URI)
isLinkedmdbURI(URI) :-
	atom_concat('http://data.linkedmdb.org/', _, URI).
