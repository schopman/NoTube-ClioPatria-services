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

:- module(labels, [getLabel/2, value2text/2]).

%% getLabel(+Resource, -Label)
getLabel(Resource, Label) :- 
	rdf(Resource, rdfs:label, literal(lang(en,Label))),
	!.
getLabel(Resource, Label) :- 
	rdf(Resource, rdfs:label, LabelLit), 
	value2text(LabelLit, Label),
	!.
getLabel(Resource, Label) :- % Resource='http://dbpedia.org/resource/Adrian_Edmondson',
	sub_atom(Resource, 0, L, A, 'http://dbpedia.org/resource/'),
	sub_atom(Resource, L, A, _, Name),
	( sub_atom(Name, _, _, _, '_')
	-> 	atomic_list_concat(List, '_', Name),
		atomic_list_concat(List, ' ', Label)
	; 	Label=Name
	),
	!.
getLabel(Resource, Resource).

%% value2text(+Value, -Text)
value2text(literal(Text), R) 	:- !, value2text(Text, R).
value2text(lang(_, Text), Text) :- !.
value2text(type(_, Text), Text) :- !.
value2text(Text, Text).
