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

/* see http://www.notube.tv/wiki/index.php/Mappings_between_Open_Graph_Categories_%26_DBpedia_Ontology
 * for a standard that is not followed at all on Facebook.
 * 
 * Note: all categories should be lower case, eg _not_: mapping('Actor', Type), but mapping('actor', Type)
 */
:- module(mappings_og, [og_mapping/2]).

og_mapping('actor', Type)  					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Actor').
og_mapping('actor/director', Type) 			:- rdf_equal(Type, 'http://dbpedia.org/ontology/Actor').
og_mapping('artist', Type) 					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Artist').
og_mapping('book', Type) 					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Book').
og_mapping('comedian', Type) 				:- rdf_equal(Type, 'http://dbpedia.org/ontology/Comedian').
og_mapping('film', Type) 					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Film').
og_mapping('games', Type)					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Game').
og_mapping('movie', Type) 					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Film').
og_mapping('musician/band', Type) 			:- rdf_equal(Type, 'http://dbpedia.org/ontology/MusicalArtist').
og_mapping('musician/band', Type) 			:- rdf_equal(Type, 'http://dbpedia.org/ontology/Band').
og_mapping('musicians', Type) 				:- rdf_equal(Type, 'http://dbpedia.org/ontology/MusicalArtist').
og_mapping('public_figures_other', Type) 	:- rdf_equal(Type, 'http://dbpedia.org/ontology/Person').
og_mapping('television', Type) 				:- rdf_equal(Type, 'http://dbpedia.org/ontology/TelevisionShow').
og_mapping('tv show', Type) 				:- rdf_equal(Type, 'http://dbpedia.org/ontology/TelevisionShow').
og_mapping('website', Type) 				:- rdf_equal(Type, 'http://dbpedia.org/ontology/Website').
og_mapping('writer', Type) 					:- rdf_equal(Type, 'http://dbpedia.org/ontology/Writer').
