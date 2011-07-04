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

:- module(mappings_roles_bbc, [bbc_role_mapping/2]).

bbc_role_mapping('Actor', 		'http://purl.org/vocabularies/princeton/wn30/synset-actor-noun-1').
bbc_role_mapping('Director', 	'http://purl.org/vocabularies/princeton/wn30/synset-film_director-noun-1').
bbc_role_mapping('Writer', 		'http://purl.org/vocabularies/princeton/wn30/synset-writer-noun-1').
