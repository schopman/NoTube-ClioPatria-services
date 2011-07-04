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

:- module(fetch_json_data, [fetch_json_data/2]).

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

%% fetch_json_data(+URI, -Data)
fetch_json_data(URI, Data) :-
	print_message(informational, 'Fetching JSON data from: '+URI),
	setup_call_cleanup(
		http_open(URI, Stream, []),
		json_read(Stream, Data),
		close(Stream)).
