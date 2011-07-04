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

:- module(subset, [subset/4, subset/3]).

%% subset(+Offset, +Size, +List, -Result)
subset(0, Size, List, Result) :- 
	!,
	subset(Size, List, Result).
subset(Offset, Size, [_|T], Result) :- 
	Offset > 0,
	Offset1 is Offset-1,
	subset(Offset1, Size, T, Result).

%% subset(+Size, +List, -Result)
subset(0, _, []) :- !.
subset(_, [], []) :- !.
subset(Size, [H|T], [H|Subresult]) :-
	Size1 is Size-1,
	subset(Size1, T, Subresult).
