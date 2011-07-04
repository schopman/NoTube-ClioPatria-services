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

:- module(linkedmdb_freebase_guid2mid, [guid2mid/2, guid_uri2mid_uri/2]).

:- dynamic cached_guid2mid/2, cached_mid2guid/2.

% NB: GUID were the persitant identifiers used in Freebase, which are now deprecated and replaced by MIDs.
% see http://wiki.freebase.com/wiki/Guid

%% guid2mid(+GUID, -MID)
% eg GUID_URI='http://www.freebase.com/view/guid/9202a8c04000641f8000000001121b1d',
guid_uri2mid_uri(GUID_URI, MID_URI) :-
	nonvar(GUID_URI),
	sub_string(GUID_URI, _, _, Length, '/guid/'),
	sub_string(GUID_URI, _, Length, 0, GUID),
	guid2mid(GUID, MID),
	atom_concat('http://rdf.freebase.com/ns', MID, MID_URI).
guid_uri2mid_uri(GUID_URI, MID_URI) :-
	nonvar(MID_URI),
	atom_concat('http://rdf.freebase.com/ns', MID, MID_URI),
	guid2mid(GUID, MID),
	atom_concat('http://www.freebase.com/view/guid/', GUID, GUID_URI).

%% guid2mid(+GUID, -MID)
guid2mid(GUID, MID) :-
	HEX = '0123456789abcdef',
	B32 = '0123456789bcdfghjklmnpqrstvwxyz_',
	PREFIX_GUID = '9202a8c04000641f8',
	PREFIX_MID =  '/m/0',
	( nonvar(GUID)
	->	( cached_guid2mid(GUID, MID)
		-> 	true
		; 	atom_concat(PREFIX_GUID, HEX_ID, GUID),
			baseX(HEX_ID, HEX, DEC_ID),
			baseX(B32_ID, B32, DEC_ID),
			atom_concat(PREFIX_MID, B32_ID, MID),
			assert(cached_guid2mid(GUID, MID))
		)
		
	;	( cached_mid2guid(MID, GUID), nonvar(MID)
	 	->	true
	 	;	atom_concat(PREFIX_MID, B32_ID, MID),
			baseX(B32_ID, B32, DEC_ID),
			baseX(HEX_ID, HEX, DEC_ID),
			addZerosToGuid(HEX_ID, HEX_ID2),
			atom_concat(PREFIX_GUID, HEX_ID2, GUID),
			assert(cached_mid2guid(MID, GUID))
		)
	).

%% addZerosToGuid(+HEX_ID, -Result)
addZerosToGuid(HEX_ID, Result) :-
	atom_length(HEX_ID, L),
	L < 15, !,
	atom_concat('0', HEX_ID, HEX_ID2),
	addZerosToGuid(HEX_ID2, Result).
addZerosToGuid(R, R).

%% baseX(?Encoded, +Alphabet, ?Decimal)
baseX(Enc, Alphabet, Dec) :-
	nonvar(Enc),
	baseX2dec(Enc, Alphabet, _, Dec).
baseX(Enc, Alphabet, Dec) :-
	nonvar(Dec),
	dec2baseX(Dec, Alphabet, Enc).
	
%% baseX2dec(+EncCodes, +Alphabet, -Depth, -Decimal)
baseX2dec(Encoded, Alphabet, Depth, Result) :- 
	sub_atom(Encoded, 0, 1, RestLen, FirstChar),
	sub_atom(Encoded, 1, RestLen, _, Rest),
	sub_atom(Alphabet, Decimal, _, _, FirstChar),
	
	baseX2dec(Rest, Alphabet, SubDepth, Subresult),
	Depth is SubDepth+1,
	atom_length(Alphabet, Base),
	Result is Decimal * Base^SubDepth + Subresult.
baseX2dec('', _, 0, 0).

%% dec2baseX(+Dec, +Alphabet, -ResultBaseX)
dec2baseX(0, _, '') :- !.
dec2baseX(Dec, Alphabet, Result) :-
	atom_length(Alphabet, Base),
	Nr is Dec mod Base,
	Rest is floor(Dec / Base),
	sub_atom(Alphabet, Nr, 1, _, Code),	% nth0(Nr, AlphCodes, Code),
	dec2baseX(Rest, Alphabet, Subresult),
	atom_concat(Subresult, Code, Result).
	% append(Subresult, [Code], Result).

% - GUID : 9202a8c04000641f80000000054dca01
% -> HEX : 000000001103098
% -> DEC : 17838232 / 88984065
% -> MID : /m/02nwlj1
% http://www.freebase.com/api/service/mqlread?query={"query":[{"guid":"%239202a8c04000641f80000000054dca01","mid":null}]}
