:- use_module(library(semweb/rdf_zlib_plugin)).

:- rdf_load('../ClioPatria/rdf/base/foaf.owl', [graph('base_foaf')]).
:- rdf_load('../ClioPatria/rdf/base/dc.rdfs', [graph('base_dc')]).
:- rdf_load('../ClioPatria/rdf/base/dcterms.rdf', [graph('base_dcTerms')]).
:- rdf_load('../data/po/po.n3', [graph('PO')]).
:- rdf_load('../data/linkedmdb/lmdbNamesTitles.nt.gz', [graph('lmdb_names_and_titles')]).
