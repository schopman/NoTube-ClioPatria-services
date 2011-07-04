% put your custom code, such as api's here, eg: :- [apis/api1].
:- set_setting_default(http:port, 53021).
:- [namespaces].
:- [loaddata].
:- [apis/enrich].
:- enrich_lupedia:read_persistent_cache.
