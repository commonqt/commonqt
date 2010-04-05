#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#  define EXPORT __declspec(dllexport)
#else
#  define EXPORT
#endif

typedef struct SmokeData {
	const char *name;

	void *classes;
	short numClasses;

	void *methods;
	short numMethods;

	void *methodMaps;
	short numMethodMaps;

	void *methodNames;
	short numMethodNames;

	void *types;
	short numTypes;

	short *inheritanceList;
	short *argumentList;
	short *ambiguousMethodList;
	void *castFn;

	void *thin;
	void *fat;
} SmokeData;

EXPORT void sw_init();
EXPORT void sw_smoke(Smoke *, SmokeData *data, void *, void *, void *);
EXPORT void* sw_make_qstring(char *);
EXPORT void sw_delete_qstring(void *);
EXPORT void* sw_make_metaobject(void *, char *, int *);
EXPORT void sw_delete(void *p);
EXPORT void* sw_qstring_to_utf8(void* s);
EXPORT const void* sw_qstring_to_utf16(void*);

EXPORT void sw_map_children(void *x, void *y);

EXPORT void* sw_make_qpointer(void* target);
EXPORT bool sw_qpointer_is_null(void* x);
EXPORT void sw_delete_qpointer(void* x);

EXPORT int sw_windows_version();

EXPORT void sw_find_class(char *, Smoke **, short *);
EXPORT short sw_find_name(Smoke *, char *);
EXPORT short sw_id_method(Smoke *, short, short);
EXPORT short sw_id_type(Smoke *, char *);
EXPORT short sw_id_class(Smoke *, char *, bool);

EXPORT void* sw_make_qstringlist();
EXPORT void sw_delete_qstringlist(void *);
EXPORT void sw_qstringlist_append(void*, char*);

EXPORT void* sw_qlist_variant_new(void);
EXPORT int sw_qlist_variant_size(void*);
EXPORT void sw_qlist_variant_delete(void*);
EXPORT const void* sw_qlist_variant_at(void*, int);
EXPORT void sw_qlist_variant_append(void*, void*);

EXPORT void* sw_qlist_int_new(void);
EXPORT int sw_qlist_int_size(void*);
EXPORT void sw_qlist_int_delete(void*);
EXPORT int sw_qlist_int_at(void*, int);
EXPORT void sw_qlist_int_append(void*, int);

#ifdef __cplusplus
}
#endif
