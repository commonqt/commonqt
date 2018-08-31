#ifdef __cplusplus
extern "C" {
#endif

#ifdef WIN32
#  define EXPORT __declspec(dllexport)
#else
#  define EXPORT
#endif

class DynamicBinding;
class Binding;
  
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

	void *binding;
} SmokeData;

EXPORT void sw_init();
EXPORT void sw_smoke(Smoke *, SmokeData*, void *, void *);
EXPORT void sw_override(DynamicBinding*, short, short);
EXPORT void* sw_make_dynamic_binding(Smoke*, QMetaObject*, short, short,
                                     void*, void*, void*);

EXPORT void* sw_make_qvector_uint(unsigned int *, int);
EXPORT void sw_delete_qvector_uint(void *);
EXPORT void* sw_make_qbytearray(char *);
EXPORT void sw_delete_qbytearray(void *q);
EXPORT void* sw_make_qstring(char *);
EXPORT void sw_delete_qstring(void *);
EXPORT void* sw_make_metaobject(void *, char *, int *);
EXPORT void sw_delete(void *p);
EXPORT void* sw_qstring_to_utf8(void* s);
EXPORT const void* sw_qstring_to_utf16(void*);

EXPORT int sw_windows_version();

EXPORT void sw_find_class(char *, Smoke **, short *);
EXPORT void sw_id_instance_class(void *, Smoke **, short *);
EXPORT short sw_find_name(Smoke *, char *);
EXPORT short sw_id_method(Smoke *, short, short);
EXPORT short sw_id_type(Smoke *, char *);
EXPORT short sw_id_class(Smoke *, char *, bool);

EXPORT void* sw_qstringlist_new();
EXPORT void sw_qstringlist_delete(void*);
EXPORT void sw_qstringlist_append(void*, char*);
EXPORT int sw_qstringlist_size(void*);
EXPORT const void* sw_qstringlist_at(void*, int);

EXPORT void* sw_qlist_void_new(void);
EXPORT int sw_qlist_void_size(void*);
EXPORT void sw_qlist_void_delete(void*);
EXPORT const void* sw_qlist_void_at(void*, int);
EXPORT void sw_qlist_void_append(void*, void*);

#define DECLARE_QLIST_SCALAR_MARSHALLER(NAME) \
  EXPORT void* sw_qlist_##NAME##_new(); \
  EXPORT int sw_qlist_##NAME##_size(void *ptr); \
  EXPORT void sw_qlist_##NAME##_delete(void *ptr); \
  EXPORT const void* sw_qlist_##NAME##_at(void *ptr, int index); \
  EXPORT void sw_qlist_##NAME##_append(void *ptr, void *whatptr);

DECLARE_QLIST_SCALAR_MARSHALLER(int)
DECLARE_QLIST_SCALAR_MARSHALLER(papersize)
DECLARE_QLIST_SCALAR_MARSHALLER(qvariant)
DECLARE_QLIST_SCALAR_MARSHALLER(qbytearray)
DECLARE_QLIST_SCALAR_MARSHALLER(qmodelindex)
DECLARE_QLIST_SCALAR_MARSHALLER(qkeysequence)
DECLARE_QLIST_SCALAR_MARSHALLER(extraselection)

#ifdef __cplusplus
}
#endif
