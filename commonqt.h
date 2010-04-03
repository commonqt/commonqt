#ifdef __cplusplus
extern "C" {
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

        void sw_init();
        void sw_smoke(Smoke *, SmokeData *data, void *, void *, void *);
        void* sw_make_qstring(char *);
        void sw_delete_qstring(void *);
        void* sw_make_metaobject(void *, char *, int *);
        void sw_delete(void *p);
        void* sw_qstring_to_utf8(void* s);

	void sw_map_children(void *x, void *y);

	void* sw_make_qpointer(void* target);
	bool sw_qpointer_is_null(void* x);
	void sw_delete_qpointer(void* x);

	int sw_windows_version();

	void sw_find_class(char *, Smoke **, short *);
	short sw_find_name(Smoke *, char *);
	short sw_id_method(Smoke *, short, short);
	short sw_id_type(Smoke *, char *);
	short sw_id_class(Smoke *, char *, bool);

	void* sw_make_qstringlist();
	void sw_delete_qstringlist(void *);
	void sw_qstringlist_append(void*, char*);

#ifdef __cplusplus
}
#endif
