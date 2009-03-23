#ifdef __cplusplus
extern "C" {
#endif

        typedef struct SmokeData {
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
        } SmokeData;

        void* sw_init(SmokeData *data, void *, void *, void *);
        void* sw_make_qstring(char *);
        void sw_delete_qstring(void *);
        void* sw_make_metaobject(void *, char *, int *);
        void sw_delete(void *p);
        void* sw_qstring_to_utf8(void* s);

#ifdef __cplusplus
}
#endif
