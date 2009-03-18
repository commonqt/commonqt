// -*- c-basic-offset: 8; indent-tabs: nil -*-
//
// See LICENSE for details.
//
#include <smoke.h>
#include <Qt/qstring.h>
#include <Qt/qmetaobject.h>
#include "commonqt.h"

extern Smoke* qt_Smoke;
extern void init_qt_Smoke();

#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;

typedef bool (*t_callmethod_callback)(short, void*, void*, bool);

class CommonQtBinding : public SmokeBinding
{
public:
        CommonQtBinding(Smoke* s) : SmokeBinding(s) {}

        t_callmethod_callback callmethod_callback;

        void deleted(Smoke::Index classId, void* obj) {
                callmethod_callback(-1, obj, 0, 0);
        }

        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool isAbstract)
        {
                return callmethod_callback(method, obj, args, isAbstract);
        }

        char* className(Smoke::Index classId) {
                char* real = (char*) smoke->classes[classId].className;
                char* prefix = (char*) "HACK_";
                char* result = (char*) malloc(strlen(real) + strlen(prefix) + 1);
                strcpy(result, prefix);
                strcpy(result + strlen(prefix), real);
                return result;
        }
};

static CommonQtBinding* commonQtBinding;

void*
sw_init(SmokeData *data, void *callback)
{
        init_qt_Smoke();
        commonQtBinding = new CommonQtBinding(qt_Smoke);

        data->classes = qt_Smoke->classes;
        data->numClasses = qt_Smoke->numClasses;

        data->methods = qt_Smoke->methods;
        data->numMethods = qt_Smoke->numMethods;

        data->methodMaps = qt_Smoke->methodMaps;
        data->numMethodMaps = qt_Smoke->numMethodMaps;

        data->methodNames = qt_Smoke->methodNames;
        data->numMethodNames = qt_Smoke->numMethodNames;

        data->types = qt_Smoke->types;
        data->numTypes = qt_Smoke->numTypes;

        data->inheritanceList = qt_Smoke->inheritanceList;
        data->argumentList = qt_Smoke->argumentList;
        data->ambiguousMethodList = qt_Smoke->ambiguousMethodList;
        data->castFn = (void *) qt_Smoke->castFn;

        commonQtBinding->callmethod_callback
                = (t_callmethod_callback) callback;

        return commonQtBinding;
}

void*
sw_make_qstring(char *str)
{
        return new QString(str);
}

void*
sw_qstring_to_utf8(void* s)
{
        QString* str = (QString*) s;
        return new QByteArray(str->toUtf8());
}

void
sw_delete_qstring(void *q)
{
        delete (QString*) q;
}

void*
sw_make_metaobject(void *p, char *strings, int *d)
{
        QMetaObject* parent = (QMetaObject*) p;
        const uint* data = (const uint*) d;

        QMetaObject tmp = { { parent, strings, data, 0 } };
        QMetaObject* ptr = new QMetaObject;
        *ptr = tmp;
        return ptr;
}

void
sw_delete(void *p)
{
        QObject* q = (QObject*) p;
        delete q;
}
