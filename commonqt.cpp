// -*- c-basic-offset: 8; indent-tabs: nil -*-
//
// See LICENSE for details.
//
#include <smoke.h>
#include <Qt/qstring.h>
#include <Qt/qpointer.h>
#include <Qt/qmetaobject.h>
#include <QtCore/qobject.h>
#include <QtGui/qapplication.h>
#include "commonqt.h"

extern Smoke* qt_Smoke;
extern void init_qt_Smoke();

#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;

static bool sw_event_notify(void **data);

typedef void (*t_deletion_callback)(void*);
typedef bool (*t_callmethod_callback)(short, void*, void*, bool);
typedef void (*t_child_callback)(bool, void*);

class CommonQtBinding : public SmokeBinding
{
public:
        CommonQtBinding(Smoke* s) : SmokeBinding(s) {}

        t_deletion_callback deletion_callback;
        t_callmethod_callback callmethod_callback;
	t_child_callback child_callback;

        void deleted(Smoke::Index classId, void* obj) {
                deletion_callback(obj);
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
sw_init(SmokeData *data,
	void *deletion_callback,
	void *method_callback,
	void *child_callback)
{
        init_qt_Smoke();
        qt_Smoke->binding = commonQtBinding = new CommonQtBinding(qt_Smoke);

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

	commonQtBinding->deletion_callback
		= (t_deletion_callback) deletion_callback;

        commonQtBinding->callmethod_callback
                = (t_callmethod_callback) method_callback;

	commonQtBinding->child_callback
		= (t_child_callback) child_callback;

#if QT_VERSION >= 0x40300
    QInternal::registerCallback(QInternal::EventNotifyCallback,
				sw_event_notify);
#else
#warn Old version of Qt detected.  At least Qt 4.3 is needed for memory management heuristics
        cout << "warning: Old version of Qt detected.  At least Qt 4.3 is needed for memory management heuristics" << endl;
#endif

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

static bool
sw_event_notify(void **data)
{
	QEvent* event = (QEvent*) data[1];

	if (event->type() != QEvent::ChildAdded
	    && event->type() != QEvent::ChildRemoved)
		return false;

	QChildEvent* e = static_cast<QChildEvent*>(event);
	commonQtBinding->child_callback(e->added(), e->child());
	return false;
}

typedef void (*t_ptr_callback)(void *);

// quick hack, to be rewritten once we have QList marshalling support
void
sw_map_children(void *x, void *y)
{
	t_ptr_callback cb = (t_ptr_callback) y;
	QObject* o = (QObject*) x;
	const QList<QObject*> l = o->children();
	for (int i=0; i < l.size(); ++i)
		cb(l.at(i));
}

void*
sw_make_qpointer(void* target)
{
        return new QPointer<QObject>((QObject*) target);
}

bool
sw_qpointer_is_null(void* x)
{
	QPointer<QObject>* ptr = (QPointer<QObject>*) x;
	return ptr->isNull();
}

void
sw_delete_qpointer(void* x)
{
	QPointer<QObject>* ptr = (QPointer<QObject>*) x;
	delete ptr;
}
