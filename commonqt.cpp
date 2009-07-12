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

// work around bugs in CCL cdecl callback support
#ifdef WINDOWS
#define MAYBE_STDCALL __stdcall
#else
#define MAYBE_STDCALL
#endif

// #define DEBUG 1

extern void init_qt_Smoke();
extern void init_qtwebkit_Smoke();

#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;

typedef void (MAYBE_STDCALL *t_deletion_callback)(void*, void*);
typedef bool (MAYBE_STDCALL *t_callmethod_callback)(void*, short, void*, void*, bool);
typedef void (MAYBE_STDCALL *t_child_callback)(void*, bool, void*);

class ThinBinding : public SmokeBinding
{
public:
        ThinBinding(Smoke* s) : SmokeBinding(s) {}

        t_deletion_callback deletion_callback;
        t_callmethod_callback callmethod_callback;
	t_child_callback child_callback;

        void deleted(Smoke::Index classId, void* obj) {
                deletion_callback(smoke, obj);
        }

        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool isAbstract)
        {
		Smoke::Method* m = &smoke->methods[method];
		const char* name = smoke->methodNames[m->name];
		Smoke::Class* c = &smoke->classes[m->classId];
		if (*name == '~')
			callmethod_callback(smoke, method, obj, args, isAbstract);
		else if (!strcmp(name, "notify")
			 && !strcmp(c->className, "QApplication"))
		{
			QEvent* e = (QEvent*) args[2].s_voidp;
			if (e->type() == QEvent::ChildAdded
			    || e->type() == QEvent::ChildRemoved)
			{
				QChildEvent* f = (QChildEvent*) e;
				child_callback(smoke, f->added(), f->child());
			}
		}
		return false;
	}

        char* className(Smoke::Index classId) {
                return (char*) smoke->classes[classId].className;
        }
};

class FatBinding : public SmokeBinding
{
public:
	FatBinding(Smoke* s) : SmokeBinding(s) {}

        t_deletion_callback deletion_callback;
        t_callmethod_callback callmethod_callback;
	t_child_callback child_callback;

        void deleted(Smoke::Index classId, void* obj) {
                deletion_callback(smoke, obj);
        }

        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool isAbstract)
        {
#if 0
		{
			Smoke::Method* m = &qt_Smoke->methods[method];
			short ic = m->classId;
			short iname = m->name;
			Smoke::Class* c = &qt_Smoke->classes[ic];
			const char *name = qt_Smoke->methodNames[iname];

			cout << "calling " << c->className <<  "." << name << endl;
		}
#endif
                return callmethod_callback(smoke, method, obj, args, isAbstract);
        }

        char* className(Smoke::Index classId) {
#if 0
                char* real = (char*) smoke->classes[classId].className;
                char* prefix = (char*) "HACK_";
                char* result = (char*) malloc(strlen(real) + strlen(prefix) + 1);
                strcpy(result, prefix);
                strcpy(result + strlen(prefix), real);
                return result;
#endif
                return (char*) smoke->classes[classId].className;
        }
};

void
sw_init()
{
        init_qt_Smoke();
        init_qtwebkit_Smoke();
}

void
sw_smoke(Smoke* smoke,
	 SmokeData* data,
	 void* deletion_callback,
	 void* method_callback,
	 void* child_callback)
{
        ThinBinding* thinBinding = new ThinBinding(smoke);
        FatBinding* fatBinding = new FatBinding(smoke);

        data->classes = smoke->classes;
        data->numClasses = smoke->numClasses;

        data->methods = smoke->methods;
        data->numMethods = smoke->numMethods;

        data->methodMaps = smoke->methodMaps;
        data->numMethodMaps = smoke->numMethodMaps;

        data->methodNames = smoke->methodNames;
        data->numMethodNames = smoke->numMethodNames;

        data->types = smoke->types;
        data->numTypes = smoke->numTypes;

        data->inheritanceList = smoke->inheritanceList;
        data->argumentList = smoke->argumentList;
        data->ambiguousMethodList = smoke->ambiguousMethodList;
        data->castFn = (void *) smoke->castFn;

	fatBinding->deletion_callback
		= (t_deletion_callback) deletion_callback;

        fatBinding->callmethod_callback
                = (t_callmethod_callback) method_callback;

	fatBinding->child_callback
		= (t_child_callback) child_callback;

	thinBinding->deletion_callback = fatBinding->deletion_callback;
	thinBinding->callmethod_callback = fatBinding->callmethod_callback;
	thinBinding->child_callback = fatBinding->child_callback;

        data->thin = thinBinding;
        data->fat = fatBinding;
}

int
sw_windows_version()
{
#ifdef WINDOWS
	return QSysInfo::windowsVersion();
#else
	return -1;
#endif
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
