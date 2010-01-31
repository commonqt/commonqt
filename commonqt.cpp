// -*- c-basic-offset: 8; indent-tabs: nil -*-
//
// See LICENSE for details.
//
#include <smoke.h>
#include <Qt/qstring.h>
#include <Qt/qstringlist.h>
#include <Qt/qpointer.h>
#include <Qt/qmetaobject.h>
#include <QtCore/qobject.h>
#include <QtGui/qapplication.h>
#include "commonqt.h"

// work around bugs in CCL cdecl callback support
#ifdef COMMONQT_USE_STDCALL
#define MAYBE_STDCALL __stdcall
#else
#define MAYBE_STDCALL
#endif

// #define DEBUG 1

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
			 && (!strcmp(c->className, "QApplication")
			    || !strcmp(c->className, "QCoreApplication")))
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
                return callmethod_callback(smoke, method, obj, args, isAbstract);
        }

        char* className(Smoke::Index classId) {
                return (char*) smoke->classes[classId].className;
        }
};

void
sw_smoke(Smoke* smoke,
	 SmokeData* data,
	 void* deletion_callback,
	 void* method_callback,
	 void* child_callback)
{
        ThinBinding* thinBinding = new ThinBinding(smoke);
        FatBinding* fatBinding = new FatBinding(smoke);

	data->name = smoke->moduleName();

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
sw_make_qstringlist()
{
        return new QStringList();
}

void
sw_delete_qstringlist(void *q)
{
	delete static_cast<QStringList*>(q);
}

void
sw_qstringlist_append(void *q, char *x)
{
	static_cast<QStringList*>(q)->append(QString(x));
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

extern Smoke *qtcore_Smoke;

void
sw_find_class(char *name, Smoke **smoke, short *index)
{
	Smoke::ModuleIndex mi = qtcore_Smoke->findClass(name);
	*smoke = mi.smoke;
	*index = mi.index;
}

short
sw_find_name(Smoke *smoke, char *name)
{
	Smoke::ModuleIndex mi = smoke->idMethodName(name);
	return mi.index;
}

short
sw_id_method(Smoke *smoke, short classIndex, short name)
{
	Smoke::ModuleIndex mi = smoke->idMethod(classIndex, name);
	return mi.index;
}

short
sw_id_type(Smoke *smoke, char *name)
{
	return smoke->idType(name);
}
