// -*- c-basic-offset: 8; indent-tabs: nil -*-
//
// See LICENSE for details.
//
#include <smoke.h>
#include <qtcore_smoke.h>
#include <Qt/qstring.h>
#include <Qt/qstringlist.h>
#include <Qt/qpointer.h>
#include <Qt/qmetaobject.h>
#include <QtCore/qobject.h>
#include <QtGui/qapplication.h>
#include "commonqt.h"

// #define DEBUG 1

#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;

typedef void (*t_deletion_callback)(void*, void*);
typedef bool (*t_callmethod_callback)(void*, short, void*, void*, bool);
typedef void (*t_child_callback)(void*, bool, void*);

class ThinBinding : public SmokeBinding
{
public:
        ThinBinding(Smoke* s) : SmokeBinding(s) {}

        t_deletion_callback deletion_callback;
        t_callmethod_callback callmethod_callback;
	t_child_callback child_callback;

        void deleted(Smoke::Index, void* obj) {
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

        void deleted(Smoke::Index, void* obj) {
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

// void*
// sw_make_qstring(char *str)
// {
//         QString* qstr = new QString();
//         *qstr = QString::fromUtf8(str);
//         return qstr;
// }

void*
sw_make_qstring(char *str)
{
        return new QString(QString::fromUtf8(str));
}

void*
sw_qstring_to_utf8(void* s)
{
        QString* str = (QString*) s;
        return new QByteArray(str->toUtf8());
}

const void*
sw_qstring_to_utf16(void* s)
{
        QString* str = static_cast<QString*>(s);
	return str->utf16();
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

short
sw_id_class(Smoke *smoke, char *name, bool external)
{
	return smoke->idClass(name, external).index;
}

// fixme: cl-smoke has nice macros wrapping qlist access.
// Should reuse those instead of hand-coding here.
void*
sw_qlist_variant_new()
{
	return new QList<QVariant>;
}

int
sw_qlist_variant_size(void *ptr)
{
	QList<QVariant>* qlist = static_cast<QList<QVariant>*>(ptr);
	return qlist->size();
}

void
sw_qlist_variant_delete(void *ptr)
{
	QList<QVariant>* qlist = static_cast<QList<QVariant>*>(ptr);
	delete qlist;
}

const void*
sw_qlist_variant_at(void *ptr, int index)
{
	QList<QVariant>* qlist = static_cast<QList<QVariant>*>(ptr);
	return new QVariant(qlist->at(index));
}

void
sw_qlist_variant_append(void *ptr, void *whatptr)
{
	QList<QVariant>* qlist = static_cast<QList<QVariant>*>(ptr);
	QVariant* what = static_cast<QVariant*>(whatptr);
	
	qlist->append(*what);
}

// int
void*
sw_qlist_int_new()
{
	return new QList<int>;
}

int
sw_qlist_int_size(void *ptr)
{
	QList<int>* qlist = static_cast<QList<int>*>(ptr);
	return qlist->size();
}

void
sw_qlist_int_delete(void *ptr)
{
	QList<int>* qlist = static_cast<QList<int>*>(ptr);
	delete qlist;
}

int
sw_qlist_int_at(void *ptr, int index)
{
	QList<int>* qlist = static_cast<QList<int>*>(ptr);
	return qlist->at(index);
}

void
sw_qlist_int_append(void *ptr, int what)
{
	QList<int>* qlist = static_cast<QList<int>*>(ptr);
	qlist->append(what);
}

// void
void*
sw_qlist_void_new()
{
	return new QList<void*>;
}

int
sw_qlist_void_size(void *ptr)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	return qlist->size();
}

void
sw_qlist_void_delete(void *ptr)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	delete qlist;
}

const void*
sw_qlist_void_at(void *ptr, int index)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	return qlist->at(index);
}

void
sw_qlist_void_append(void *ptr, void *whatptr)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	
	qlist->append(whatptr);
}
