// -*- c-basic-offset: 8; indent-tabs: nil -*-
//
// See LICENSE for details.
//
#include <smoke.h>
#include <smoke/qtcore_smoke.h>
#include <QtCore>
#include <QtGui>
#include "commonqt.h"

// #define DEBUG 1

#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;

typedef void (*t_deletion_callback)(void*);
typedef bool (*t_callmethod_callback)(void*, short, void*, void*);
typedef bool (*t_dynamic_callmethod_callback)(void*, short, short, void*, void*);
typedef bool (*t_metacall_callback)(void*, int, int, void*);

class Binding : public SmokeBinding
{
public:
        Binding(Smoke* s) : SmokeBinding(s) {}

        t_deletion_callback deletion_callback;
        t_callmethod_callback callmethod_callback;

        void deleted(Smoke::Index, void* obj) {
                deletion_callback(obj);
        }

        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool)
        {
		Smoke::Method* m = &smoke->methods[method];
		const char* name = smoke->methodNames[m->name];

		if (*name == '~')
			callmethod_callback(smoke, method, obj, args);

		return false;
	}

        char* className(Smoke::Index classId) {
                return (char*) smoke->classes[classId].className;
        }
};

class DynamicBinding : public Binding
{
public:
        DynamicBinding(Smoke* s) : Binding(s) {}

        QHash<short, short> overridenMethods;
        QMetaObject* metaObject;
        short metaObjectIndex;
        Smoke::Index metacallMethod;
        Smoke::ClassFn metacallClassFn;
        short metacallIndex;

        t_dynamic_callmethod_callback callmethod_callback;
        t_metacall_callback metacall_callback;

        int call_metacall(void*, Smoke::Stack);
        
        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool)
        {
                
                if (method == metaObjectIndex) {
                        args[0].s_voidp = (void*)metaObject;
                        return true;
                }
                if (method == metacallIndex) {
                        return call_metacall(obj, args);     
                }
                else if (overridenMethods.contains(method)) {
                        short override_index = overridenMethods.value(method);
                        
                        return callmethod_callback(smoke, method,
                                                   override_index,
                                                   obj, args);
                }
                else {
                        return false;
                }
        }
};

int DynamicBinding::call_metacall(void* obj, Smoke::Stack args)
{
        metacallClassFn(metacallMethod, obj, args);
        int slot_or_signal_index = (int)args[0].s_int;

        QMetaObject::Call call = (QMetaObject::Call)args[1].s_enum;

        if (slot_or_signal_index < 0 ||
            call != QMetaObject::InvokeMetaMethod)
                return slot_or_signal_index;

        int id = (int)args[2].s_int;
        void **method_args = (void**)args[3].s_class;

        metacall_callback(obj, id, slot_or_signal_index, method_args);
        return -1;
}

void
sw_smoke(Smoke* smoke,
	 SmokeData* data,
	 void* deletion_callback,
	 void* method_callback)
{
        Binding* binding = new Binding(smoke);

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

	binding->deletion_callback
		= (t_deletion_callback) deletion_callback;

        binding->callmethod_callback
                = (t_callmethod_callback) method_callback;

        data->binding = binding;
}

void sw_override(DynamicBinding* dynamicBinding, short method,
                 short override_index)
{
        dynamicBinding->overridenMethods[method] = override_index;
}

void* sw_make_dynamic_binding(Smoke* smoke,
                              QMetaObject* metaObject,
                              short metaObjectIndex,
                              short metacallIndex,
                              void* deletion_callback,
                              void* method_callback,
                              void* metacall_callback) {
        DynamicBinding* dynamicBinding = new DynamicBinding(smoke);

        dynamicBinding->deletion_callback
		= (t_deletion_callback) deletion_callback;

        dynamicBinding->callmethod_callback
                = (t_dynamic_callmethod_callback) method_callback;

        dynamicBinding->metacall_callback
		= (t_metacall_callback) metacall_callback;

        dynamicBinding->metaObject = metaObject;
        dynamicBinding->metaObjectIndex = metaObjectIndex;

        dynamicBinding->metacallIndex = metacallIndex;
        Smoke::Method* method = smoke->methods + metacallIndex;
        dynamicBinding->metacallClassFn = smoke->classes[method->classId].classFn;
        dynamicBinding->metacallMethod = method->method;
        return dynamicBinding;
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
sw_make_qvector_uint(unsigned int *data, int size)
{
	QVector<unsigned int>* v = new QVector<unsigned int>(size);
	for (int i = 0; i < size; i++) {
		(*v)[i] = data[i];
	}
	return v;
}

void
sw_delete_qvector_uint(void *v)
{
	delete (QVector<unsigned int> *) v;
}

void*
sw_make_qbytearray(char* str)
{
        return new QByteArray(str);
}

void
sw_delete_qbytearray(void *q)
{
        delete (QByteArray*) q;
}

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

void
sw_find_class(char *name, Smoke **smoke, short *index)
{
	Smoke::ModuleIndex mi = qtcore_Smoke->findClass(name);
	*smoke = mi.smoke;
	*index = mi.index;
}

void
sw_id_instance_class(void *ptr, Smoke **smoke, short *index)
{
	Smoke::ModuleIndex mi = qtcore_Smoke->findClass(((QObject*)ptr)->metaObject()->className());
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

// QStringList marshalling

void*
sw_qstringlist_new()
{
        return new QStringList();
}

int
sw_qstringlist_size(void* q)
{
	return static_cast<QStringList*>(q)->size();
}

void
sw_qstringlist_delete(void *q)
{
	delete static_cast<QStringList*>(q);
}

const void*
sw_qstringlist_at(void* q, int index)
{
	return static_cast<QStringList*>(q)->at(index).utf16();
}

void
sw_qstringlist_append(void *q, char *x)
{
	static_cast<QStringList*>(q)->append(QString(QString::fromUtf8(x)));
}

// QList<Something*> marshalling

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

// QList<scalar> marshalling

#define DEFINE_QLIST_SCALAR_MARSHALLER(ELEMENT_TYPE, NAME) \
  void* \
  sw_qlist_##NAME##_new() \
  { \
          return new QList<ELEMENT_TYPE>; \
  } \
  int \
  sw_qlist_##NAME##_size(void *ptr) \
  { \
        QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	return qlist->size(); \
  } \
  void \
  sw_qlist_##NAME##_delete(void *ptr) \
  { \
  	QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	delete qlist; \
  } \
  const void* \
  sw_qlist_##NAME##_at(void *ptr, int index) \
  { \
  	QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	return &qlist->at(index); \
  } \
  void \
  sw_qlist_##NAME##_append(void *ptr, void *whatptr) \
  { \
  	QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	qlist->append(*static_cast<ELEMENT_TYPE*>(whatptr)); \
  }

DEFINE_QLIST_SCALAR_MARSHALLER(int, int)
DEFINE_QLIST_SCALAR_MARSHALLER(QPrinter::PageSize,papersize)
DEFINE_QLIST_SCALAR_MARSHALLER(QVariant, qvariant)
DEFINE_QLIST_SCALAR_MARSHALLER(QByteArray, qbytearray)
DEFINE_QLIST_SCALAR_MARSHALLER(QModelIndex, qmodelindex)
DEFINE_QLIST_SCALAR_MARSHALLER(QKeySequence, qkeysequence)
DEFINE_QLIST_SCALAR_MARSHALLER(QTextEdit::ExtraSelection, extraselection)
