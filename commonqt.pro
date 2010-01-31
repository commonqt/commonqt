TEMPLATE     = lib
HEADERS     += commonqt.h
SOURCES     += commonqt.cpp
CONFIG      += qt thread debug dll
INCLUDEPATH += /home/david/src/kdebindings/smoke
LIBS        += -lsmokeqtcore -L/home/david/src/kdebindings/smoke/qtcore
