unix:TEMPLATE     = lib
win32:TEMPLATE    = vclib

HEADERS     += commonqt.h
SOURCES     += commonqt.cpp
CONFIG      += qt thread debug dll

unix:INCLUDEPATH  += /usr/include/smoke/
win32:INCLUDEPATH += c:\Users\david\kdebindings-656d8106\smoke\
win32:INCLUDEPATH += c:\Users\david\kdebindings-656d8106\smoke\qtcore

unix:LIBS += -lsmokeqtcore -L/usr/lib
win32:LIBS += c:\Users\david\kdebindings-656d8106\build\smoke\qtcore\Debug\smokeqtcore.lib
