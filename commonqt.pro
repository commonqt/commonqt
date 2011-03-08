unix:TEMPLATE     = lib
win32:TEMPLATE    = vclib

HEADERS     += commonqt.h
SOURCES     += commonqt.cpp
CONFIG      += qt thread debug dll

unix:LIBS += -lsmokeqtcore
win32:LIBS += smokebase.lib
