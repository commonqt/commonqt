CFLAGS:=-I/usr/include/qt4
LDFLAGS:=

all: libcommonqt.so

.PHONY: clean
clean: faslclean
	rm -f libcommonqt.so commonqt.o

.PHONY: faslclean
faslclean:
	rm -f *.fasl tutorial/*.fasl

%.o: %.cpp
	c++ -fPIC -c -o $@ $< -I. $(CFLAGS)

libcommonqt.so: commonqt.o
	c++ -fPIC -shared -o $@ $^ -lsmokeqt4 $(LDFLAGS)
