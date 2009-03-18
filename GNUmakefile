CFLAGS=-I/usr/include/qt4 -I/home/david/src/kdebindings/smoke
LDFLAGS:=-L/home/david/src/kdebindings/lib

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
	c++ -fPIC -shared -o $@ $^ -lsmokeqt $(LDFLAGS)
