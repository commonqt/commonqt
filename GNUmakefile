CFLAGS=-I/usr/include/qt4 -I/home/david/src/kdebindings/smoke
LDFLAGS:=-L/home/david/src/kdebindings/lib

#CFLAGS=-I/e/Qt/4.5.0/include -I/e/kdebindings/smoke
#LDFLAGS:=-lQtCore4 -lQtGui4 -L/e/Qt/4.5.0/lib -L/e/kdebindings/windows/lib

all: libcommonqt.so

.PHONY: clean
clean: faslclean
	rm -f libcommonqt.so commonqt.o

.PHONY: faslclean
faslclean:
	rm -f *.fasl tutorial/*.fasl
	rm -f *.lx32fsl tutorial/*.lx32fsl
	rm -f *.wx32fsl tutorial/*.wx32fsl

%.o: %.cpp
	c++ -fPIC -c -o $@ $< -I. $(CFLAGS)

libcommonqt.so: commonqt.o
	c++ -fPIC -shared -o $@ $^ -lsmokeqt $(LDFLAGS)
