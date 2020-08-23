Commonqt version compatible with Qt5.
Instructions to build the libraries for commonqt.

In Linux I installed from scratch on top of Ubuntu MATE 20.04.

Previous requirements:
gcc
Qt 5.15.0 
OpenGL dev
cmake
git

Install llvm 9, if you use llvm 10 you will need smokegen-llvm-10.patch to build smokegen 
from  commonqt/Instructions.

sudo apt-get install libllvm9 llvm-9 llvm-9-dev llvm-9-runtime
sudo apt-get install clang-9 clang-tools-9 libclang-common-9-dev libclang-9-dev libclang1-9 clang-format-9 clangd-9

Always use your Qt5 path where required:

git clone -b clang https://github.com/chrisburel/smokegen
cd smokegen
mkdir build 
cd build

cmake -DCMAKE_INSTALL_PREFIX=/usr/local \
      -DLLVM_DIR="/usr/lib/llvm-9/cmake" \
      -DQt5_DIR="~/Qt/5.15.0/gcc_64/lib/cmake/Qt5" \
      -G "Unix Makefiles" ../
make 
sudo make install

cd ../../

git clone -b qt5 https://github.com/chrisburel/smokeqt
cd smokeqt
mkdir build 
cd build

cmake -DCMAKE_INSTALL_PREFIX=/usr/local \
      -DQt5_DIR="~/Qt/5.15.0/gcc_64/lib/cmake/Qt5" \
      -G "Unix Makefiles" ../

make

Correct any errors that appear, see files in commonqt/Instructions: 

manual-modification-smokeqtcore.txt
manual-modification-smokeqtgui.txt
manual-modification-smokeqtnetwork.txt 
manual-modification-smokeqtsql.txt 
manual-modification-smokeqtwebenginewidgets.txt 
manual-modification-smokeqtwidgets.txt
 
sudo make install

cd ../../

git clone -b commonqt5 https://github.com/mcristg/commonqt

cd commonqt

export PATH=$PATH:~/Qt/5.15.0/gcc_64/bin

Change unix:INCLUDEPATH with your Qt5 path.

qmake "unix:INCLUDEPATH+=/your path/Qt/5.15.0/gcc_64/include/QtCore/5.15.0 /your path/Qt/5.15.0/gcc_64/include/QtCore/5.15.0/QtCore" \
       commonqt.pro

make
sudo make install

In the terminal (your Qt5 path)
export LD_LIBRARY_PATH=/lib:/usr/lib:/usr/local/lib:/your path/Qt/5.15.0/gcc_64/lib

or in /etc 
create qt5_lib.conf to add the path to qt5 lib with the following 2 lines (your Qt5 path)

include /etc/ld.so.conf.d/*.conf
/your path/Qt/5.15.0/gcc_64/lib

and then run:
sudo ldconfig

Test commonqt5.

sbcl 

(ql:quickload :cffi)

(ql:quickload :named-readtables)
(ql:quickload :cl-ppcre)
(ql:quickload :closer-mop)
(ql:quickload :iterate)
(ql:quickload :trivial-garbage)

(pushnew "/your path/dev/commonqt/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op 'qt)
(asdf:oos 'asdf:load-op 'qt-tutorial)

(qt-tutorial-14:main)

In Win 10.

Previous requirements:

Visual studio 2019 16.7.1 Community version
Clang/LLVM Support in Visual Studio 
see:
https://devblogs.microsoft.com/cppblog/clang-llvm-support-in-visual-studio/
https://docs.microsoft.com/en-us/cpp/build/clang-support-msbuild?view=vs-2019#:~:text=To%20configure%20a%20Visual%20Studio,-cl)%20and%20then%20OK.
Qt 5.15.0 
cmake
git

Download llvm and clang version 10.0.1

qtenv2.bat
%comspec% /k "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvars64.bat"

open win 10 terminal (cmd)

cd C:\dev\llvm-10.0.1.src
mkdir build
cd build

cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD=X86 -DCMAKE_INSTALL_PREFIX=C:/dev/llvm-10 ^
      -DLLVM_INCLUDE_TESTS=Off -DLLVM_INCLUDE_EXAMPLES=Off -G "Visual Studio 16 2019" -T ClangCL ../
  
MsBuild LLVM.sln /t:Build /p:Configuration=Release 
MsBuild INSTALL.vcxproj /t:Build /p:Configuration=Release	
cd ..\..\

Always use your Qt5 path where required:

copy smokegen-llvm-10.patch to smokegen dir from commonqt/Instructions.
cd smokegen
git apply -p1 < smokegen-llvm-10.patch
mkdir build 
cd build

cmake -DCMAKE_INSTALL_PREFIX=C:/dev/smokegen-lib -DLLVM_DIR="C:/dev/llvm-10/lib/cmake/llvm" ^
      -DQt5_DIR="C:/dev/Qt/5.15.0/msvc2019_64/lib/cmake/Qt5" -G "Visual Studio 16 2019" -T ClangCL ../

MsBuild smokegenerator.sln /t:Build /p:Configuration=Release 
MsBuild INSTALL.vcxproj /t:Build /p:Configuration=Release	  
cd ..\..\


git clone -b qt5 https://github.com/chrisburel/smokeqt
cd smokeqt
mkdir build 
cd build
	  
cmake -DCMAKE_INSTALL_PREFIX=C:/dev/smokeqt-lib ^
      -DQt5_DIR="C:/dev/Qt/5.15.0/msvc2019_64/lib/cmake/Qt5" ^
      -G "Visual Studio 16 2019" -A x64 -Thost=x64 ../	

MsBuild SMOKEQT5.sln /t:Build /p:Configuration=Release 

Correct any errors that appear, see files in commonqt/Instructions: 
manual-modification-smokeqtcore.txt
manual-modification-smokeqtgui.txt
manual-modification-smokeqtnetwork.txt
manual-modification-smokeqtsql.txt
manual-modification-smokeqtwebenginewidgets.txt
manual-modification-smokeqtwidgets.txt
  
MsBuild INSTALL.vcxproj /t:Build /p:Configuration=Release	

cd ..\..\

Copy smoke.h from your "path\smokegen-lib\include" to "your path\smokeqt-lib\include",
smokebase.dll from your "path\smokegen-lib\bin" to "your path\smokeqt-lib\bin"
and smokebase.lib from your "path\smokegen-lib\lib" to "your path\smokeqt-lib\lib"

git clone -b commonqt5 https://github.com/mcristg/commonqt

cd commonqt

Change win32:INCLUDEPATH with your Qt5 path.

qmake "win32:INCLUDEPATH+=C:/dev/Qt/5.15.0/msvc2019_64/include/QtCore/5.15.0 C:/dev/Qt/5.15.0/msvc2019_64/include/QtCore/5.15.0/QtCore c:/dev/smokeqt-lib/include" ^
      "QMAKE_LIBDIR+=c:/dev/smokeqt-lib/lib" "DLLDESTDIR=c:/dev/smokeqt-lib/bin" commonqt.pro
	  

MsBuild commonqt.vcxproj /t:Build /p:Configuration=Release	

close win 10 terminal (cmd) and add your path\smokeqt-lib\bin path to your system

open win 10 terminal (cmd)

sbcl 

(ql:quickload :cffi)

(ql:quickload :named-readtables)
(ql:quickload :cl-ppcre)
(ql:quickload :closer-mop)
(ql:quickload :iterate)
(ql:quickload :trivial-garbage)

(pushnew "c:/dev/commonqt/" asdf:*central-registry*)
(asdf:oos 'asdf:load-op 'qt)
(asdf:oos 'asdf:load-op 'qt-tutorial)

(qt-tutorial-14:main)	  
	  
