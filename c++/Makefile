all: so dylib

#compile the dynamic library for linux
so:
	g++ -fPIC utilities.cpp problem_wrapper.cpp gecode_problem.cpp -std=c++11 -shared -lgecodedriver -lgecodeflatzinc -lgecodefloat -lgecodeint -lgecodekernel -lgecodeminimodel -lgecodesearch -lgecodeset -lgecodesupport -o ../sources/libgecode.so -O3 -ffast-math

#compile the dynamic library for macOS
dylib: problem_wrapper.cpp
	g++  problem_wrapper.cpp gecode_problem.cpp -std=c++11 -dynamiclib -fPIC -F/Library/Frameworks -framework gecode -o ../sources/libgecode.dylib  -ffast-math -O3
	install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode ../sources/libgecode.dylib

#run the main.cpp file on ubuntu
run:
	g++ -I/home/gecode-release-6.2.0/include -c utilities.cpp gecode_problem.cpp main.cpp
	g++ -o main -L/home/gecode-release-6.2.0/lib utilities.o gecode_problem.o main.o -lgecodesearch -lgecodeminimodel -lgecodeint -lgecodekernel -lgecodesupport
	./main

#remove temporary files and dynamic library files
clean:
	rm -f *.o *.so *.dylib ../sources/*.so ../sources/*.dylib Main
