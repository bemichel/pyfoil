all: pymain.so

pymain.so : vardef.o main.o f90wrap_main.f90
	f2py-f90wrap -c -m _pymain *.o f90wrap_*.f90

f90wrap_main.f90 : vardef.f90 main.f90
	f90wrap -m pymain main.f90 vardef.f90

main.o : main.f90
	gfortran -c -fPIC main.f90

vardef.o : vardef.f90
	gfortran -c -fPIC vardef.f90

clean:
	-rm *.o *.so *.mod f90wrap* pymain.py *.pyc

test: mymodule.so
	python example.py
