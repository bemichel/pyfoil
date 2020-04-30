FCFLAGS=-g -O3 -Wall -static

all: pymain.so

pymain.so : xfoil_deps.o vardef.o util.o memory.o optimization.o math_deps.o edge_util.o surface_util.o elliptic_surface_grid.o hyperbolic_surface_grid.o surface_grid.o main.o f90wrap_main.f90
	f2py-f90wrap -c -m _pymain *.o f90wrap_*.f90

f90wrap_main.f90 : vardef.f90 util.f90 memory.f90 optimization.f90 math_deps.f90 edge_util.f90 surface_util.f90 elliptic_surface_grid.f90 hyperbolic_surface_grid.f90 surface_grid.f90 main.f90
	f90wrap -m pymain vardef.f90 util.f90 memory.f90 optimization.f90 math_deps.f90 edge_util.f90 surface_util.f90 elliptic_surface_grid.f90 hyperbolic_surface_grid.f90 surface_grid.f90 main.f90

xfoil_deps.o : xfoil_deps.f
	gfortran -c -fPIC -fdefault-real-8 xfoil_deps.f

vardef.o : vardef.f90
	gfortran -c $(FCFLAGS) -fPIC vardef.f90

util.o : util.f90
	gfortran -c $(FCFLAGS) -fPIC util.f90

memory.o : memory.f90
	gfortran -c $(FCFLAGS) -fPIC memory.f90

optimization.o : optimization.f90
	gfortran -c $(FCFLAGS) -fPIC optimization.f90

math_deps.o : math_deps.f90
	gfortran -c $(FCFLAGS) -fPIC math_deps.f90

edge_util.o : edge_util.f90
	gfortran -c $(FCFLAGS) -fPIC edge_util.f90

surface_util.o : surface_util.f90
	gfortran -c $(FCFLAGS) -fPIC surface_util.f90

elliptic_surface_grid.o : elliptic_surface_grid.f90
	gfortran -c $(FCFLAGS) -fPIC elliptic_surface_grid.f90

hyperbolic_surface_grid.o : hyperbolic_surface_grid.f90
	gfortran -c $(FCFLAGS) -fPIC hyperbolic_surface_grid.f90

surface_grid.o : surface_grid.f90
	gfortran -c $(FCFLAGS) -fPIC surface_grid.f90

main.o : main.f90
	gfortran -c $(FCFLAGS) -fPIC main.f90

clean:
	-rm *.o *.so *.mod f90wrap* pymain.py *.pyc

test: mymodule.so
	python pyconstruct2d.py
