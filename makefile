SRCDIR=src
OBJDIR=obj
FCFLAGS=-g -O3 -Wall -static -fPIC -J$(OBJDIR)

VPATH=%.f90 src
VPATH=%.o obj

SRC=vardef.f90 util.f90 memory.f90 optimization.f90 math_deps.f90 edge_util.f90 surface_util.f90 elliptic_surface_grid.f90 hyperbolic_surface_grid.f90 surface_grid.f90 main.f90

OBJ=$(patsubst %.f90, %.o, $(SRC))

SRCS=$(addprefix $(SRCDIR)/, $(SRC) )
OBJS=$(addprefix $(OBJDIR)/, $(OBJ) )

all: $(OBJDIR)/xfoil_deps.o $(OBJS) pymain.so

pymain.so : $(OBJDIR)/xfoil_deps.o $(OBJS) f90wrap_main.f90
	f2py-f90wrap -I$(OBJDIR) -c -m _pymain $(OBJDIR)/xfoil_deps.o $(OBJS) f90wrap_*.f90

f90wrap_main.f90 : $(SRCS)
	f90wrap -m pymain $(SRCS)

$(OBJDIR)/xfoil_deps.o : $(SRCDIR)/xfoil_deps.f
	gfortran -fdefault-real-8 $(FCFLAGS) -c -o $@ $<

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	@mkdir -p $(dir $@)
	gfortran $(FCFLAGS) -c -o $@ $<

clean:
	-rm $(OBJDIR)/*.o $(OBJDIR)/*.mod *.so f90wrap* pymain.py *.pyc mesh/*

test: all
	echo 'Test naca12           - 2D'
	python3 naca12.py -f mesh/naca12 -dz 0.
	echo 'Test naca12-sharp     - 2D'
	python3 naca12.py --sharp -f mesh/naca12 -dz 0.
	echo 'Test naca12-3D        - 3D'
	python3 naca12.py -f mesh/naca12-3D -dz 0.1
	echo 'Test naca12-3D-sharp - 3D'
	python3 naca12.py --sharp -f mesh/naca12-3D -dz 0.1

diff: test
	echo 'Test naca12.dat       - 2D'
	diff mesh/naca12.dat       ref/naca12.dat
	echo 'Test naca12-sharp.dat - 2D'
	diff mesh/naca12-sharp.dat ref/naca12-sharp.dat
	echo 'Test naca12.dat       - 3D'
	diff mesh/naca12-3D.dat       ref/naca12-3D.dat
	echo 'Test naca12-sharp.dat - 3D'
	diff mesh/naca12-3D-sharp.dat ref/naca12-3D-sharp.dat

