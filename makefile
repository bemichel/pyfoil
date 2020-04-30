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
	-rm $(OBJDIR)/*.o $(OBJDIR)/*.mod *.so f90wrap* pymain.py *.pyc

test: mymodule.so
	python pyconstruct2d.py
