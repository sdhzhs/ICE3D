vpath %.o obj
vpath %.exe bin
FC=gfortran
FCFLAGS=-W -fdefault-real-8 -fdefault-double-8 -ffree-line-length-none -O3
#FCFLAGS+=-ID:\Libsrc\hypre-2.11.1\src\hypre\dll\include
LDFLAGS=-O3 -s
#LDFLAGS+=-LD:\Libsrc\hypre-2.11.1\src\hypre\dll\lib -lHYPRE
src:=$(sort $(wildcard *.f03))
objects:=$(src:.f03=.o)
exec:=ICE3D.exe
bprefix:=bin/
oprefix:=obj/
bdir:=bin
odir:=obj
installdir:=/d/Project/ICE3D
md:=mkdir -p
rd:=rm -r -f
del:=rm -f
cp:=cp -f
%.o:%.f03 COM.mod
	$(FC) $(FCFLAGS) -I$(odir) -c $< -o $(addprefix $(oprefix),$@)
%.mod:%.f03|$(odir)
	$(FC) $(FCFLAGS) -J$(odir) -c $< -o $(addprefix $(oprefix),$(subst .mod,.o,$@))
$(exec):$(objects)|$(bdir)
	$(FC) $(addprefix $(oprefix),$(objects)) $(LDFLAGS) -o $(addprefix $(bprefix),$(exec))
$(odir):
	-$(md) $(odir)
$(bdir):
	-$(md) $(bdir)
.PHONY:clean
clean:
	-$(del) *.o *.mod *.exe *.dat
	-$(rd) $(odir) $(bdir)
.PHONY:install
install:$(installdir)/$(bdir)
	-$(cp) $(bdir)/$(exec) $(installdir)/$(bdir)
$(installdir)/$(bdir):
	-$(md) $(installdir)/$(bdir)
.PHONY:uninstall
uninstall:
	-$(del) $(installdir)/$(bdir)/$(exec)
	-$(rd) $(installdir)/$(bdir) $(installdir)