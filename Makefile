vpath %.o obj
vpath %.exe bin
FC=gfortran
FCFLAGS=-Wall -ffree-line-length-none -O3
LDFLAGS=-O3 -s
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
	$(FC) -c $(FCFLAGS) -I$(odir) $< -o $(addprefix $(oprefix),$@)
%.mod:%.f03|$(odir)
	$(FC) -c $(FCFLAGS) -J$(odir) $< -o $(addprefix $(oprefix),$(subst .mod,.o,$@))
$(exec):$(objects)|$(bdir)
	$(FC) $(LDFLAGS) $(addprefix $(oprefix),$(objects)) -o $(addprefix $(bprefix),$(exec))
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