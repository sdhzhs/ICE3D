vpath %.o obj
vpath %.exe bin
FC=gfortran
#FC=x86_64-w64-mingw32-gfortran
FCFLAGS=-Wall -ffree-line-length-none -O3
#FCFLAGS=-Wall -ffree-line-length-none -Og -g -fcheck=bounds -ffpe-trap=invalid,zero,overflow
ifdef HYPRE
	FCFLAGS += -DHYPRE -I/home/sdhzhs/library/hypre-2.11.2/src/hypre/include
endif
LDFLAGS=-O3
#LDFLAGS=-Og -g -static -fcheck=bounds -ffpe-trap=invalid,zero,overflow
ifdef HYPRE
	LIBS=/home/sdhzhs/library/hypre-2.11.2/src/hypre/lib/libHYPRE.so
endif
src:=$(sort $(wildcard *.[fF]03))
objects:=$(src:.f03=.o)
objects:=$(objects:.F03=.o)
ifdef HYPRE
	src += Hypresolver.f90
	objects += Hypresolver.o
endif
exec:=ICE3D.exe
bprefix:=bin/
oprefix:=obj/
bdir:=bin
odir:=obj
cdir:=cases
ddir:=doc
installdir:=v1.0
mingwdir:=/c/MinGW64/bin
relname:=ICE3D1.0_win64_binary
md:=mkdir -p
rd:=rm -r -f
del:=rm -f
cp:=cp -f
cd:=cp -r -f
zip:=7z a -tzip
%.o:%.f90 COM.mod
	$(FC) -c $(FCFLAGS) -I$(odir) $< -o $(addprefix $(oprefix),$@)
%.o:%.f03 COM.mod
	$(FC) -c $(FCFLAGS) -I$(odir) $< -o $(addprefix $(oprefix),$@)
%.o:%.F03 COM.mod
	$(FC) -c $(FCFLAGS) -I$(odir) $< -o $(addprefix $(oprefix),$@)
%.mod:%.f03|$(odir)
	$(FC) -c $(FCFLAGS) -J$(odir) $< -o $(addprefix $(oprefix),$(subst .mod,.o,$@))
$(exec):$(objects)|$(bdir)
	$(FC) $(LDFLAGS) $(addprefix $(oprefix),$(objects)) $(LIBS) -o $(addprefix $(bprefix),$(exec))
$(odir):
	-$(md) $(odir)
$(bdir):
	-$(md) $(bdir)
.PHONY:clean
clean:
	-$(del) *.o *.mod *.exe *.xyz *.dat *.zip *.txt
	-$(rd) $(odir) $(bdir)
.PHONY:pkg
pkg:$(installdir)
	-$(zip) $(relname).zip $(installdir)
.PHONY:install
install:$(exec)|$(installdir)
	-$(cp) $(bdir)/$(exec) $(installdir)
	-$(cp) LICENSE $(installdir)
	-$(cp) README.md $(installdir)
	-$(cd) $(cdir) $(installdir)
	-$(cd) $(ddir) $(installdir)
$(installdir):
	-$(md) $(installdir)
.PHONY:uninstall
uninstall:
	-$(rd) $(installdir)
