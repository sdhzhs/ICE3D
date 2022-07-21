vpath %.o obj
vpath %.exe bin
FC=gfortran
#FC=x86_64-w64-mingw32-gfortran
FCFLAGS=-Wall -ffree-line-length-none -O3
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
	-$(del) *.o *.mod *.exe *.dat *.zip
	-$(rd) $(odir) $(bdir)
.PHONY:pkg
pkg:$(installdir)
	-$(zip) $(relname).zip $(installdir)
.PHONY:install
install:$(exec)|$(installdir)
	-$(cp) $(bdir)/$(exec) $(installdir)
	-$(cp) LICENSE $(installdir)
	-$(cp) README.md $(installdir)
	#-$(cp) $(mingwdir)/libgfortran-3.dll $(installdir)
	-$(cp) $(mingwdir)/libquadmath-0.dll $(installdir)
	-$(cp) $(mingwdir)/libgomp-1.dll $(installdir)
	-$(cp) $(mingwdir)/libgcc_s_seh-1.dll $(installdir)
	-$(cp) $(mingwdir)/libwinpthread-1.dll $(installdir)
	-$(cd) $(cdir) $(installdir)
	-$(cd) $(ddir) $(installdir)
$(installdir):
	-$(md) $(installdir)
.PHONY:uninstall
uninstall:
	-$(rd) $(installdir)
