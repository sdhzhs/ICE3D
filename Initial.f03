Subroutine Initial
use COM
implicit none
integer i,j,l,m,n
real(8) d,dmin
if(initialtimecontrol=='Y') then
if(icecoupled=='N') then
bp=0
else if(icecoupled=='Y') then
bp=5e-6
end if
DO l=1,block
Icecoordinates(l)%h=hp
Icecoordinates(l)%b=bp
end DO
time=0
else if(initialtimecontrol=='N') then
if(advancemethod=='singl') then
open(unit=1,file=filename(5),status='old')
DO l=1,block
read(1,formc(l)) Icecoordinates(l)%h
read(1,formc(l)) Icecoordinates(l)%b
end DO
read(1,*) time
read(1,*) hp
close(1)
print *,'Read initial parameters completed!'
else if(advancemethod=='multi') then
DO l=1,block
DO j=1,Jg(l)
 DO i=1,Ig(l)
  dmin=1
  DO m=1,Jg(l)
   DO n=1,Ig(l)
   d=sqrt((Grids(l)%X(i,j)-Grids(l)%X0(n,m))**2+(Grids(l)%Y(i,j)-Grids(l)%Y0(n,m))**2+(Grids(l)%Z(i,j)-Grids(l)%Z0(n,m))**2)
   if(d<dmin) then
   dmin=d
   end if
   end DO
  end DO
 Icecoordinates(l)%b(i,j)=dmin
 end DO
end DO
Icecoordinates(l)%h=hp
end DO
Call innerBCtransfer
DO l=1,block
Icecoordinates(l)%b0=Icecoordinates(l)%b
Boundatas(l)%bbcl0=Boundatas(l)%bbcl
Boundatas(l)%bbcr0=Boundatas(l)%bbcr
Boundatas(l)%bbcu0=Boundatas(l)%bbcu
Boundatas(l)%bbcd0=Boundatas(l)%bbcd
end DO
end if
end if
open(unit=1,file='force.dat',status='replace')
DO l=1,block
write (1,formc(l)) Forces(l)%A1
end DO
DO l=1,block
write (1,formc(l)) Forces(l)%A2
end DO
DO l=1,block
write (1,formc(l)) Forces(l)%G1
end DO
DO l=1,block
write (1,formc(l)) Forces(l)%G2
end DO
close(1)
if(icecoupled=='N') then
filename(1)='Grids(t=00).xyz'
open(unit=1,file=filename(1),status='replace')
write(1,*) block
DO l=1,block
write(1,*) Ig(l),Jg(l),1
end DO
DO l=1,block
write(1,formc(l)) Grids(l)%X
write(1,formc(l)) Grids(l)%Y
write(1,formc(l)) Grids(l)%Z
end DO
close(1)
filename(1)='Filmsol(t=00).dat'
open(unit=2,file=filename(1),status='replace')
write(2,*) block
DO l=1,block
write(2,*) Ig(l),Jg(l),1,1
end DO
DO l=1,block
write(2,formc(l)) Icecoordinates(l)%h
end DO
write(2,*) time
write(2,*) hp
close(2)
end if
end Subroutine Initial
