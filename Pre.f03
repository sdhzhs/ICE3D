Subroutine Pre
use COM
implicit none
integer i,j,l,k,Kmin
real(8) d,dmin
DO l=1,block
  if(advancemethod=='multi') then
  Grids(l)%Xp0=Grids(l)%Xp0*c
  Grids(l)%Yp0=Grids(l)%Yp0*c
  Grids(l)%Zp0=Grids(l)%Zp0*c
  else if(advancemethod=='singl') then
  Grids(l)%Xp=Grids(l)%Xp*c
  Grids(l)%Yp=Grids(l)%Yp*c
  Grids(l)%Zp=Grids(l)%Zp*c
  end if
end DO
DO l=1,block
  DO j=1,Jg(l)
    DO i=1,Ig(l)
    Grids(l)%X(i,j)=0.25*(Grids(l)%Xp(i,j)+Grids(l)%Xp(i+1,j)+Grids(l)%Xp(i,j+1)+Grids(l)%Xp(i+1,j+1))
    Grids(l)%Y(i,j)=0.25*(Grids(l)%Yp(i,j)+Grids(l)%Yp(i+1,j)+Grids(l)%Yp(i,j+1)+Grids(l)%Yp(i+1,j+1))
    Grids(l)%Z(i,j)=0.25*(Grids(l)%Zp(i,j)+Grids(l)%Zp(i+1,j)+Grids(l)%Zp(i,j+1)+Grids(l)%Zp(i+1,j+1))
    if(advancemethod=='multi') then
    Grids(l)%X0(i,j)=0.25*(Grids(l)%Xp0(i,j)+Grids(l)%Xp0(i+1,j)+Grids(l)%Xp0(i,j+1)+Grids(l)%Xp0(i+1,j+1))
    Grids(l)%Y0(i,j)=0.25*(Grids(l)%Yp0(i,j)+Grids(l)%Yp0(i+1,j)+Grids(l)%Yp0(i,j+1)+Grids(l)%Yp0(i+1,j+1))
    Grids(l)%Z0(i,j)=0.25*(Grids(l)%Zp0(i,j)+Grids(l)%Zp0(i+1,j)+Grids(l)%Zp0(i,j+1)+Grids(l)%Zp0(i+1,j+1))
    end if
    end DO
  end DO
end DO
open(unit=1,file='pre.dat',status='replace')
DO l=1,block
write(1,formc(l)) Grids(l)%X
write(1,formc(l)) Grids(l)%Y
write(1,formc(l)) Grids(l)%Z
write(1,formc(l)) Forces(l)%beta
write(1,formc(l)) Forces(l)%dropv
end DO
close(1)
end subroutine Pre
