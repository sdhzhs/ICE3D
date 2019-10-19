Subroutine Forceparameter(l)
use COM
implicit none
integer i,j,l
real(8) gx,gy,gz,dux,duy,duz,dvx,dvy,dvz,dnx,dny,dnz
real(8) Ax,Ay,Az,DE,DG,DF,panel,A1,A2,G1,G2,G3
real(8),dimension(:,:),pointer::Xp,Yp,Zp

Xp=>Grids(l)%Xp
Yp=>Grids(l)%Yp
Zp=>Grids(l)%Zp
gx=0
gy=-g
gz=0
DO i=1,Ig(l)
  DO j=1,Jg(l)
  Ax=Forces(l)%Ax(i,j)
  Ay=Forces(l)%Ay(i,j)
  Az=Forces(l)%Az(i,j)
  dux=0.5*(Xp(i+1,j+1)+Xp(i+1,j)-Xp(i,j+1)-Xp(i,j))
  duy=0.5*(Yp(i+1,j+1)+Yp(i+1,j)-Yp(i,j+1)-Yp(i,j))
  duz=0.5*(Zp(i+1,j+1)+Zp(i+1,j)-Zp(i,j+1)-Zp(i,j))
  dvx=0.5*(Xp(i+1,j+1)+Xp(i,j+1)-Xp(i+1,j)-Xp(i,j))
  dvy=0.5*(Yp(i+1,j+1)+Yp(i,j+1)-Yp(i+1,j)-Yp(i,j))
  dvz=0.5*(Zp(i+1,j+1)+Zp(i,j+1)-Zp(i+1,j)-Zp(i,j))
  DE=dux**2+duy**2+duz**2
  DG=dvx**2+dvy**2+dvz**2
  DF=dux*dvx+duy*dvy+duz*dvz
  panel=sqrt(DE*DG-DF**2)
  !DF=0
  !panel=sqrt(DE*DG)
  dnx=(duy*dvz-duz*dvy)/panel
  dny=(duz*dvx-dux*dvz)/panel
  dnz=(dux*dvy-duy*dvx)/panel
  G1=rhow*DE**0.5*(gx*(dvy*dnz-dvz*dny)+gy*(dvz*dnx-dvx*dnz)+gz*(dvx*dny-dvy*dnx))/panel
  G2=rhow*DG**0.5*(gx*(duz*dny-duy*dnz)+gy*(dux*dnz-duz*dnx)+gz*(duy*dnx-dux*dny))/panel
  !G3=-rhow*(gx*dnx+gy*dny+gz*dnz)
  G3=0
  A1=DE**0.5*(Ax*(dvy*dnz-dvz*dny)+Ay*(dvz*dnx-dvx*dnz)+Az*(dvx*dny-dvy*dnx))/panel
  A2=DG**0.5*(Ax*(duz*dny-duy*dnz)+Ay*(dux*dnz-duz*dnx)+Az*(duy*dnx-dux*dny))/panel
  !G1=rhow*(gx*dux+gy*duy+gz*duz)/DE**0.5
  !G2=rhow*(gx*dvx+gy*dvy+gz*dvz)/DG**0.5
  !G3=-rhow*(gx*dnx+gy*dny+gz*dnz)
  !A1=(Ax*dux+Ay*duy+Az*duz)/DE**0.5
  !A2=(Ax*dvx+Ay*dvy+Az*dvz)/DG**0.5
  Grids(l)%DE(i,j)=DE
  Grids(l)%DG(i,j)=DG
  Grids(l)%DF(i,j)=DF
  Grids(l)%panel(i,j)=panel
  Forces(l)%G1(i,j)=G1
  Forces(l)%G2(i,j)=G2
  Forces(l)%G3(i,j)=G3
  Forces(l)%A1(i,j)=A1
  Forces(l)%A2(i,j)=A2
  end DO
end DO
DO j=1,Jg(l)
  DO i=2,Ig(l)
  Grids(l)%DE1(i,j)=0.5*(Grids(l)%DE(i,j)+Grids(l)%DE(i-1,j))
  Grids(l)%DG1(i,j)=0.5*(Grids(l)%DG(i,j)+Grids(l)%DG(i-1,j))
  Grids(l)%DF1(i,j)=0.5*(Grids(l)%DF(i,j)+Grids(l)%DF(i-1,j))
  end DO
end DO
DO j=2,Jg(l)
  DO i=1,Ig(l)
  Grids(l)%DE2(i,j)=0.5*(Grids(l)%DE(i,j)+Grids(l)%DE(i,j-1))
  Grids(l)%DG2(i,j)=0.5*(Grids(l)%DG(i,j)+Grids(l)%DG(i,j-1))
  Grids(l)%DF2(i,j)=0.5*(Grids(l)%DF(i,j)+Grids(l)%DF(i,j-1))
  end DO
end DO
print *,'block:',l
print *,'average alpha:',sum(180*acos(Grids(l)%DF/sqrt(Grids(l)%DE*Grids(l)%DG))/pi)/(Ig(l)*Jg(l))
print *,'alpha variance:',sqrt(sum((180*acos(Grids(l)%DF/sqrt(Grids(l)%DE*Grids(l)%DG))/pi-&
sum(180*acos(Grids(l)%DF/sqrt(Grids(l)%DE*Grids(l)%DG))/pi)/(Ig(l)*Jg(l)))**2)/(Ig(l)*Jg(l)))

end subroutine Forceparameter
