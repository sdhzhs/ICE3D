Subroutine Implicitcompute(l)
use COM
implicit none
integer i,j,l
real(8) omiga
real(8) dh,dhc,dhw,dhe,dhs,dhn,ahP,ahW,ahE,ahS,ahN,bh

omiga=1
   DO j=1,Jg(l)
     DO i=1,Ig(l)
     ahP=Imps(l)%ahP(i,j)
     ahW=Imps(l)%ahW(i,j)
     ahE=Imps(l)%ahE(i,j)
     ahS=Imps(l)%ahS(i,j)
     ahN=Imps(l)%ahN(i,j)
     bh=Imps(l)%bh(i,j)
     dhc=Imps(l)%dh(i,j)
     if(i==1) then
     dhw=Boundatas(l)%dhbcl(j)
     else
     dhw=Imps(l)%dh(i-1,j)
     end if
     if(i==Ig(l)) then
     dhe=Boundatas(l)%dhbcr(j)
     else
     dhe=Imps(l)%dh(i+1,j)
     end if
     if(j==1) then
     dhs=Boundatas(l)%dhbcu(i)
     else
     dhs=Imps(l)%dh(i,j-1)
     end if
     if(j==Jg(l)) then
     dhn=Boundatas(l)%dhbcd(i)
     else
     dhn=Imps(l)%dh(i,j+1)
     end if
     dh=omiga*((ahW*dhw+ahE*dhe+ahS*dhs+ahN*dhn+bh)/ahP)+(1-omiga)*dhc
     Imps(l)%dh(i,j)=dh
     end DO
   end DO

end Subroutine Implicitcompute
