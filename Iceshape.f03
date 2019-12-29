Subroutine Iceshape(l)
use COM
implicit none
integer i,j,l
real(8) Xuplus,Yuplus,Zuplus,Xuminus,Yuminus,Zuminus,Xvplus,Yvplus,Zvplus,Xvminus,Yvminus,Zvminus,ds,dnx,dny,dnz,dn
real(8) Xpc,Ypc,Zpc,Xpl,Ypl,Zpl,Xpr,Ypr,Zpr,Xpu,Ypu,Zpu,Xpd,Ypd,Zpd,hc,bc,hl,bl,hu,bu,blu,hlu
real(8) Xi,Yi,Zi,Xw,Yw,Zw,bn,hn

DO j=1,Jn(l)
  DO i=1,In(l)
  Xpc=Grids(l)%Xp(i,j)
  Ypc=Grids(l)%Yp(i,j)
  Zpc=Grids(l)%Zp(i,j)
  if(i==1) then
  Xpl=Boundatas(l)%Xpbcl(j)
  Ypl=Boundatas(l)%Ypbcl(j)
  Zpl=Boundatas(l)%Zpbcl(j)
  else
  Xpl=Grids(l)%Xp(i-1,j)
  Ypl=Grids(l)%Yp(i-1,j)
  Zpl=Grids(l)%Zp(i-1,j)
  end if
  if(i==In(l)) then
  Xpr=Boundatas(l)%Xpbcr(j)
  Ypr=Boundatas(l)%Ypbcr(j)
  Zpr=Boundatas(l)%Zpbcr(j)
  else
  Xpr=Grids(l)%Xp(i+1,j)
  Ypr=Grids(l)%Yp(i+1,j)
  Zpr=Grids(l)%Zp(i+1,j)
  end if
  if(j==1) then
  Xpu=Boundatas(l)%Xpbcu(i)
  Ypu=Boundatas(l)%Ypbcu(i)
  Zpu=Boundatas(l)%Zpbcu(i)
  else
  Xpu=Grids(l)%Xp(i,j-1)
  Ypu=Grids(l)%Yp(i,j-1)
  Zpu=Grids(l)%Zp(i,j-1)
  end if
  if(j==Jn(l)) then
  Xpd=Boundatas(l)%Xpbcd(i)
  Ypd=Boundatas(l)%Ypbcd(i)
  Zpd=Boundatas(l)%Zpbcd(i)
  else
  Xpd=Grids(l)%Xp(i,j+1)
  Ypd=Grids(l)%Yp(i,j+1)
  Zpd=Grids(l)%Zp(i,j+1)
  end if
  if(i==In(l).and.j==Jn(l)) then
  if(topos(l)%nbr(j-1)==0) then
  hc=Boundatas(l)%hbcd(i-1)
  bc=Boundatas(l)%bbcd(i-1)
  else if(topos(l)%nbd(i-1)==0) then
  hc=Boundatas(l)%hbcr(j-1)
  bc=Boundatas(l)%bbcr(j-1)
  else
  hc=(Boundatas(l)%hbcd(i-1)+Boundatas(l)%hbcr(j-1)+Icecoordinates(l)%h(i-1,j-1))/3
  bc=(Boundatas(l)%bbcd(i-1)+Boundatas(l)%bbcr(j-1)+Icecoordinates(l)%b(i-1,j-1))/3
  end if
  else if(i==In(l)) then
  hc=Boundatas(l)%hbcr(j)
  bc=Boundatas(l)%bbcr(j)
  else if(j==Jn(l)) then
  hc=Boundatas(l)%hbcd(i)
  bc=Boundatas(l)%bbcd(i)
  else
  hc=Icecoordinates(l)%h(i,j)
  bc=Icecoordinates(l)%b(i,j)
  end if
  if(i==1.and.j==Jn(l)) then
  if(topos(l)%nbl(j-1)==0) then
  hl=Boundatas(l)%hbcd(i)
  bl=Boundatas(l)%bbcd(i)
  else if(topos(l)%nbd(i)==0) then
  hl=Boundatas(l)%hbcl(j-1)
  bl=Boundatas(l)%bbcl(j-1)
  else
  hl=(Boundatas(l)%hbcd(i)+Boundatas(l)%hbcl(j-1)+Icecoordinates(l)%h(i,j-1))/3
  bl=(Boundatas(l)%bbcd(i)+Boundatas(l)%bbcl(j-1)+Icecoordinates(l)%b(i,j-1))/3
  end if
  else if(i==1) then
  hl=Boundatas(l)%hbcl(j)
  bl=Boundatas(l)%bbcl(j)
  else if(j==Jn(l)) then
  hl=Boundatas(l)%hbcd(i-1)
  bl=Boundatas(l)%bbcd(i-1)
  else
  hl=Icecoordinates(l)%h(i-1,j)
  bl=Icecoordinates(l)%b(i-1,j)
  end if
  if(i==In(l).and.j==1) then
  if(topos(l)%nbr(j)==0) then
  hu=Boundatas(l)%hbcu(i-1)
  bu=Boundatas(l)%bbcu(i-1)
  else if(topos(l)%nbu(i-1)==0) then
  hu=Boundatas(l)%hbcr(j)
  bu=Boundatas(l)%bbcr(j)
  else
  hu=(Boundatas(l)%hbcu(i-1)+Boundatas(l)%hbcr(j)+Icecoordinates(l)%h(i-1,j))/3
  bu=(Boundatas(l)%bbcu(i-1)+Boundatas(l)%bbcr(j)+Icecoordinates(l)%b(i-1,j))/3
  end if
  else if(i==In(l)) then
  hu=Boundatas(l)%hbcr(j-1)
  bu=Boundatas(l)%bbcr(j-1)
  else if(j==1) then
  hu=Boundatas(l)%hbcu(i)
  bu=Boundatas(l)%bbcu(i)
  else
  hu=Icecoordinates(l)%h(i,j-1)
  bu=Icecoordinates(l)%b(i,j-1)
  end if
  if(i==1.and.j==1) then
  if(topos(l)%nbl(j)==0) then
  hlu=Boundatas(l)%hbcu(i)
  blu=Boundatas(l)%bbcu(i)
  else if(topos(l)%nbu(i)==0) then
  hlu=Boundatas(l)%hbcl(j)
  blu=Boundatas(l)%bbcl(j)
  else
  hlu=(Boundatas(l)%hbcu(i)+Boundatas(l)%hbcl(j)+Icecoordinates(l)%h(i,j))/3
  blu=(Boundatas(l)%bbcu(i)+Boundatas(l)%bbcl(j)+Icecoordinates(l)%b(i,j))/3
  end if
  else if(i==1) then
  hlu=Boundatas(l)%hbcl(j-1)
  blu=Boundatas(l)%bbcl(j-1)
  else if(j==1) then
  hlu=Boundatas(l)%hbcu(i-1)
  blu=Boundatas(l)%bbcu(i-1)
  else
  hlu=Icecoordinates(l)%h(i-1,j-1)
  blu=Icecoordinates(l)%b(i-1,j-1)
  end if
  hn=0.25*(hc+hl+hu+hlu)
  bn=0.25*(bc+bl+bu+blu)
  Xuplus=Xpr-Xpc
  Yuplus=Ypr-Ypc
  Zuplus=Zpr-Zpc
  ds=sqrt(Xuplus**2+Yuplus**2+Zuplus**2)
  Xuplus=Xuplus/ds
  Yuplus=Yuplus/ds
  Zuplus=Zuplus/ds
  Xuminus=Xpl-Xpc
  Yuminus=Ypl-Ypc
  Zuminus=Zpl-Zpc
  ds=sqrt(Xuminus**2+Yuminus**2+Zuminus**2)
  Xuminus=Xuminus/ds
  Yuminus=Yuminus/ds
  Zuminus=Zuminus/ds
  Xvplus=Xpd-Xpc
  Yvplus=Ypd-Ypc
  Zvplus=Zpd-Zpc
  ds=sqrt(Xvplus**2+Yvplus**2+Zvplus**2)
  Xvplus=Xvplus/ds
  Yvplus=Yvplus/ds
  Zvplus=Zvplus/ds
  Xvminus=Xpu-Xpc
  Yvminus=Ypu-Ypc
  Zvminus=Zpu-Zpc
  ds=sqrt(Xvminus**2+Yvminus**2+Zvminus**2)
  Xvminus=Xvminus/ds
  Yvminus=Yvminus/ds
  Zvminus=Zvminus/ds
  dnx=Yuplus*Zvplus-Zuplus*Yvplus+Yuminus*Zvminus-Zuminus*Yvminus+Yvplus*Zuminus-Zvplus*Yuminus+Yvminus*Zuplus-&
  Zvminus*Yuplus
  dny=Zuplus*Xvplus-Xuplus*Zvplus+Zuminus*Xvminus-Xuminus*Zvminus+Zvplus*Xuminus-Xvplus*Zuminus+Zvminus*Xuplus-&
  Xvminus*Zuplus
  dnz=Xuplus*Yvplus-Yuplus*Xvplus+Xuminus*Yvminus-Yuminus*Xvminus+Xvplus*Yuminus-Yvplus*Xuminus+Xvminus*Yuplus-&
  Yvminus*Xuplus
  dn=sqrt(dnx**2+dny**2+dnz**2)
  Xi=Xpc+bn*dnx/dn
  Yi=Ypc+bn*dny/dn
  Zi=Zpc+bn*dnz/dn
  Xw=Xpc+(bn+hn)*dnx/dn
  Yw=Ypc+(bn+hn)*dny/dn
  Zw=Zpc+(bn+hn)*dnz/dn
  Icecoordinates(l)%Xi(i,j)=Xi
  Icecoordinates(l)%Yi(i,j)=Yi
  Icecoordinates(l)%Zi(i,j)=Zi
  Icecoordinates(l)%Xw(i,j)=Xw
  Icecoordinates(l)%Yw(i,j)=Yw
  Icecoordinates(l)%Zw(i,j)=Zw
  Icecoordinates(l)%hn(i,j)=hn
  Icecoordinates(l)%bn(i,j)=bn
  end DO
end DO

end subroutine Iceshape
