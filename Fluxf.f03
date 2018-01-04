Subroutine Fluxf(l)
use COM
implicit none
integer i,j,l,s1,s2
real(8) DE(Ig(l),Jg(l)),DG(Ig(l),Jg(l)),DF(Ig(l),Jg(l)),DE1(In(l),Jg(l)),DE2(Ig(l),Jn(l)),DG1(In(l),Jg(l)),DG2(Ig(l),Jn(l)),&
DF1(In(l),Jg(l)),DF2(Ig(l),Jn(l)),panel(Ig(l),Jg(l)),beta(Ig(l),Jg(l)),qe0(Ig(l),Jg(l)),qe1(Ig(l),Jg(l)),Qp1(Ig(l),Jg(l)),&
Qp2(Ig(l),Jg(l)),dQp1(Ig(l),Jg(l)),dQp2(Ig(l),Jg(l)),sax(In(l),Jg(l)),say(Ig(l),Jn(l)),h(Ig(l),Jg(l)),b(Ig(l),Jg(l))
real(8) hbcl(Jg(l)),hbcll(Jg(l)),hbcr(Jg(l)),hbcrr(Jg(l)),hbcu(Ig(l)),hbcuu(Ig(l)),hbcd(Ig(l)),hbcdd(Ig(l)),Qp1bcl(Jg(l)),&
Qp1bcr(Jg(l)),Qp2bcu(Ig(l)),Qp2bcd(Ig(l)),dQp1bcl(Jg(l)),dQp1bcr(Jg(l)),dQp2bcu(Ig(l)),dQp2bcd(Ig(l)),dul(Jg(l)),dur(Jg(l)),&
dvu(Ig(l)),dvd(Ig(l)),saxl(Jg(l)),saxr(Jg(l)),sayu(Ig(l)),sayd(Ig(l))
real(8) lamda1(In(l),Jg(l)),lamda2(Ig(l),Jn(l)),Qup1(In(l),Jg(l)),Qup2(Ig(l),Jn(l)),Qlw1(In(l),Jg(l)),Qlw2(Ig(l),Jn(l)),&
r1(In(l),Jg(l)),r2(Ig(l),Jn(l)),fai1(In(l),Jg(l)),fai2(Ig(l),Jn(l))
real(8) Cp1(In(l),Jg(l)),Cp2(Ig(l),Jn(l)),Cn1(In(l),Jg(l)),Cn2(Ig(l),Jn(l)),Dp1(In(l),Jg(l)),Dp2(Ig(l),Jn(l)),Dn1(In(l),Jg(l)),&
Dn2(Ig(l),Jn(l))
real(8) Q1(In(l),Jg(l)),Q2(Ig(l),Jn(l)),ahP(Ig(l),Jg(l)),ahW(Ig(l),Jg(l)),ahE(Ig(l),Jg(l)),ahN(Ig(l),Jg(l)),ahS(Ig(l),Jg(l)),&
bh(Ig(l),Jg(l))
DE=Grids(l)%DE
DG=Grids(l)%DG
DF=Grids(l)%DF
DE1=Grids(l)%DE1
DG1=Grids(l)%DG1
DF1=Grids(l)%DF1
DE2=Grids(l)%DE2
DG2=Grids(l)%DG2
DF2=Grids(l)%DF2
panel=Grids(l)%panel
beta=Forces(l)%beta
qe0=Energys(l)%qe0
qe1=Energys(l)%qe1
Qp1=Fluxs(l)%Qp1
Qp2=Fluxs(l)%Qp2
dQp1=Fluxs(l)%dQp1
dQp2=Fluxs(l)%dQp2
sax=Fluxs(l)%sax
say=Fluxs(l)%say
h=Icecoordinates(l)%h
b=Icecoordinates(l)%b
hbcl=Boundatas(l)%hbcl
hbcr=Boundatas(l)%hbcr
hbcu=Boundatas(l)%hbcu
hbcd=Boundatas(l)%hbcd
hbcll=Boundatas(l)%hbcll
hbcrr=Boundatas(l)%hbcrr
hbcuu=Boundatas(l)%hbcuu
hbcdd=Boundatas(l)%hbcdd
Qp1bcl=Boundatas(l)%Qp1bcl
Qp1bcr=Boundatas(l)%Qp1bcr
Qp2bcu=Boundatas(l)%Qp2bcu
Qp2bcd=Boundatas(l)%Qp2bcd
dQp1bcl=Boundatas(l)%dQp1bcl
dQp1bcr=Boundatas(l)%dQp1bcr
dQp2bcu=Boundatas(l)%dQp2bcu
dQp2bcd=Boundatas(l)%dQp2bcd
dul=Boundatas(l)%dul
dur=Boundatas(l)%dur
dvu=Boundatas(l)%dvu
dvd=Boundatas(l)%dvd
saxl=Boundatas(l)%saxl
saxr=Boundatas(l)%saxr
sayu=Boundatas(l)%sayu
sayd=Boundatas(l)%sayd
lamda1=dt/DE1**0.5
lamda2=dt/DG2**0.5
  DO i=1,In(l)
    DO j=1,Jg(l)
    if(i==1) then
    Qup1(i,j)=0.5*(Qp1(i,j)+Qp1bcl(j))-0.5*sgn(sax(i,j))*(Qp1(i,j)-Qp1bcl(j))
    Qlw1(i,j)=0.5*(Qp1(i,j)+Qp1bcl(j))-0.5*dt*sax(i,j)**2*(h(i,j)-hbcl(j))/DE1(i,j)**0.5
    else if(i==In(l)) then
    Qup1(i,j)=0.5*(Qp1(i-1,j)+Qp1bcr(j))-0.5*sgn(sax(i,j))*(Qp1bcr(j)-Qp1(i-1,j))
    Qlw1(i,j)=0.5*(Qp1(i-1,j)+Qp1bcr(j))-0.5*dt*sax(i,j)**2*(hbcr(j)-h(i-1,j))/DE1(i,j)**0.5
    else
    Qup1(i,j)=0.5*(Qp1(i,j)+Qp1(i-1,j))-0.5*sgn(sax(i,j))*(Qp1(i,j)-Qp1(i-1,j))
    Qlw1(i,j)=0.5*(Qp1(i,j)+Qp1(i-1,j))-0.5*dt*sax(i,j)**2*(h(i,j)-h(i-1,j))/DE1(i,j)**0.5
    end if
    end DO
  end DO
  DO i=1,Ig(l)
    DO j=1,Jn(l)
    if(j==1) then
    Qup2(i,j)=0.5*(Qp2(i,j)+Qp2bcu(i))-0.5*sgn(say(i,j))*(Qp2(i,j)-Qp2bcu(i))
    Qlw2(i,j)=0.5*(Qp2(i,j)+Qp2bcu(i))-0.5*dt*say(i,j)**2*(h(i,j)-hbcu(i))/DG2(i,j)**0.5
    else if(j==Jn(l)) then
    Qup2(i,j)=0.5*(Qp2(i,j-1)+Qp2bcd(i))-0.5*sgn(say(i,j))*(Qp2bcd(i)-Qp2(i,j-1))
    Qlw2(i,j)=0.5*(Qp2(i,j-1)+Qp2bcd(i))-0.5*dt*say(i,j)**2*(hbcd(i)-h(i,j-1))/DG2(i,j)**0.5
    else
    Qup2(i,j)=0.5*(Qp2(i,j)+Qp2(i,j-1))-0.5*sgn(say(i,j))*(Qp2(i,j)-Qp2(i,j-1))
    Qlw2(i,j)=0.5*(Qp2(i,j)+Qp2(i,j-1))-0.5*dt*say(i,j)**2*(h(i,j)-h(i,j-1))/DG2(i,j)**0.5
    end if
    end DO
  end DO
  DO i=1,In(l)
    DO j=1,Jg(l)
    s1=sgn(sax(i,j))
    if(i==1) then
    if(s1==0.or.abs(h(i,j)-hbcl(j))<1e-30) then
    r1(i,j)=1
    else if(s1<0) then
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(h(i-s1,j)-h(i-1-s1,j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(h(i,j)-hbcl(j)))
    else
    r1(i,j)=(abs(saxl(j))-dt*saxl(j)**2/dul(j)**0.5)*(hbcl(j)-hbcll(j))/((abs(sax(i,j))-lamda1(i,j)*sax(i,j)**2)*(h(i,j)-hbcl(j)))
    end if
    else if(i==2) then
    if(s1==0.or.abs(h(i,j)-h(i-1,j))<1e-30) then
    r1(i,j)=1
    else if(s1>0) then
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(h(i-s1,j)-hbcl(j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(h(i,j)-h(i-1,j)))
    else
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(h(i-s1,j)-h(i-1-s1,j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(h(i,j)-h(i-1,j)))
    end if
    else if(i==Ig(l)) then
    if(s1==0.or.abs(h(i,j)-h(i-1,j))<1e-30) then
    r1(i,j)=1
    else if(s1<0) then
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(hbcr(j)-h(i-1-s1,j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(h(i,j)-h(i-1,j)))
    else
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(h(i-s1,j)-h(i-1-s1,j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(h(i,j)-h(i-1,j)))
    end if
    else if(i==In(l)) then
    if(s1==0.or.abs(hbcr(j)-h(i-1,j))<1e-30) then
    r1(i,j)=1
    else if(s1>0) then
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(h(i-s1,j)-h(i-1-s1,j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(hbcr(j)-h(i-1,j)))
    else
    r1(i,j)=(abs(saxr(j))-dt*saxr(j)**2/dur(j)**0.5)*(hbcrr(j)-hbcr(j))/((abs(sax(i,j))-lamda1(i,j)*sax(i,j)**2)*(hbcr(j)-h(i-1,j)))
    end if
    else
    if(s1==0.or.abs(h(i,j)-h(i-1,j))<1e-30) then
    r1(i,j)=1
    else
    r1(i,j)=(abs(sax(i-s1,j))-lamda1(i-s1,j)*sax(i-s1,j)**2)*(h(i-s1,j)-h(i-1-s1,j))/((abs(sax(i,j))-&
    lamda1(i,j)*sax(i,j)**2)*(h(i,j)-h(i-1,j)))
    end if
    end if
    end DO
  end DO
  DO i=1,Ig(l)
    DO j=1,Jn(l)
    s2=sgn(say(i,j))
    if(j==1) then
    if(s2==0.or.abs(h(i,j)-hbcu(i))<1e-30) then
    r2(i,j)=1
    else if(s2>0) then
    r2(i,j)=(abs(sayu(i))-dt*sayu(i)**2/dvu(i)**0.5)*(hbcu(i)-hbcuu(i))/((abs(say(i,j))-lamda2(i,j)*say(i,j)**2)*(h(i,j)-hbcu(i)))
    else
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(h(i,j-s2)-h(i,j-1-s2))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(h(i,j)-hbcu(i)))
    end if
    else if(j==2) then
    if(s2==0.or.abs(h(i,j)-h(i,j-1))<1e-30) then
    r2(i,j)=1
    else if(s2>0) then
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(h(i,j-s2)-hbcu(i))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(h(i,j)-h(i,j-1)))
    else
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(h(i,j-s2)-h(i,j-1-s2))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(h(i,j)-h(i,j-1)))
    end if
    else if(j==Jg(l)) then
    if(s2==0.or.abs(h(i,j)-h(i,j-1))<1e-30) then
    r2(i,j)=1
    else if(s2<0) then
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(hbcd(i)-h(i,j-1-s2))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(h(i,j)-h(i,j-1)))
    else
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(h(i,j-s2)-h(i,j-1-s2))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(h(i,j)-h(i,j-1)))
    end if
    else if(j==Jn(l)) then
    if(s2==0.or.abs(hbcd(i)-h(i,j-1))<1e-30) then
    r2(i,j)=1
    else if(s2<0) then
    r2(i,j)=(abs(sayd(i))-dt*sayd(i)**2/dvd(i)**0.5)*(hbcdd(i)-hbcd(i))/((abs(say(i,j))-lamda2(i,j)*say(i,j)**2)*(hbcd(i)-h(i,j-1)))
    else
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(h(i,j-s2)-h(i,j-1-s2))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(hbcd(i)-h(i,j-1)))
    end if
    else
    if(s2==0.or.abs(h(i,j)-h(i,j-1))<1e-30) then
    r2(i,j)=1
    else
    r2(i,j)=(abs(say(i,j-s2))-lamda2(i,j-s2)*say(i,j-s2)**2)*(h(i,j-s2)-h(i,j-1-s2))/((abs(say(i,j))-&
    lamda2(i,j)*say(i,j)**2)*(h(i,j)-h(i,j-1)))
    end if
    end if
    end DO
  end DO
  DO i=1,In(l)
    DO j=1,Jg(l)
    fai1(i,j)=max(0.0,min(2*r1(i,j),1.0),min(r1(i,j),2.0))
    !fai1(i,j)=max(0.0,min(r1(i,j),1.0))
    if(discretecontrol=='TVD') then
    Q1(i,j)=(1-fai1(i,j))*Qup1(i,j)+fai1(i,j)*Qlw1(i,j)
    else if(discretecontrol=='upwind') then
    Q1(i,j)=Qup1(i,j)
    end if
    end DO
  end DO
  DO i=1,Ig(l)
    DO j=1,Jn(l)
    fai2(i,j)=max(0.0,min(2*r2(i,j),1.0),min(r2(i,j),2.0))
    !fai2(i,j)=max(0.0,min(r2(i,j),1.0))
    if(discretecontrol=='TVD') then
    Q2(i,j)=(1-fai2(i,j))*Qup2(i,j)+fai2(i,j)*Qlw2(i,j)
    else if(discretecontrol=='upwind') then
    Q2(i,j)=Qup2(i,j)
    end if
    end DO
  end DO
if(solutioncontrol=='implicit') then
  DO i=1,In(l)
    DO j=1,Jg(l)
    if(i==1) then
    Cp1(i,j)=dQp1bcl(j)/2+sgn(sax(i,j))*dQp1bcl(j)/2
    Dp1(i,j)=dQp1bcl(j)/2+lamda1(i,j)*sax(i,j)**2/2
    Cn1(i,j)=dQp1(i,j)/2-sgn(sax(i,j))*dQp1(i,j)/2
    Dn1(i,j)=dQp1(i,j)/2-lamda1(i,j)*sax(i,j)**2/2
    else if(i==In(l)) then
    Cp1(i,j)=dQp1(i-1,j)/2+sgn(sax(i,j))*dQp1(i-1,j)/2
    Dp1(i,j)=dQp1(i-1,j)/2+lamda1(i,j)*sax(i,j)**2/2
    Cn1(i,j)=dQp1bcr(j)/2-sgn(sax(i,j))*dQp1bcr(j)/2
    Dn1(i,j)=dQp1bcr(j)/2-lamda1(i,j)*sax(i,j)**2/2
    else
    Cp1(i,j)=dQp1(i-1,j)/2+sgn(sax(i,j))*dQp1(i-1,j)/2
    Dp1(i,j)=dQp1(i-1,j)/2+lamda1(i,j)*sax(i,j)**2/2
    Cn1(i,j)=dQp1(i,j)/2-sgn(sax(i,j))*dQp1(i,j)/2
    Dn1(i,j)=dQp1(i,j)/2-lamda1(i,j)*sax(i,j)**2/2
    end if
    end DO
  end DO
  DO i=1,Ig(l)
    DO j=1,Jn(l)
    if(j==1) then
    Cp2(i,j)=dQp2bcu(i)/2+sgn(say(i,j))*dQp2bcu(i)/2
    Dp2(i,j)=dQp2bcu(i)/2+lamda2(i,j)*say(i,j)**2/2
    Cn2(i,j)=dQp2(i,j)/2-sgn(say(i,j))*dQp2(i,j)/2
    Dn2(i,j)=dQp2(i,j)/2-lamda2(i,j)*say(i,j)**2/2
    else if(j==Jn(l)) then
    Cp2(i,j)=dQp2(i,j-1)/2+sgn(say(i,j))*dQp2(i,j-1)/2
    Dp2(i,j)=dQp2(i,j-1)/2+lamda2(i,j)*say(i,j)**2/2
    Cn2(i,j)=dQp2bcd(i)/2-sgn(say(i,j))*dQp2bcd(i)/2
    Dn2(i,j)=dQp2bcd(i)/2-lamda2(i,j)*say(i,j)**2/2
    else
    Cp2(i,j)=dQp2(i,j-1)/2+sgn(say(i,j))*dQp2(i,j-1)/2
    Dp2(i,j)=dQp2(i,j-1)/2+lamda2(i,j)*say(i,j)**2/2
    Cn2(i,j)=dQp2(i,j)/2-sgn(say(i,j))*dQp2(i,j)/2
    Dn2(i,j)=dQp2(i,j)/2-lamda2(i,j)*say(i,j)**2/2
    end if
    end DO
  end DO
  DO i=1,Ig(l)
    DO j=1,Jg(l)
    if(discretecontrol=='TVD') then
    ahP(i,j)=dt*((sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*(1-fai1(i+1,j))*Cp1(i+1,j)-&
    sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*(1-fai1(i,j))*Cn1(i,j)+sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*fai1(i+1,j)*Dp1(i+1,j)-&
    sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*fai1(i,j)*Dn1(i,j))/panel(i,j)+&
    (sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*(1-fai2(i,j+1))*Cp2(i,j+1)-&
    sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*(1-fai2(i,j))*Cn2(i,j)+sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*fai2(i,j+1)*Dp2(i,j+1)-&
    sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*fai2(i,j)*Dn2(i,j))/panel(i,j))+1
    ahE(i,j)=-dt*(sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*(1-fai1(i+1,j))*Cn1(i+1,j)+&
    sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*fai1(i+1,j)*Dn1(i+1,j))/panel(i,j)
    ahW(i,j)=dt*(sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*(1-fai1(i,j))*Cp1(i,j)+&
    sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*fai1(i,j)*Dp1(i,j))/panel(i,j)
    ahN(i,j)=-dt*(sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*(1-fai2(i,j+1))*Cn2(i,j+1)+&
    sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*fai2(i,j+1)*Dn2(i,j+1))/panel(i,j)
    ahS(i,j)=dt*(sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*(1-fai2(i,j))*Cp2(i,j)+&
    sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*fai2(i,j)*Dp2(i,j))/panel(i,j)
    if(i==1.and.topos%nbl(l)==0) then
    !if(i==1.and.topos(l)%nbl(j)==0) then
    ahW(i,j)=0
    ahP(i,j)=ahP(i,j)+dt*(sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*(1-fai1(i,j))*Cn1(i,j)+&
    sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*fai1(i,j)*Dn1(i,j))/panel(i,j)
    else if(i==Ig(l).and.topos%nbr(l)==0) then
    !else if(i==Ig(l).and.topos(l)%nbr(j)==0) then
    ahE(i,j)=0
    ahP(i,j)=ahP(i,j)-dt*(sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*(1-fai1(i+1,j))*Cp1(i+1,j)+&
    sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*fai1(i+1,j)*Dp1(i+1,j))/panel(i,j)
    else if(j==1.and.topos%nbu(l)==0) then
    !else if(j==1.and.topos(l)%nbu(i)==0) then
    ahS(i,j)=0
    ahP(i,j)=ahP(i,j)+dt*(sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*(1-fai2(i,j))*Cn2(i,j)+&
    sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*fai2(i,j)*Dn2(i,j))/panel(i,j)
    else if(j==Jg(l).and.topos%nbd(l)==0) then
    !else if(j==Jg(l).and.topos(l)%nbd(i)==0) then
    ahN(i,j)=0
    ahP(i,j)=ahP(i,j)-dt*(sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*(1-fai2(i,j+1))*Cp2(i,j+1)+&
    sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*fai2(i,j+1)*Dp2(i,j+1))/panel(i,j)
    end if
    else if(discretecontrol=='upwind') then
    ahP(i,j)=dt*((sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*Cp1(i+1,j)-sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*Cn1(i,j))/panel(i,j)+&
    (sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*Cp2(i,j+1)-sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*Cn2(i,j))/panel(i,j))+1
    ahE(i,j)=-dt*sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*Cn1(i+1,j)/panel(i,j)
    ahW(i,j)=dt*sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*Cp1(i,j)/panel(i,j)
    ahN(i,j)=-dt*sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*Cn2(i,j+1)/panel(i,j)
    ahS(i,j)=dt*sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*Cp2(i,j)/panel(i,j)
    if(i==1.and.topos%nbl(l)==0) then
    !if(i==1.and.topos(l)%nbl(j)==0) then
    ahW(i,j)=0
    ahP(i,j)=ahP(i,j)+dt*sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*Cn1(i,j)/panel(i,j)
    else if(i==Ig(l).and.topos%nbr(l)==0) then
    !else if(i==Ig(l).and.topos(l)%nbr(j)==0) then
    ahE(i,j)=0
    ahP(i,j)=ahP(i,j)-dt*sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*Cp1(i+1,j)/panel(i,j)
    else if(j==1.and.topos%nbu(l)==0) then
    !else if(j==1.and.topos(l)%nbu(i)==0) then
    ahS(i,j)=0
    ahP(i,j)=ahP(i,j)+dt*sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*Cn2(i,j)/panel(i,j)
    else if(j==Jg(l).and.topos%nbd(l)==0) then
    !else if(j==Jg(l).and.topos(l)%nbd(i)==0) then
    ahN(i,j)=0
    ahP(i,j)=ahP(i,j)-dt*sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*Cp2(i,j+1)/panel(i,j)
    end if
    end if
    if(icecoupled=='Y') then
    bh(i,j)=-dt*(sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*Q1(i+1,j)-sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*Q1(i,j)+&
    sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*Q2(i,j+1)-sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*Q2(i,j))/panel(i,j)+&
    lwc*beta(i,j)*Wf*dt/rouw-(alpha*ki*(Tf-Ts)/b(i,j)-kw*(qe0(i,j)+qe1(i,j)*Tf))*dt/(rouw*Lf)
    else if(icecoupled=='N') then
    bh(i,j)=-dt*(sqrt(DG1(i+1,j)-DF1(i+1,j)**2/DE1(i+1,j))*Q1(i+1,j)-sqrt(DG1(i,j)-DF1(i,j)**2/DE1(i,j))*Q1(i,j)+&
    sqrt(DE2(i,j+1)-DF2(i,j+1)**2/DG2(i,j+1))*Q2(i,j+1)-sqrt(DE2(i,j)-DF2(i,j)**2/DG2(i,j))*Q2(i,j))/panel(i,j)+&
    lwc*beta(i,j)*Wf*dt/rouw
    end if
    end DO
  end DO
end if
Fluxs(l)%Q1=Q1
Fluxs(l)%Q2=Q2
Imps(l)%ahP=ahP
Imps(l)%ahW=ahW
Imps(l)%ahE=ahE
Imps(l)%ahS=ahS
Imps(l)%ahN=ahN
Imps(l)%bh=bh
end subroutine Fluxf
