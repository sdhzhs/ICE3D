Subroutine Wavevelocity(l)
use COM
implicit none
integer i,j,l
real(8) DE1,DG2,Pc,Pw,Ps,hc,hw,hs,G1c,G1w,G2c,G2s,A1c,A1w,A2c,A2s,Qp1c,Qp1w,Qp2c,Qp2s
real(8) sax,say

  DO i=1,In(l)
    DO j=1,Jg(l)
    DE1=Grids(l)%DE1(i,j)
    if(i==In(l)) then
    Pc=Boundatas(l)%Pbcr(j)
    G1c=Boundatas(l)%G1bcr(j)
    A1c=Boundatas(l)%A1bcr(j)
    Qp1c=Boundatas(l)%Qp1bcr(j)
    hc=Boundatas(l)%hbcr(j)
    else
    Pc=Forces(l)%P(i,j)
    G1c=Forces(l)%G1(i,j)
    A1c=Forces(l)%A1(i,j)
    Qp1c=Fluxs(l)%Qp1(i,j)
    hc=Icecoordinates(l)%h(i,j)
    end if
    if(i==1) then
    Pw=Boundatas(l)%Pbcl(j)
    G1w=Boundatas(l)%G1bcl(j)
    A1w=Boundatas(l)%A1bcl(j)
    Qp1w=Boundatas(l)%Qp1bcl(j)
    hw=Boundatas(l)%hbcl(j)
    else
    Pw=Forces(l)%P(i-1,j)
    G1w=Forces(l)%G1(i-1,j)
    A1w=Forces(l)%A1(i-1,j)
    Qp1w=Fluxs(l)%Qp1(i-1,j)
    hw=Icecoordinates(l)%h(i-1,j)
    end if
    if(hc-hw/=0) then
    sax=(Qp1c-Qp1w)/(hc-hw)
    else
    sax=(-(Pc-Pw)/DE1**0.5+0.5*(G1c+G1w))*hc**2/muw+0.5*(A1c+A1w)*hc/muw
    end if
    Fluxs(l)%sax(i,j)=sax
    end DO
  end DO
  DO i=1,Ig(l)
    DO j=1,Jn(l)
    DG2=Grids(l)%DG2(i,j)
    if(j==Jn(l)) then
    Pc=Boundatas(l)%Pbcd(i)
    G2c=Boundatas(l)%G2bcd(i)
    A2c=Boundatas(l)%A2bcd(i)
    Qp2c=Boundatas(l)%Qp2bcd(i)
    hc=Boundatas(l)%hbcd(i)
    else
    Pc=Forces(l)%P(i,j)
    G2c=Forces(l)%G2(i,j)
    A2c=Forces(l)%A2(i,j)
    Qp2c=Fluxs(l)%Qp2(i,j)
    hc=Icecoordinates(l)%h(i,j)
    end if
    if(j==1) then
    Ps=Boundatas(l)%Pbcu(i)
    G2s=Boundatas(l)%G2bcu(i)
    A2s=Boundatas(l)%A2bcu(i)
    Qp2s=Boundatas(l)%Qp2bcu(i)
    hs=Boundatas(l)%hbcu(i)
    else
    Ps=Forces(l)%P(i,j-1)
    G2s=Forces(l)%G2(i,j-1)
    A2s=Forces(l)%A2(i,j-1)
    Qp2s=Fluxs(l)%Qp2(i,j-1)
    hs=Icecoordinates(l)%h(i,j-1)
    end if
    if(hc-hs/=0) then
    say=(Qp2c-Qp2s)/(hc-hs)
    else
    say=(-(Pc-Ps)/DG2**0.5+0.5*(G2c+G2s))*hc**2/muw+0.5*(A2c+A2s)*hc/muw
    end if
    Fluxs(l)%say(i,j)=say
    end DO
  end DO

end Subroutine Wavevelocity
