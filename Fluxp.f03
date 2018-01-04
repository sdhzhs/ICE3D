Subroutine Fluxp(l)
use COM
implicit none
integer i,j,l
real(8) DE,DG,DF,G1,G2,G3,A1,A2
real(8) Pc,Pw,Pe,Ps,Pn,hc,hw,he,hs,hn
real(8) Qp1,Qp2,dQp1,dQp2

  DO i=1,Ig(l)
    DO j=1,Jg(l)
    DE=Grids(l)%DE(i,j)
    DG=Grids(l)%DG(i,j)
    DF=Grids(l)%DF(i,j)
    G1=Forces(l)%G1(i,j)
    G2=Forces(l)%G2(i,j)
    G3=Forces(l)%G3(i,j)
    A1=Forces(l)%A1(i,j)
    A2=Forces(l)%A2(i,j)
    Pc=Forces(l)%P(i,j)
    hc=Icecoordinates(l)%h(i,j)
    if(i==1) then
    Pw=Boundatas(l)%Pbcl(j)
    hw=Boundatas(l)%hbcl(j)
    else
    Pw=Forces(l)%P(i-1,j)
    hw=Icecoordinates(l)%h(i-1,j)
    end if
    if(i==Ig(l)) then
    Pe=Boundatas(l)%Pbcr(j)
    he=Boundatas(l)%hbcr(j)
    else
    Pe=Forces(l)%P(i+1,j)
    he=Icecoordinates(l)%h(i+1,j)
    end if
    if(j==1) then
    Ps=Boundatas(l)%Pbcu(i)
    hs=Boundatas(l)%hbcu(i)
    else
    Ps=Forces(l)%P(i,j-1)
    hs=Icecoordinates(l)%h(i,j-1)
    end if
    if(j==Jg(l)) then
    Pn=Boundatas(l)%Pbcd(i)
    hn=Boundatas(l)%hbcd(i)
    else
    Pn=Forces(l)%P(i,j+1)
    hn=Icecoordinates(l)%h(i,j+1)
    end if
    Qp1=hc**3*(-DE**0.5*(DG*((Pe-Pw)/2+G3*(he-hw)/2)-DF*((Pn-Ps)/2+G3*(hn-hs)/2))/(DE*DG-DF**2)+G1)/(3*miuw)+A1*hc**2/(2*miuw)
    Qp2=hc**3*(-DG**0.5*(DE*((Pn-Ps)/2+G3*(hn-hs)/2)-DF*((Pe-Pw)/2+G3*(he-hw)/2))/(DE*DG-DF**2)+G2)/(3*miuw)+A2*hc**2/(2*miuw)
    Fluxs(l)%Qp1(i,j)=Qp1
    Fluxs(l)%Qp2(i,j)=Qp2
    if(solutioncontrol=='implicit') then
    dQp1=hc**2*(-DE**0.5*(DG*((Pe-Pw)/2+G3*(he-hw)/2)-DF*((Pn-Ps)/2+G3*(hn-hs)/2))/(DE*DG-DF**2)+G1)/miuw+A1*hc/miuw
    dQp2=hc**2*(-DG**0.5*(DE*((Pn-Ps)/2+G3*(hn-hs)/2)-DF*((Pe-Pw)/2+G3*(he-hw)/2))/(DE*DG-DF**2)+G2)/miuw+A2*hc/miuw
    Fluxs(l)%dQp1(i,j)=dQp1
    Fluxs(l)%dQp2(i,j)=dQp2
    end if
    end DO
  end DO

end Subroutine Fluxp
