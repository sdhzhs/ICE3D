Subroutine Impliciticecompute(l)
use COM
implicit none
integer i,j,l
real(8) panel,DEw,DEe,DEs,DEn,DGw,DGe,DGs,DGn,DFw,DFe,DFs,DFn,beta,qe0,qe1,Qw,Qe,Qs,Qn,h,b

  DO j=1,Jg(l)
    DO i=1,Ig(l)
    panel=Grids(l)%panel(i,j)
    beta=Forces(l)%beta(i,j)
    qe0=Energys(l)%qe0(i,j)
    qe1=Energys(l)%qe1(i,j)
    h=Icecoordinates(l)%h(i,j)
    b=Icecoordinates(l)%b(i,j)
    DEw=Grids(l)%DE1(i,j)
    DEe=Grids(l)%DE1(i+1,j)
    DEs=Grids(l)%DE2(i,j)
    DEn=Grids(l)%DE2(i,j+1)
    DGw=Grids(l)%DG1(i,j)
    DGe=Grids(l)%DG1(i+1,j)
    DGs=Grids(l)%DG2(i,j)
    DGn=Grids(l)%DG2(i,j+1)
    DFw=Grids(l)%DF1(i,j)
    DFe=Grids(l)%DF1(i+1,j)
    DFs=Grids(l)%DF2(i,j)
    DFn=Grids(l)%DF2(i,j+1)
    Qw=Fluxs(l)%Q1(i,j)
    Qe=Fluxs(l)%Q1(i+1,j)
    Qs=Fluxs(l)%Q2(i,j)
    Qn=Fluxs(l)%Q2(i,j+1)
    if(h>1e-4) then
    h=1e-4
    b=b+(alpha*ki*(Tf-Ts)/b-kw*(qe0+qe1*Tf))*dt/(roui*Lf)
    else if(h>hp) then
    b=b+(alpha*ki*(Tf-Ts)/b-kw*(qe0+qe1*Tf))*dt/(roui*Lf)
    else
    h=hp
    b=b+lwc*beta*Wf*dt/roui-rouw*(sqrt(DGe-DFe**2/DEe)*Qe-sqrt(DGw-DFw**2/DEw)*Qw+sqrt(DEn-DFn**2/DGn)*Qn-&
    sqrt(DEs-DFs**2/DGs)*Qs)*dt/panel/roui
    end if
    Icecoordinates(l)%h(i,j)=h
    Icecoordinates(l)%b(i,j)=b
    end DO
  end DO

end Subroutine Impliciticecompute
