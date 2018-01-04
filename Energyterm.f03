Subroutine Energyterm(l)
use COM
implicit none
integer i,j,l,s
real(8) qr,ks,dQe,dqe0,dQs,dqr0,Rek,recov
real(8) P,beta,dropv,Uae,hcv,h,b
real(8) qe0,qe1,qr0,qr1,Tb,Tb0,mev,msv
real(8) Qk,Qa,Qe,Qs,Ql,qd,qc
if(mvd<=2e-5) then
ks=0.6839*(0.5714+0.2457*(lwc*1000)+1.2571*(lwc*1000)**2)*(0.047*Ta-11.27)*0.001177*c
else
ks=0.6839*(0.5714+0.2457*(lwc*1000)+1.2571*(lwc*1000)**2)*(1.667-0.0333*(mvd*10**6))*(0.047*Ta-11.27)*0.001177*c
end if
DO j=1,Jg(l)
DO i=1,Ig(l)
P=Forces(l)%P(i,j)
beta=Forces(l)%beta(i,j)
dropv=Forces(l)%dropv(i,j)
Uae=Energys(l)%Uae(i,j)
hcv=Energys(l)%hcv(i,j)
h=Icecoordinates(l)%h(i,j)
b=Icecoordinates(l)%b(i,j)
if(abs(h-hp)>1e-15) then
 Tb=Tf
else
 Tb=Ta
end if
if(surtempcontrol=='Y') then
Call BlanceT
end if
Rek=roua*Uae*ks/miua
if(Rek>=600) then
 recov=Pr**(1./3)
else
 recov=Pr**0.5
end if
Ql=lwc*beta*Wf*Lf
Qk=lwc*beta*Wf*dropv**2/2
if(fluidproperty=='incom') then
Qa=recov*hcv*Uae**2/(2*ca)
else if(fluidproperty=='com') then
Qa=0
end if
Qe=Lv*0.696*hcv*(610.8*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))-610.8*10**(7.63*(Ta-Tf)/(241.9+Ta-Tf)))/(ca*(2*P0+P)/2)
Qs=(Lv+Lf)*0.696*hcv*(610.8*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))-610.8*10**(7.63*(Ta-Tf)/(241.9+Ta-Tf)))/(ca*(2*P0+P)/2)
qd=lwc*beta*Wf*cw
qc=hcv
qr=4*epsi*sigmar*Ta**3
qe1=-(qd+qc+qr)/kw
qr1=-(qd+qc+qr)/ki
if(fluidproperty=='com') then
qe0=(Qk+Qa-Qe+qd*Td+qc*Ta)/kw
qr0=(Ql+Qk+Qa-Qs+qd*Td+qc*Ta)/ki
else if(fluidproperty=='incom') then
qe0=(Qk+Qa-Qe+qd*Td+qc*(Ta+(Wf**2-Uae**2)/(2*ca)))/kw
qr0=(Ql+Qk+Qa-Qs+qd*Td+qc*(Ta+(Wf**2-Uae**2)/(2*ca)))/ki
end if
mev=0.696*hcv*(610.8*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))-610.8*10**(7.63*(Ta-Tf)/(241.9+Ta-Tf)))/(ca*(2*P0+P)/2)
msv=0.696*hcv*(610.8*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))-610.8*10**(7.63*(Ta-Tf)/(241.9+Ta-Tf)))/(ca*(2*P0+P)/2)
Energys(l)%Tb(i,j)=Tb
Energys(l)%qe0(i,j)=qe0
Energys(l)%qe1(i,j)=qe1
Energys(l)%qr0(i,j)=qr0
Energys(l)%qr1(i,j)=qr1
Energys(l)%mev(i,j)=mev
Energys(l)%msv(i,j)=msv
end DO
end DO

contains
Subroutine BlanceT
DO s=1,100
  Tb0=Tb
  if(abs(h-hp)>1e-15) then
    dQe=Lv*0.696*hcv*610.8*2.3*7.63*(241.9/(241.9+Tb-Tf)**2)*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))/(ca*(2*P0+P)/2)
    dqe0=-dQe/kw
    Tb=Tb-(Tb-Tf-(qe0+qe1*Tf)*h/(1-qe1*h))/(1-dqe0*h/(1-qe1*h))
    Qe=Lv*0.696*hcv*(610.8*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))-610.8*10**(7.63*(Ta-Tf)/(241.9+Ta-Tf)))/(ca*(2*P0+P)/2)
    qe0=(Qk+Qa-Qe+qd*Td+qc*Ta)/kw
  else
    dQs=(Lv+Lf)*0.696*hcv*610.8*2.3*7.63*(241.9/(241.9+Tb-Tf)**2)*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))/(ca*(2*P0+P)/2)
    dqr0=-dQs/ki
    Tb=Tb-(Tb-Ts-(qr0+qr1*Ts)*b/(1-qr1*b))/(1-dqr0*b/(1-qr1*b))
    Qs=(Lv+Lf)*0.696*hcv*(610.8*10**(7.63*(Tb-Tf)/(241.9+Tb-Tf))-610.8*10**(7.63*(Ta-Tf)/(241.9+Ta-Tf)))/(ca*(2*P0+P)/2)
    qr0=(Ql+Qk+Qa-Qs+qd*Td+qc*Ta)/ki
  end if
  if(abs(Tb-Tb0)<1e-10) exit
end DO
if(mod(timestep,nint(timeout/1000.))==1.and.i==Ig(l).and.j==Jg(l)) then
  write(7,*) timestep,abs(Tb-Tb0),s
end if
end Subroutine BlanceT

end Subroutine Energyterm
