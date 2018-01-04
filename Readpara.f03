Subroutine Readpara
use COM
implicit none
character(1) ch
print *,'Input from script file(Y/N)?'
Scpt='Y'
if(Scpt=='Y') then
open(unit=10,file='Script.txt',status='old')
read(10,*) ch
read(10,*) filename(1)
read(10,*) ch
read(10,*) filename(2)
read(10,*) ch
read(10,*) filename(3)
read(10,*) ch
read(10,*) filename(4)
read(10,*) ch
read(10,*) fluidproperty
read(10,*) ch
read(10,*) advancemethod
read(10,*) ch
read(10,*) icecoupled
read(10,*) ch
read(10,*) c
read(10,*) ch
read(10,*) lwc
read(10,*) ch
read(10,*) mvd
read(10,*) ch
read(10,*) Ta
read(10,*) ch
read(10,*) Td
read(10,*) ch
read(10,*) Ts
read(10,*) ch
read(10,*) Wf
read(10,*) ch
read(10,*) dt
read(10,*) ch
read(10,*) t
read(10,*) ch
read(10,*) initialtimecontrol
if(initialtimecontrol=='Y') then
read(10,*) ch
read(10,*) hp
else if(initialtimecontrol=='N') then
read(10,*) ch
if(advancemethod=='multi') then
read(10,*) hp
else
read(10,*) filename(5)
end if
end if
read(10,*) ch
read(10,*) surtempcontrol
read(10,*) ch
read(10,*) solutioncontrol
read(10,*) ch
read(10,*) discretecontrol
if(solutioncontrol/='explicit') then
read(10,*) ch
read(10,*) maxl
read(10,*) ch
read(10,*) err
end if
close(10)
else
print *,'Give name of initial surface grids file:'
read *,filename(1)
print *,'Give name of surface grids file:'
read *,filename(2)
print *,'Give name of air fluid property file:'
read *,filename(3)
print *,'Give name of drop property file:'
read *,filename(4)
print *,'Set fluid property(com/incom):'
read *,fluidproperty
print *,'Select advanced method(singl/multi):'
read *,advancemethod
print *,'Include Ice Accretion(Y/N)?'
read *,icecoupled
print *,'input grid scale factor:'
read *,c
print *,'input liquid water content(kg/m3):'
read *,lwc
print *,'input mean volumn diameter(m):'
read *,mvd
print *,'input air far field tempreture(K):'
read *,Ta
print *,'input drop tempreture(K):'
read *,Td
print *,'input initial solid surface tempreture(K):'
read *,Ts
print *,'input air far field velocity(m/s):'
read *,Wf
print *,'Give time step parameter(s):'
read *,dt
print *,'Give total advance time(s):'
read *,t
print *,'Start advance from clean surface(Y/N)?'
read *,initialtimecontrol
if(initialtimecontrol=='Y') then
print *,'Input presuror film height:'
read *,hp
else if(initialtimecontrol=='N') then
if(advancemethod=='multi') then
print *,'Input presuror film height:'
read *, hp
else
print *,'Give name of initial data file:'
read *,filename(5)
end if
end if
print *,'Start inner surface tempreture iteration(Y/N)?'
read *,surtempcontrol
print *,'Select solution method(explicit/implicit)?'
read *,solutioncontrol
print *,'Select discretation scheme(upwind/TVD)?'
read *,discretecontrol
if(solutioncontrol/='explicit') then
print *,'Give max step of inner implicit iteration:'
read *,maxl
print *,'Give resident of inner implicit iteration:'
read *,err
end if
end if
print *,'Read control parameters completed!'
end Subroutine Readpara
