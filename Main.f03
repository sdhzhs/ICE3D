Program ICE3D
use COM
implicit none
integer l
real(8) massold,massdrop,massice,masswater
logical lret
Call Readpara
Call Readdata
Call Constants
Call Pre
Call Topology
Call BCtransfer
DO l=1,block
Call Forceparameter(l)
end DO
print *,'Force parameter compute completed!'
Call BCtransfer
print *,'BC force parameter transfer completed!'
Call Initial
massold=0
DO l=1,block
massold=massold+sum((rhow*Icecoordinates(l)%h+rhoi*Icecoordinates(l)%b)*Grids(l)%panel)
end DO
timeout=nint(t/dt)
open(unit=4,file='water&ice.txt',status='replace')
open(unit=7,file='Energy term.txt',status='replace')
open(unit=8,file='implicitinner.txt',status='replace')
write(4,*) surtempcontrol,icecoupled,initialtimecontrol
write(4,*) fluidproperty,advancemethod,solutioncontrol,discretecontrol
print *,'Time advancing is beginning...'
DO timestep=1,timeout
  if(icecoupled=='Y') then
  DO l=1,block
   Call Energyterm(l)
  end DO
  end if
  DO l=1,block
   Call Fluxp(l)
  end DO
  Call innerBCtransfer
  DO l=1,block
   Call Wavevelocity(l)
  end DO
  Call saBCtransfer
  DO l=1,block
   Call Fluxf(l)
  end DO
  if(solutioncontrol=='explicit') then
  DO l=1,block
   Call Explicitcompute(l)
  end DO
  else if(solutioncontrol=='implicit') then
  DO l=1,block
   Imps(l)%dh=0
  end DO
  Call MBSolve
  !Call hypresolve(3)
  DO l=1,block
   Icecoordinates(l)%h=Icecoordinates(l)%h+Imps(l)%dh
  end DO
  if(icecoupled=='Y') then
  DO l=1,block
   Call Impliciticecompute(l)
  end DO
  end if
  end if
  Call innerBCtransfer
  if(mod(timestep,nint(timeout/4.))==0.or.timestep==timeout) then
  if(icecoupled=='Y') then
  DO l=1,block
  if(advancemethod=='multi') then
  Icecoordinates(l)%b=Icecoordinates(l)%b-Icecoordinates(l)%b0
  Boundatas(l)%bbcl=Boundatas(l)%bbcl-Boundatas(l)%bbcl0
  Boundatas(l)%bbcr=Boundatas(l)%bbcr-Boundatas(l)%bbcr0
  Boundatas(l)%bbcu=Boundatas(l)%bbcu-Boundatas(l)%bbcu0
  Boundatas(l)%bbcd=Boundatas(l)%bbcd-Boundatas(l)%bbcd0
  end if
  Call Iceshape(l)
  if(advancemethod=='multi') then
  Icecoordinates(l)%b=Icecoordinates(l)%b+Icecoordinates(l)%b0
  Boundatas(l)%bbcl=Boundatas(l)%bbcl+Boundatas(l)%bbcl0
  Boundatas(l)%bbcr=Boundatas(l)%bbcr+Boundatas(l)%bbcr0
  Boundatas(l)%bbcu=Boundatas(l)%bbcu+Boundatas(l)%bbcu0
  Boundatas(l)%bbcd=Boundatas(l)%bbcd+Boundatas(l)%bbcd0
  end if
  end DO
  end if
  DO l=1,block
   Icecoordinates(l)%Xi0=Icecoordinates(l)%Xi
   Icecoordinates(l)%Yi0=Icecoordinates(l)%Yi
   Icecoordinates(l)%Zi0=Icecoordinates(l)%Zi
   Call Cornersmooth(l)
  end DO
  DO l=1,block
   Icecoordinates(l)%Xi=Icecoordinates(l)%Xi0
   Icecoordinates(l)%Yi=Icecoordinates(l)%Yi0
   Icecoordinates(l)%Zi=Icecoordinates(l)%Zi0
  end DO
  Call SaveResults
  end if
  if(mod(timestep,100)==1.or.timestep==timeout) then
  masswater=0
  massdrop=0
  massice=0
  DO l=1,block
   massdrop=massdrop+sum(lwc*Wf*Forces(l)%beta*Grids(l)%panel*timestep*dt)
   masswater=masswater+sum(rhow*Icecoordinates(l)%h*Grids(l)%panel)
   massice=massice+sum(rhoi*Icecoordinates(l)%b*Grids(l)%panel)
   write(4,*) timestep,maxval(abs(Icecoordinates(l)%h)),minval(abs(Icecoordinates(l)%h)),maxval(abs(Icecoordinates(l)%b)),&
   minval(abs(Icecoordinates(l)%b))
  end DO
  print *,massold+massdrop,masswater+massice,timestep
  end if
  lret=.false.
  DO l=1,block
   lret=lret.or.maxval(abs(Icecoordinates(l)%h))>1e+5.or.maxval(abs(Icecoordinates(l)%b))>1e+5
  end DO
  if(lret) then
  print *,'Time step of advance diverge!'
  stop
  end if
end DO
close(4)
close(7)
close(8)
print *,'Advancing time is out!'
print *,'Total mass flux of drops and existing mass:'
print *,massold+massdrop
print *,'Total mass of ice layer and water film:'
print *,masswater+massice

end Program ICE3D
