Subroutine MBSolve
use COM
implicit none
integer l,s
logical lret
DO s=1,maxl
  DO l=1,block
  Imps(l)%dh0=Imps(l)%dh
  end DO
  Call dhbctransfer
  lret=.true.
  DO l=1,block
  Call Implicitcompute(l)
  lret=lret.and.maxval(abs(Imps(l)%dh-Imps(l)%dh0))<err
  end DO
  if(lret) exit
end DO
DO l=1,block
  if(mod(timestep,10)==1.or.timestep==timeout) then
  write(8,*) timestep,maxval(abs(Imps(l)%dh-Imps(l)%dh0)),minval(abs(Imps(l)%dh-Imps(l)%dh0)),s
  end if
end DO
end Subroutine MBSolve
