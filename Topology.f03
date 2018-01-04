Subroutine Topology
use COM
implicit none
integer k,l
real(8)::tol=1e-10
DO l=1,block
topos%nbl(l)=0
topos%nbr(l)=0
topos%nbu(l)=0
topos%nbd(l)=0
end DO
DO l=1,block
  DO k=1,block
  if(k/=l) then
  if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='J'
  topos%positl(l)='S'
  topos%orientl(l)='+'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='J'
  topos%positl(l)='S'
  topos%orientl(l)='-'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='I'
  topos%positl(l)='S'
  topos%orientl(l)='-'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='I'
  topos%positl(l)='S'
  topos%orientl(l)='+'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='J'
  topos%positl(l)='N'
  topos%orientl(l)='-'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='J'
  topos%positl(l)='N'
  topos%orientl(l)='+'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='I'
  topos%positl(l)='N'
  topos%orientl(l)='+'
  else if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,Jn(l)))<tol) then
  topos%nbl(l)=k
  topos%topol(l)='I'
  topos%positl(l)='N'
  topos%orientl(l)='-'
  end if
  end if
  end DO
end DO
DO l=1,block
  DO k=1,block
  if(k/=l) then
  if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='J'
  topos%positr(l)='N'
  topos%orientr(l)='+'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='J'
  topos%positr(l)='N'
  topos%orientr(l)='-'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='I'
  topos%positr(l)='N'
  topos%orientr(l)='-'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='I'
  topos%positr(l)='N'
  topos%orientr(l)='+'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='J'
  topos%positr(l)='S'
  topos%orientr(l)='-'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='J'
  topos%positr(l)='S'
  topos%orientr(l)='+'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='I'
  topos%positr(l)='S'
  topos%orientr(l)='+'
  else if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),1))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),1))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbr(l)=k
  topos%topor(l)='I'
  topos%positr(l)='S'
  topos%orientr(l)='-'
  end if
  end if
  end DO
end DO
DO l=1,block
  DO k=1,block
  if(k/=l) then
  if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='J'
  topos%positu(l)='S'
  topos%orientu(l)='+'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='J'
  topos%positu(l)='S'
  topos%orientu(l)='-'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='I'
  topos%positu(l)='S'
  topos%orientu(l)='-'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='I'
  topos%positu(l)='S'
  topos%orientu(l)='+'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='J'
  topos%positu(l)='N'
  topos%orientu(l)='-'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='J'
  topos%positu(l)='N'
  topos%orientu(l)='+'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='I'
  topos%positu(l)='N'
  topos%orientu(l)='+'
  else if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),1))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,1))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,1))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),1))<tol) then
  topos%nbu(l)=k
  topos%topou(l)='I'
  topos%positu(l)='N'
  topos%orientu(l)='-'
  end if
  end if
  end DO
end DO
DO l=1,block
  DO k=1,block
  if(k/=l) then
  if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='J'
  topos%positd(l)='N'
  topos%orientd(l)='+'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='J'
  topos%positd(l)='N'
  topos%orientd(l)='-'
  else if(abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='I'
  topos%positd(l)='N'
  topos%orientd(l)='-'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(In(k),Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(In(k),Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='I'
  topos%positd(l)='N'
  topos%orientd(l)='+'
  else if(abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='J'
  topos%positd(l)='S'
  topos%orientd(l)='-'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(1,Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(1,Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='J'
  topos%positd(l)='S'
  topos%orientd(l)='+'
  else if(abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='I'
  topos%positd(l)='S'
  topos%orientd(l)='+'
  else if(abs(Grids(k)%Xp(In(k),1)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Xp(1,1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),1)-Grids(l)%Yp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),1)-Grids(l)%Zp(1,Jn(l)))<tol.and.abs(Grids(k)%Zp(1,1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  topos%nbd(l)=k
  topos%topod(l)='I'
  topos%positd(l)='S'
  topos%orientd(l)='-'
  end if
  end if
  end DO
end DO
end Subroutine Topology
