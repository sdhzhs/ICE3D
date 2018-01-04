Subroutine dhbctransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(Ig(topos%nbl(l)),j)
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(Ig(topos%nbl(l)),Jg(topos%nbl(l))+1-j)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l)))
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(j,Jg(topos%nbl(l)))
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(1,Jg(topos%nbl(l))+1-j)
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(1,j)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(Ig(topos%nbl(l))+1-j,1)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
  Boundatas(l)%dhbcl(j)=Imps(topos%nbl(l))%dh(j,1)
  else
  Boundatas(l)%dhbcl(j)=Imps(l)%dh(1,j)
  end if
  end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(1,j)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(1,Jg(topos%nbr(l))+1-j)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(Ig(topos%nbr(l))+1-j,1)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(j,1)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(Ig(topos%nbr(l)),Jg(topos%nbr(l))+1-j)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(Ig(topos%nbr(l)),j)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l)))
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
  Boundatas(l)%dhbcr(j)=Imps(topos%nbr(l))%dh(j,Jg(topos%nbr(l)))
  else
  Boundatas(l)%dhbcr(j)=Imps(l)%dh(Ig(l),j)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(Ig(topos%nbu(l)),i)
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(Ig(topos%nbu(l)),Jg(topos%nbu(l))+1-i)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l)))
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(i,Jg(topos%nbu(l)))
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(1,Jg(topos%nbu(l))+1-i)
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(1,i)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(Ig(topos%nbu(l))+1-i,1)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
  Boundatas(l)%dhbcu(i)=Imps(topos%nbu(l))%dh(i,1)
  else
  Boundatas(l)%dhbcu(i)=Imps(l)%dh(i,1)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(1,i)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(1,Jg(topos%nbd(l))+1-i)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(Ig(topos%nbd(l))+1-i,1)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(i,1)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(Ig(topos%nbd(l)),Jg(topos%nbd(l))+1-i)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(Ig(topos%nbd(l)),i)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l)))
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
  Boundatas(l)%dhbcd(i)=Imps(topos%nbd(l))%dh(i,Jg(topos%nbd(l)))
  else
  Boundatas(l)%dhbcd(i)=Imps(l)%dh(i,Jg(l))
  end if
  end DO
end DO
end Subroutine dhbctransfer
