Subroutine innerBCtransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l)),j)
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l))-1,j)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l)),j)
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l))-1,j)
  Boundatas(l)%Qp1bcl(j)=Fluxs(topos%nbl(l))%Qp1(Ig(topos%nbl(l)),j)
  Boundatas(l)%dQp1bcl(j)=Fluxs(topos%nbl(l))%dQp1(Ig(topos%nbl(l)),j)
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l)),Jg(l)+1-j)
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l))-1,Jg(l)+1-j)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l)),Jg(l)+1-j)
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l))-1,Jg(l)+1-j)
  Boundatas(l)%Qp1bcl(j)=Fluxs(topos%nbl(l))%Qp1(Ig(topos%nbl(l)),Jg(l)+1-j)
  Boundatas(l)%dQp1bcl(j)=Fluxs(topos%nbl(l))%dQp1(Ig(topos%nbl(l)),Jg(l)+1-j)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l)))
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l))-1)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l)))
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l))-1)
  Boundatas(l)%Qp1bcl(j)=Fluxs(topos%nbl(l))%Qp2(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l)))
  Boundatas(l)%dQp1bcl(j)=Fluxs(topos%nbl(l))%dQp2(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l)))
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(j,Jg(topos%nbl(l)))
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(j,Jg(topos%nbl(l))-1)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(j,Jg(topos%nbl(l)))
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(j,Jg(topos%nbl(l))-1)
  Boundatas(l)%Qp1bcl(j)=Fluxs(topos%nbl(l))%Qp2(j,Jg(topos%nbl(l)))
  Boundatas(l)%dQp1bcl(j)=Fluxs(topos%nbl(l))%dQp2(j,Jg(topos%nbl(l)))
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(1,Jg(topos%nbl(l))+1-j)
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(2,Jg(topos%nbl(l))+1-j)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(1,Jg(topos%nbl(l))+1-j)
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(2,Jg(topos%nbl(l))+1-j)
  Boundatas(l)%Qp1bcl(j)=-Fluxs(topos%nbl(l))%Qp1(1,Jg(topos%nbl(l))+1-j)
  Boundatas(l)%dQp1bcl(j)=-Fluxs(topos%nbl(l))%dQp1(1,Jg(topos%nbl(l))+1-j)
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(1,j)
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(2,j)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(1,j)
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(2,j)
  Boundatas(l)%Qp1bcl(j)=-Fluxs(topos%nbl(l))%Qp1(1,j)
  Boundatas(l)%dQp1bcl(j)=-Fluxs(topos%nbl(l))%dQp1(1,j)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l))+1-j,1)
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(Ig(topos%nbl(l))+1-j,2)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l))+1-j,1)
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(Ig(topos%nbl(l))+1-j,2)
  Boundatas(l)%Qp1bcl(j)=-Fluxs(topos%nbl(l))%Qp2(Ig(topos%nbl(l))+1-j,1)
  Boundatas(l)%dQp1bcl(j)=-Fluxs(topos%nbl(l))%dQp2(Ig(topos%nbl(l))+1-j,1)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos%nbl(l))%h(j,1)
  Boundatas(l)%hbcll(j)=Icecoordinates(topos%nbl(l))%h(j,2)
  Boundatas(l)%bbcl(j)=Icecoordinates(topos%nbl(l))%b(j,1)
  Boundatas(l)%bbcll(j)=Icecoordinates(topos%nbl(l))%b(j,2)
  Boundatas(l)%Qp1bcl(j)=-Fluxs(topos%nbl(l))%Qp2(j,1)
  Boundatas(l)%dQp1bcl(j)=-Fluxs(topos%nbl(l))%dQp2(j,1)
  else
  Boundatas(l)%hbcl(j)=Icecoordinates(l)%h(1,j)
  Boundatas(l)%hbcll(j)=Icecoordinates(l)%h(1,j)
  Boundatas(l)%bbcl(j)=Icecoordinates(l)%b(1,j)
  Boundatas(l)%bbcll(j)=Icecoordinates(l)%b(1,j)
  Boundatas(l)%Qp1bcl(j)=-Fluxs(l)%Qp1(1,j)
  Boundatas(l)%dQp1bcl(j)=-Fluxs(l)%dQp1(1,j)
  end if
  end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(1,j)
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(2,j)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(1,j)
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(2,j)
  Boundatas(l)%Qp1bcr(j)=Fluxs(topos%nbr(l))%Qp1(1,j)
  Boundatas(l)%dQp1bcr(j)=Fluxs(topos%nbr(l))%dQp1(1,j)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(1,Jg(l)+1-j)
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(2,Jg(l)+1-j)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(1,Jg(l)+1-j)
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(2,Jg(l)+1-j)
  Boundatas(l)%Qp1bcr(j)=Fluxs(topos%nbr(l))%Qp1(1,Jg(l)+1-j)
  Boundatas(l)%dQp1bcr(j)=Fluxs(topos%nbr(l))%dQp1(1,Jg(l)+1-j)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l))+1-j,1)
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l))+1-j,2)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l))+1-j,1)
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l))+1-j,2)
  Boundatas(l)%Qp1bcr(j)=Fluxs(topos%nbr(l))%Qp2(Ig(topos%nbr(l))+1-j,1)
  Boundatas(l)%dQp1bcr(j)=Fluxs(topos%nbr(l))%dQp2(Ig(topos%nbr(l))+1-j,1)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(j,1)
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(j,2)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(j,1)
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(j,2)
  Boundatas(l)%Qp1bcr(j)=Fluxs(topos%nbr(l))%Qp2(j,1)
  Boundatas(l)%dQp1bcr(j)=Fluxs(topos%nbr(l))%dQp2(j,1)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l)),Jg(topos%nbr(l))+1-j)
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l))-1,Jg(topos%nbr(l))+1-j)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l)),Jg(topos%nbr(l))+1-j)
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l))-1,Jg(topos%nbr(l))+1-j)
  Boundatas(l)%Qp1bcr(j)=-Fluxs(topos%nbr(l))%Qp1(Ig(topos%nbr(l)),Jg(topos%nbr(l))+1-j)
  Boundatas(l)%dQp1bcr(j)=-Fluxs(topos%nbr(l))%dQp1(Ig(topos%nbr(l)),Jg(topos%nbr(l))+1-j)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l)),j)
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l))-1,j)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l)),j)
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l))-1,j)
  Boundatas(l)%Qp1bcr(j)=-Fluxs(topos%nbr(l))%Qp1(Ig(topos%nbr(l)),j)
  Boundatas(l)%dQp1bcr(j)=-Fluxs(topos%nbr(l))%dQp1(Ig(topos%nbr(l)),j)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l)))
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l))-1)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l)))
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l))-1)
  Boundatas(l)%Qp1bcr(j)=-Fluxs(topos%nbr(l))%Qp2(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l)))
  Boundatas(l)%dQp1bcr(j)=-Fluxs(topos%nbr(l))%dQp2(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l)))
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos%nbr(l))%h(j,Jg(topos%nbr(l)))
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos%nbr(l))%h(j,Jg(topos%nbr(l))-1)
  Boundatas(l)%bbcr(j)=Icecoordinates(topos%nbr(l))%b(j,Jg(topos%nbr(l)))
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos%nbr(l))%b(j,Jg(topos%nbr(l))-1)
  Boundatas(l)%Qp1bcr(j)=-Fluxs(topos%nbr(l))%Qp2(j,Jg(topos%nbr(l)))
  Boundatas(l)%dQp1bcr(j)=-Fluxs(topos%nbr(l))%dQp2(j,Jg(topos%nbr(l)))
  else
  Boundatas(l)%hbcr(j)=Icecoordinates(l)%h(Ig(l),j)
  Boundatas(l)%hbcrr(j)=Icecoordinates(l)%h(Ig(l),j)
  Boundatas(l)%bbcr(j)=Icecoordinates(l)%b(Ig(l),j)
  Boundatas(l)%bbcrr(j)=Icecoordinates(l)%b(Ig(l),j)
  Boundatas(l)%Qp1bcr(j)=-Fluxs(l)%Qp1(Ig(l),j)
  Boundatas(l)%dQp1bcr(j)=-Fluxs(l)%dQp1(Ig(l),j)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l)),i)
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l))-1,i)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l)),i)
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l))-1,i)
  Boundatas(l)%Qp2bcu(i)=Fluxs(topos%nbu(l))%Qp1(Ig(topos%nbu(l)),i)
  Boundatas(l)%dQp2bcu(i)=Fluxs(topos%nbu(l))%dQp1(Ig(topos%nbu(l)),i)
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l)),Jg(topos%nbu(l))+1-i)
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l))-1,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l)),Jg(topos%nbu(l))+1-i)
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l))-1,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%Qp2bcu(i)=Fluxs(topos%nbu(l))%Qp1(Ig(topos%nbu(l)),Jg(topos%nbu(l))+1-i)
  Boundatas(l)%dQp2bcu(i)=Fluxs(topos%nbu(l))%dQp1(Ig(topos%nbu(l)),Jg(topos%nbu(l))+1-i)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l)))
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l))-1)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l)))
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l))-1)
  Boundatas(l)%Qp2bcu(i)=Fluxs(topos%nbu(l))%Qp2(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l)))
  Boundatas(l)%dQp2bcu(i)=Fluxs(topos%nbu(l))%dQp2(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l)))
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(i,Jg(topos%nbu(l)))
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(i,Jg(topos%nbu(l))-1)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(i,Jg(topos%nbu(l)))
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(i,Jg(topos%nbu(l))-1)
  Boundatas(l)%Qp2bcu(i)=Fluxs(topos%nbu(l))%Qp2(i,Jg(topos%nbu(l)))
  Boundatas(l)%dQp2bcu(i)=Fluxs(topos%nbu(l))%dQp2(i,Jg(topos%nbu(l)))
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(1,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(2,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(1,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(2,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%Qp2bcu(i)=-Fluxs(topos%nbu(l))%Qp1(1,Jg(topos%nbu(l))+1-i)
  Boundatas(l)%dQp2bcu(i)=-Fluxs(topos%nbu(l))%dQp1(1,Jg(topos%nbu(l))+1-i)
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(1,i)
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(2,i)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(1,i)
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(2,i)
  Boundatas(l)%Qp2bcu(i)=-Fluxs(topos%nbu(l))%Qp1(1,i)
  Boundatas(l)%dQp2bcu(i)=-Fluxs(topos%nbu(l))%dQp1(1,i)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l))+1-i,1)
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(Ig(topos%nbu(l))+1-i,2)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l))+1-i,1)
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(Ig(topos%nbu(l))+1-i,2)
  Boundatas(l)%Qp2bcu(i)=-Fluxs(topos%nbu(l))%Qp2(Ig(topos%nbu(l))+1-i,1)
  Boundatas(l)%dQp2bcu(i)=-Fluxs(topos%nbu(l))%dQp2(Ig(topos%nbu(l))+1-i,1)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos%nbu(l))%h(i,1)
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos%nbu(l))%h(i,2)
  Boundatas(l)%bbcu(i)=Icecoordinates(topos%nbu(l))%b(i,1)
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos%nbu(l))%b(i,2)
  Boundatas(l)%Qp2bcu(i)=-Fluxs(topos%nbu(l))%Qp2(i,1)
  Boundatas(l)%dQp2bcu(i)=-Fluxs(topos%nbu(l))%dQp2(i,1)
  else
  Boundatas(l)%hbcu(i)=Icecoordinates(l)%h(i,1)
  Boundatas(l)%hbcuu(i)=Icecoordinates(l)%h(i,1)
  Boundatas(l)%bbcu(i)=Icecoordinates(l)%b(i,1)
  Boundatas(l)%bbcuu(i)=Icecoordinates(l)%b(i,1)
  Boundatas(l)%Qp2bcu(i)=-Fluxs(l)%Qp2(i,1)
  Boundatas(l)%dQp2bcu(i)=-Fluxs(l)%dQp2(i,1)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(1,i)
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(2,i)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(1,i)
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(2,i)
  Boundatas(l)%Qp2bcd(i)=Fluxs(topos%nbd(l))%Qp1(1,i)
  Boundatas(l)%dQp2bcd(i)=Fluxs(topos%nbd(l))%dQp1(1,i)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(1,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(2,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(1,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(2,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%Qp2bcd(i)=Fluxs(topos%nbd(l))%Qp1(1,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%dQp2bcd(i)=Fluxs(topos%nbd(l))%dQp1(1,Jg(topos%nbd(l))+1-i)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l))+1-i,1)
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l))+1-i,2)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l))+1-i,1)
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l))+1-i,2)
  Boundatas(l)%Qp2bcd(i)=Fluxs(topos%nbd(l))%Qp2(Ig(topos%nbd(l))+1-i,1)
  Boundatas(l)%dQp2bcd(i)=Fluxs(topos%nbd(l))%dQp2(Ig(topos%nbd(l))+1-i,1)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(i,1)
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(i,2)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(i,1)
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(i,2)
  Boundatas(l)%Qp2bcd(i)=Fluxs(topos%nbd(l))%Qp2(i,1)
  Boundatas(l)%dQp2bcd(i)=Fluxs(topos%nbd(l))%dQp2(i,1)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l)),Jg(topos%nbd(l))+1-i)
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l))-1,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l)),Jg(topos%nbd(l))+1-i)
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l))-1,Jg(topos%nbd(l))+1-i)
  Boundatas(l)%Qp2bcd(i)=-Fluxs(topos%nbd(l))%Qp1(Ig(topos%nbd(l)),Jg(topos%nbd(l))+1-i)
  Boundatas(l)%dQp2bcd(i)=-Fluxs(topos%nbd(l))%dQp1(Ig(topos%nbd(l)),Jg(topos%nbd(l))+1-i)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l)),i)
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l))-1,i)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l)),i)
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l))-1,i)
  Boundatas(l)%Qp2bcd(i)=-Fluxs(topos%nbd(l))%Qp1(Ig(topos%nbd(l)),i)
  Boundatas(l)%dQp2bcd(i)=-Fluxs(topos%nbd(l))%dQp1(Ig(topos%nbd(l)),i)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l)))
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l))-1)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l)))
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l))-1)
  Boundatas(l)%Qp2bcd(i)=-Fluxs(topos%nbd(l))%Qp2(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l)))
  Boundatas(l)%dQp2bcd(i)=-Fluxs(topos%nbd(l))%dQp2(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l)))
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos%nbd(l))%h(i,Jg(topos%nbd(l)))
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos%nbd(l))%h(i,Jg(topos%nbd(l))-1)
  Boundatas(l)%bbcd(i)=Icecoordinates(topos%nbd(l))%b(i,Jg(topos%nbd(l)))
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos%nbd(l))%b(i,Jg(topos%nbd(l))-1)
  Boundatas(l)%Qp2bcd(i)=-Fluxs(topos%nbd(l))%Qp2(i,Jg(topos%nbd(l)))
  Boundatas(l)%dQp2bcd(i)=-Fluxs(topos%nbd(l))%dQp2(i,Jg(topos%nbd(l)))
  else
  Boundatas(l)%hbcd(i)=Icecoordinates(l)%h(i,Jg(l))
  Boundatas(l)%hbcdd(i)=Icecoordinates(l)%h(i,Jg(l))
  Boundatas(l)%bbcd(i)=Icecoordinates(l)%b(i,Jg(l))
  Boundatas(l)%bbcdd(i)=Icecoordinates(l)%b(i,Jg(l))
  Boundatas(l)%Qp2bcd(i)=-Fluxs(l)%Qp2(i,Jg(l))
  Boundatas(l)%dQp2bcd(i)=-Fluxs(l)%dQp2(i,Jg(l))
  end if
  end DO
end DO
end Subroutine innerBCtransfer
