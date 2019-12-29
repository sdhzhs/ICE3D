Subroutine innerBCtransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topol(j)=='r') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%hbcll(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%bbcl(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%bbcll(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%Qp1bcl(j)=Fluxs(topos(l)%nbl(j))%Qp1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%dQp1bcl(j)=Fluxs(topos(l)%nbl(j))%dQp1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  else if(topos(l)%topol(j)=='d') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%hbcll(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%bbcl(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%bbcll(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%Qp1bcl(j)=Fluxs(topos(l)%nbl(j))%Qp2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%dQp1bcl(j)=Fluxs(topos(l)%nbl(j))%dQp2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  else if(topos(l)%topol(j)=='l') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%hbcll(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%bbcl(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%bbcll(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%Qp1bcl(j)=-Fluxs(topos(l)%nbl(j))%Qp1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%dQp1bcl(j)=-Fluxs(topos(l)%nbl(j))%dQp1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  else if(topos(l)%topol(j)=='u') then
  Boundatas(l)%hbcl(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%hbcll(j)=Icecoordinates(topos(l)%nbl(j))%h(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%bbcl(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%bbcll(j)=Icecoordinates(topos(l)%nbl(j))%b(topos(l)%Iindexll(j),topos(l)%Jindexll(j))
  Boundatas(l)%Qp1bcl(j)=-Fluxs(topos(l)%nbl(j))%Qp2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  Boundatas(l)%dQp1bcl(j)=-Fluxs(topos(l)%nbl(j))%dQp2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
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
  if(topos(l)%topor(j)=='l') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%bbcr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%Qp1bcr(j)=Fluxs(topos(l)%nbr(j))%Qp1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%dQp1bcr(j)=Fluxs(topos(l)%nbr(j))%dQp1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  else if(topos(l)%topor(j)=='u') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%bbcr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%Qp1bcr(j)=Fluxs(topos(l)%nbr(j))%Qp2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%dQp1bcr(j)=Fluxs(topos(l)%nbr(j))%dQp2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  else if(topos(l)%topor(j)=='r') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%bbcr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%Qp1bcr(j)=-Fluxs(topos(l)%nbr(j))%Qp1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%dQp1bcr(j)=-Fluxs(topos(l)%nbr(j))%dQp1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  else if(topos(l)%topor(j)=='d') then
  Boundatas(l)%hbcr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%hbcrr(j)=Icecoordinates(topos(l)%nbr(j))%h(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%bbcr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%bbcrr(j)=Icecoordinates(topos(l)%nbr(j))%b(topos(l)%Iindexrr(j),topos(l)%Jindexrr(j))
  Boundatas(l)%Qp1bcr(j)=-Fluxs(topos(l)%nbr(j))%Qp2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  Boundatas(l)%dQp1bcr(j)=-Fluxs(topos(l)%nbr(j))%dQp2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
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
  if(topos(l)%topou(i)=='r') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%bbcu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%Qp2bcu(i)=Fluxs(topos(l)%nbu(i))%Qp1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%dQp2bcu(i)=Fluxs(topos(l)%nbu(i))%dQp1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  else if(topos(l)%topou(i)=='d') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%bbcu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%Qp2bcu(i)=Fluxs(topos(l)%nbu(i))%Qp2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%dQp2bcu(i)=Fluxs(topos(l)%nbu(i))%dQp2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  else if(topos(l)%topou(i)=='l') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%bbcu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%Qp2bcu(i)=-Fluxs(topos(l)%nbu(i))%Qp1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%dQp2bcu(i)=-Fluxs(topos(l)%nbu(i))%dQp1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  else if(topos(l)%topou(i)=='u') then
  Boundatas(l)%hbcu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%hbcuu(i)=Icecoordinates(topos(l)%nbu(i))%h(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%bbcu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%bbcuu(i)=Icecoordinates(topos(l)%nbu(i))%b(topos(l)%Iindexuu(i),topos(l)%Jindexuu(i))
  Boundatas(l)%Qp2bcu(i)=-Fluxs(topos(l)%nbu(i))%Qp2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  Boundatas(l)%dQp2bcu(i)=-Fluxs(topos(l)%nbu(i))%dQp2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
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
  if(topos(l)%topod(i)=='l') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%bbcd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%Qp2bcd(i)=Fluxs(topos(l)%nbd(i))%Qp1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%dQp2bcd(i)=Fluxs(topos(l)%nbd(i))%dQp1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  else if(topos(l)%topod(i)=='u') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%bbcd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%Qp2bcd(i)=Fluxs(topos(l)%nbd(i))%Qp2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%dQp2bcd(i)=Fluxs(topos(l)%nbd(i))%dQp2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  else if(topos(l)%topod(i)=='r') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%bbcd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%Qp2bcd(i)=-Fluxs(topos(l)%nbd(i))%Qp1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%dQp2bcd(i)=-Fluxs(topos(l)%nbd(i))%dQp1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  else if(topos(l)%topod(i)=='d') then
  Boundatas(l)%hbcd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%hbcdd(i)=Icecoordinates(topos(l)%nbd(i))%h(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%bbcd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%bbcdd(i)=Icecoordinates(topos(l)%nbd(i))%b(topos(l)%Iindexdd(i),topos(l)%Jindexdd(i))
  Boundatas(l)%Qp2bcd(i)=-Fluxs(topos(l)%nbd(i))%Qp2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  Boundatas(l)%dQp2bcd(i)=-Fluxs(topos(l)%nbd(i))%dQp2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
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
