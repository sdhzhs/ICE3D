Subroutine BCtransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topol(j)/='n') then
   Boundatas(l)%Xbcl(j)=Grids(topos(l)%nbl(j))%X(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
   Boundatas(l)%Ybcl(j)=Grids(topos(l)%nbl(j))%Y(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
   Boundatas(l)%Zbcl(j)=Grids(topos(l)%nbl(j))%Z(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
   Boundatas(l)%Pbcl(j)=Forces(topos(l)%nbl(j))%P(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
   if(topos(l)%topol(j)=='r') then
    Boundatas(l)%G1bcl(j)=Forces(topos(l)%nbl(j))%G1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%A1bcl(j)=Forces(topos(l)%nbl(j))%A1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DEbcl(j)=Grids(topos(l)%nbl(j))%DE(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DGbcl(j)=Grids(topos(l)%nbl(j))%DG(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DFbcl(j)=Grids(topos(l)%nbl(j))%DF(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%dul(j)=Grids(topos(l)%nbl(j))%DE1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
   else if(topos(l)%topol(j)=='d') then
    Boundatas(l)%G1bcl(j)=Forces(topos(l)%nbl(j))%G2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%A1bcl(j)=Forces(topos(l)%nbl(j))%A2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DEbcl(j)=Grids(topos(l)%nbl(j))%DG(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DGbcl(j)=Grids(topos(l)%nbl(j))%DE(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DFbcl(j)=-Grids(topos(l)%nbl(j))%DF(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%dul(j)=Grids(topos(l)%nbl(j))%DG2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
   else if(topos(l)%topol(j)=='l') then
    Boundatas(l)%G1bcl(j)=-Forces(topos(l)%nbl(j))%G1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%A1bcl(j)=-Forces(topos(l)%nbl(j))%A1(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DEbcl(j)=Grids(topos(l)%nbl(j))%DE(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DGbcl(j)=Grids(topos(l)%nbl(j))%DG(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DFbcl(j)=Grids(topos(l)%nbl(j))%DF(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%dul(j)=Grids(topos(l)%nbl(j))%DE1(topos(l)%Iindexll(j),topos(l)%Jindexl(j))
   else if(topos(l)%topol(j)=='u') then
    Boundatas(l)%G1bcl(j)=-Forces(topos(l)%nbl(j))%G2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%A1bcl(j)=-Forces(topos(l)%nbl(j))%A2(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DEbcl(j)=Grids(topos(l)%nbl(j))%DG(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DGbcl(j)=Grids(topos(l)%nbl(j))%DE(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%DFbcl(j)=-Grids(topos(l)%nbl(j))%DF(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
    Boundatas(l)%dul(j)=Grids(topos(l)%nbl(j))%DG2(topos(l)%Iindexl(j),topos(l)%Jindexll(j))
   end if
  else
   Boundatas(l)%Xbcl(j)=Grids(l)%X(1,j)
   Boundatas(l)%Ybcl(j)=Grids(l)%Y(1,j)
   Boundatas(l)%Zbcl(j)=-Grids(l)%Z(1,j)
   Boundatas(l)%Pbcl(j)=Forces(l)%P(1,j)
   Boundatas(l)%G1bcl(j)=-Forces(l)%G1(1,j)
   Boundatas(l)%A1bcl(j)=-Forces(l)%A1(1,j)
   Boundatas(l)%DEbcl(j)=Grids(l)%DE(1,j)
   Boundatas(l)%DGbcl(j)=Grids(l)%DG(1,j)
   Boundatas(l)%DFbcl(j)=-Grids(l)%DF(1,j)
   Boundatas(l)%dul(j)=Grids(l)%DE1(1,j)
  end if
 end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topor(j)/='n') then
   Boundatas(l)%Xbcr(j)=Grids(topos(l)%nbr(j))%X(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
   Boundatas(l)%Ybcr(j)=Grids(topos(l)%nbr(j))%Y(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
   Boundatas(l)%Zbcr(j)=Grids(topos(l)%nbr(j))%Z(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
   Boundatas(l)%Pbcr(j)=Forces(topos(l)%nbr(j))%P(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
   if(topos(l)%topor(j)=='l') then
    Boundatas(l)%G1bcr(j)=Forces(topos(l)%nbr(j))%G1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%A1bcr(j)=Forces(topos(l)%nbr(j))%A1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DEbcr(j)=Grids(topos(l)%nbr(j))%DE(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DGbcr(j)=Grids(topos(l)%nbr(j))%DG(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DFbcr(j)=Grids(topos(l)%nbr(j))%DF(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%dur(j)=Grids(topos(l)%nbr(j))%DE1(topos(l)%Iindexrr(j),topos(l)%Jindexr(j))
   else if(topos(l)%topor(j)=='u') then
    Boundatas(l)%G1bcr(j)=Forces(topos(l)%nbr(j))%G2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%A1bcr(j)=Forces(topos(l)%nbr(j))%A2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DEbcr(j)=Grids(topos(l)%nbr(j))%DG(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DGbcr(j)=Grids(topos(l)%nbr(j))%DE(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DFbcr(j)=-Grids(topos(l)%nbr(j))%DF(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%dur(j)=Grids(topos(l)%nbr(j))%DG2(topos(l)%Iindexr(j),topos(l)%Jindexrr(j))
   else if(topos(l)%topor(j)=='r') then
    Boundatas(l)%G1bcr(j)=-Forces(topos(l)%nbr(j))%G1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%A1bcr(j)=-Forces(topos(l)%nbr(j))%A1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DEbcr(j)=Grids(topos(l)%nbr(j))%DE(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DGbcr(j)=Grids(topos(l)%nbr(j))%DG(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DFbcr(j)=Grids(topos(l)%nbr(j))%DF(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%dur(j)=Grids(topos(l)%nbr(j))%DE1(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
   else if(topos(l)%topor(j)=='d') then
    Boundatas(l)%G1bcr(j)=-Forces(topos(l)%nbr(j))%G2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%A1bcr(j)=-Forces(topos(l)%nbr(j))%A2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DEbcr(j)=Grids(topos(l)%nbr(j))%DG(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DGbcr(j)=Grids(topos(l)%nbr(j))%DE(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%DFbcr(j)=-Grids(topos(l)%nbr(j))%DF(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
    Boundatas(l)%dur(j)=Grids(topos(l)%nbr(j))%DG2(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
   end if
  else
   Boundatas(l)%Xbcr(j)=Grids(l)%X(Ig(l),j)
   Boundatas(l)%Ybcr(j)=Grids(l)%Y(Ig(l),j)
   Boundatas(l)%Zbcr(j)=-Grids(l)%Z(Ig(l),j)
   Boundatas(l)%Pbcr(j)=Forces(l)%P(Ig(l),j)
   Boundatas(l)%G1bcr(j)=-Forces(l)%G1(Ig(l),j)
   Boundatas(l)%A1bcr(j)=-Forces(l)%A1(Ig(l),j)
   Boundatas(l)%DEbcr(j)=Grids(l)%DE(Ig(l),j)
   Boundatas(l)%DGbcr(j)=Grids(l)%DG(Ig(l),j)
   Boundatas(l)%DFbcr(j)=-Grids(l)%DF(Ig(l),j)
   Boundatas(l)%dur(j)=Grids(l)%DE1(Ig(l),j)
  end if
 end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos(l)%topou(i)/='n') then
   Boundatas(l)%Xbcu(i)=Grids(topos(l)%nbu(i))%X(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
   Boundatas(l)%Ybcu(i)=Grids(topos(l)%nbu(i))%Y(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
   Boundatas(l)%Zbcu(i)=Grids(topos(l)%nbu(i))%Z(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
   Boundatas(l)%Pbcu(i)=Forces(topos(l)%nbu(i))%P(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
   if(topos(l)%topou(i)=='r') then
    Boundatas(l)%G2bcu(i)=Forces(topos(l)%nbu(i))%G1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%A2bcu(i)=Forces(topos(l)%nbu(i))%A1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DEbcu(i)=Grids(topos(l)%nbu(i))%DG(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DGbcu(i)=Grids(topos(l)%nbu(i))%DE(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DFbcu(i)=-Grids(topos(l)%nbu(i))%DF(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%dvu(i)=Grids(topos(l)%nbu(i))%DE1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
   else if(topos(l)%topou(i)=='d') then
    Boundatas(l)%G2bcu(i)=Forces(topos(l)%nbu(i))%G2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%A2bcu(i)=Forces(topos(l)%nbu(i))%A2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DEbcu(i)=Grids(topos(l)%nbu(i))%DE(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DGbcu(i)=Grids(topos(l)%nbu(i))%DG(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DFbcu(i)=Grids(topos(l)%nbu(i))%DF(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%dvu(i)=Grids(topos(l)%nbu(i))%DG2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
   else if(topos(l)%topou(i)=='l') then
    Boundatas(l)%G2bcu(i)=-Forces(topos(l)%nbu(i))%G1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%A2bcu(i)=-Forces(topos(l)%nbu(i))%A1(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DEbcu(i)=Grids(topos(l)%nbu(i))%DG(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DGbcu(i)=Grids(topos(l)%nbu(i))%DE(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DFbcu(i)=-Grids(topos(l)%nbu(i))%DF(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%dvu(i)=Grids(topos(l)%nbu(i))%DE1(topos(l)%Iindexuu(i),topos(l)%Jindexu(i))
   else if(topos(l)%topou(i)=='u') then
    Boundatas(l)%G2bcu(i)=-Forces(topos(l)%nbu(i))%G2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%A2bcu(i)=-Forces(topos(l)%nbu(i))%A2(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DEbcu(i)=Grids(topos(l)%nbu(i))%DE(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DGbcu(i)=Grids(topos(l)%nbu(i))%DG(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%DFbcu(i)=Grids(topos(l)%nbu(i))%DF(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
    Boundatas(l)%dvu(i)=Grids(topos(l)%nbu(i))%DG2(topos(l)%Iindexu(i),topos(l)%Jindexuu(i))
   end if
  else
   Boundatas(l)%Xbcu(i)=Grids(l)%X(i,1)
   Boundatas(l)%Ybcu(i)=Grids(l)%Y(i,1)
   Boundatas(l)%Zbcu(i)=-Grids(l)%Z(i,1)
   Boundatas(l)%Pbcu(i)=Forces(l)%P(i,1)
   Boundatas(l)%G2bcu(i)=-Forces(l)%G2(i,1)
   Boundatas(l)%A2bcu(i)=-Forces(l)%A2(i,1)
   Boundatas(l)%DEbcu(i)=Grids(l)%DE(i,1)
   Boundatas(l)%DGbcu(i)=Grids(l)%DG(i,1)
   Boundatas(l)%DFbcu(i)=-Grids(l)%DF(i,1)
   Boundatas(l)%dvu(i)=Grids(l)%DG2(i,1)
  end if
 end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos(l)%topod(i)/='n') then
   Boundatas(l)%Xbcd(i)=Grids(topos(l)%nbd(i))%X(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
   Boundatas(l)%Ybcd(i)=Grids(topos(l)%nbd(i))%Y(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
   Boundatas(l)%Zbcd(i)=Grids(topos(l)%nbd(i))%Z(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
   Boundatas(l)%Pbcd(i)=Forces(topos(l)%nbd(i))%P(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
   if(topos(l)%topod(i)=='l') then
    Boundatas(l)%G2bcd(i)=Forces(topos(l)%nbd(i))%G1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%A2bcd(i)=Forces(topos(l)%nbd(i))%A1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DEbcd(i)=Grids(topos(l)%nbd(i))%DG(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DGbcd(i)=Grids(topos(l)%nbd(i))%DE(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DFbcd(i)=-Grids(topos(l)%nbd(i))%DF(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%dvd(i)=Grids(topos(l)%nbd(i))%DE1(topos(l)%Iindexdd(i),topos(l)%Jindexd(i))
   else if(topos(l)%topod(i)=='u') then
    Boundatas(l)%G2bcd(i)=Forces(topos(l)%nbd(i))%G2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%A2bcd(i)=Forces(topos(l)%nbd(i))%A2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DEbcd(i)=Grids(topos(l)%nbd(i))%DE(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DGbcd(i)=Grids(topos(l)%nbd(i))%DG(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DFbcd(i)=Grids(topos(l)%nbd(i))%DF(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%dvd(i)=Grids(topos(l)%nbd(i))%DG2(topos(l)%Iindexd(i),topos(l)%Jindexdd(i))
   else if(topos(l)%topod(i)=='r') then
    Boundatas(l)%G2bcd(i)=-Forces(topos(l)%nbd(i))%G1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%A2bcd(i)=-Forces(topos(l)%nbd(i))%A1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DEbcd(i)=Grids(topos(l)%nbd(i))%DG(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DGbcd(i)=Grids(topos(l)%nbd(i))%DE(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DFbcd(i)=-Grids(topos(l)%nbd(i))%DF(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%dvd(i)=Grids(topos(l)%nbd(i))%DE1(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
   else if(topos(l)%topod(i)=='d') then
    Boundatas(l)%G2bcd(i)=-Forces(topos(l)%nbd(i))%G2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%A2bcd(i)=-Forces(topos(l)%nbd(i))%A2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DEbcd(i)=Grids(topos(l)%nbd(i))%DE(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DGbcd(i)=Grids(topos(l)%nbd(i))%DG(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%DFbcd(i)=Grids(topos(l)%nbd(i))%DF(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
    Boundatas(l)%dvd(i)=Grids(topos(l)%nbd(i))%DG2(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
   end if
  else
   Boundatas(l)%Xbcd(i)=Grids(l)%X(i,Jg(l))
   Boundatas(l)%Ybcd(i)=Grids(l)%Y(i,Jg(l))
   Boundatas(l)%Zbcd(i)=-Grids(l)%Z(i,Jg(l))
   Boundatas(l)%Pbcd(i)=Forces(l)%P(i,Jg(l))
   Boundatas(l)%G2bcd(i)=-Forces(l)%G2(i,Jg(l))
   Boundatas(l)%A2bcd(i)=-Forces(l)%A2(i,Jg(l))
   Boundatas(l)%DEbcd(i)=Grids(l)%DE(i,Jg(l))
   Boundatas(l)%DGbcd(i)=Grids(l)%DG(i,Jg(l))
   Boundatas(l)%DFbcd(i)=-Grids(l)%DF(i,Jg(l))
   Boundatas(l)%dvd(i)=Grids(l)%DG2(i,Jg(l))
  end if
 end DO
end DO
DO l=1,block
 DO j=1,Jn(l)
  if(topos(l)%topopl(j)/='n') then
   Boundatas(l)%Xpbcl(j)=Grids(topos(l)%nbpl(j))%Xp(topos(l)%Ipindexl(j),topos(l)%Jpindexl(j))
   Boundatas(l)%Ypbcl(j)=Grids(topos(l)%nbpl(j))%Yp(topos(l)%Ipindexl(j),topos(l)%Jpindexl(j))
   Boundatas(l)%Zpbcl(j)=Grids(topos(l)%nbpl(j))%Zp(topos(l)%Ipindexl(j),topos(l)%Jpindexl(j))
  else
   Boundatas(l)%Xpbcl(j)=Grids(l)%Xp(2,j)
   Boundatas(l)%Ypbcl(j)=Grids(l)%Yp(2,j)
   Boundatas(l)%Zpbcl(j)=-Grids(l)%Zp(2,j)
  end if
 end DO
end DO
DO l=1,block
 DO j=1,Jn(l)
  if(topos(l)%topopr(j)/='n') then
   Boundatas(l)%Xpbcr(j)=Grids(topos(l)%nbpr(j))%Xp(topos(l)%Ipindexr(j),topos(l)%Jpindexr(j))
   Boundatas(l)%Ypbcr(j)=Grids(topos(l)%nbpr(j))%Yp(topos(l)%Ipindexr(j),topos(l)%Jpindexr(j))
   Boundatas(l)%Zpbcr(j)=Grids(topos(l)%nbpr(j))%Zp(topos(l)%Ipindexr(j),topos(l)%Jpindexr(j))
  else
   Boundatas(l)%Xpbcr(j)=Grids(l)%Xp(Ig(l),j)
   Boundatas(l)%Ypbcr(j)=Grids(l)%Yp(Ig(l),j)
   Boundatas(l)%Zpbcr(j)=-Grids(l)%Zp(Ig(l),j)
  end if
 end DO
end DO
DO l=1,block
 DO i=1,In(l)
  if(topos(l)%topopu(i)/='n') then
   Boundatas(l)%Xpbcu(i)=Grids(topos(l)%nbpu(i))%Xp(topos(l)%Ipindexu(i),topos(l)%Jpindexu(i))
   Boundatas(l)%Ypbcu(i)=Grids(topos(l)%nbpu(i))%Yp(topos(l)%Ipindexu(i),topos(l)%Jpindexu(i))
   Boundatas(l)%Zpbcu(i)=Grids(topos(l)%nbpu(i))%Zp(topos(l)%Ipindexu(i),topos(l)%Jpindexu(i))
  else
   Boundatas(l)%Xpbcu(i)=Grids(l)%Xp(i,2)
   Boundatas(l)%Ypbcu(i)=Grids(l)%Yp(i,2)
   Boundatas(l)%Zpbcu(i)=-Grids(l)%Zp(i,2)
  end if
 end DO
end DO
DO l=1,block
 DO i=1,In(l)
  if(topos(l)%topopd(i)/='n') then
   Boundatas(l)%Xpbcd(i)=Grids(topos(l)%nbpd(i))%Xp(topos(l)%Ipindexd(i),topos(l)%Jpindexd(i))
   Boundatas(l)%Ypbcd(i)=Grids(topos(l)%nbpd(i))%Yp(topos(l)%Ipindexd(i),topos(l)%Jpindexd(i))
   Boundatas(l)%Zpbcd(i)=Grids(topos(l)%nbpd(i))%Zp(topos(l)%Ipindexd(i),topos(l)%Jpindexd(i))
  else
   Boundatas(l)%Xpbcd(i)=Grids(l)%Xp(i,Jg(l))
   Boundatas(l)%Ypbcd(i)=Grids(l)%Yp(i,Jg(l))
   Boundatas(l)%Zpbcd(i)=-Grids(l)%Zp(i,Jg(l))
  end if
 end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  Grids(l)%DE1(1,j)=0.5*(Grids(l)%DE(1,j)+Boundatas(l)%DEbcl(j))
  Grids(l)%DE1(In(l),j)=0.5*(Grids(l)%DE(Ig(l),j)+Boundatas(l)%DEbcr(j))
  Grids(l)%DG1(1,j)=0.5*(Grids(l)%DG(1,j)+Boundatas(l)%DGbcl(j))
  Grids(l)%DG1(In(l),j)=0.5*(Grids(l)%DG(Ig(l),j)+Boundatas(l)%DGbcr(j))
  Grids(l)%DF1(1,j)=0.5*(Grids(l)%DF(1,j)+Boundatas(l)%DFbcl(j))
  Grids(l)%DF1(In(l),j)=0.5*(Grids(l)%DF(Ig(l),j)+Boundatas(l)%DFbcr(j))
 end DO
 DO i=1,Ig(l)
  Grids(l)%DE2(i,1)=0.5*(Grids(l)%DE(i,1)+Boundatas(l)%DEbcu(i))
  Grids(l)%DE2(i,Jn(l))=0.5*(Grids(l)%DE(i,Jg(l))+Boundatas(l)%DEbcd(i))
  Grids(l)%DG2(i,1)=0.5*(Grids(l)%DG(i,1)+Boundatas(l)%DGbcu(i))
  Grids(l)%DG2(i,Jn(l))=0.5*(Grids(l)%DG(i,Jg(l))+Boundatas(l)%DGbcd(i))
  Grids(l)%DF2(i,1)=0.5*(Grids(l)%DF(i,1)+Boundatas(l)%DFbcu(i))
  Grids(l)%DF2(i,Jn(l))=0.5*(Grids(l)%DF(i,Jg(l))+Boundatas(l)%DFbcd(i))
 end DO
end DO
end Subroutine BCtransfer
