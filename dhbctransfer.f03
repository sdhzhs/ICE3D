Subroutine dhbctransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topol(j)/='n') then
  Boundatas(l)%dhbcl(j)=Imps(topos(l)%nbl(j))%dh(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  else
  Boundatas(l)%dhbcl(j)=Imps(l)%dh(1,j)
  end if
  end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topor(j)/='n') then
  Boundatas(l)%dhbcr(j)=Imps(topos(l)%nbr(j))%dh(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  else
  Boundatas(l)%dhbcr(j)=Imps(l)%dh(Ig(l),j)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos(l)%topou(i)/='n') then
  Boundatas(l)%dhbcu(i)=Imps(topos(l)%nbu(i))%dh(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  else
  Boundatas(l)%dhbcu(i)=Imps(l)%dh(i,1)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos(l)%topod(i)/='n') then
  Boundatas(l)%dhbcd(i)=Imps(topos(l)%nbd(i))%dh(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  else
  Boundatas(l)%dhbcd(i)=Imps(l)%dh(i,Jg(l))
  end if
  end DO
end DO
end Subroutine dhbctransfer
