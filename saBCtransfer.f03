Subroutine saBCtransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topol(j)=='r') then
  Boundatas(l)%saxl(j)=Fluxs(topos(l)%nbl(j))%sax(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  else if(topos(l)%topol(j)=='d') then
  Boundatas(l)%saxl(j)=Fluxs(topos(l)%nbl(j))%say(topos(l)%Iindexl(j),topos(l)%Jindexl(j))
  else if(topos(l)%topol(j)=='l') then
  Boundatas(l)%saxl(j)=-Fluxs(topos(l)%nbl(j))%sax(topos(l)%Iindexll(j),topos(l)%Jindexl(j))
  else if(topos(l)%topol(j)=='u') then
  Boundatas(l)%saxl(j)=-Fluxs(topos(l)%nbl(j))%say(topos(l)%Iindexl(j),topos(l)%Jindexll(j))
  else
  Boundatas(l)%saxl(j)=-Fluxs(l)%sax(2,j)
  end if
  end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  if(topos(l)%topor(j)=='l') then
  Boundatas(l)%saxr(j)=Fluxs(topos(l)%nbr(j))%sax(topos(l)%Iindexrr(j),topos(l)%Jindexr(j))
  else if(topos(l)%topor(j)=='u') then
  Boundatas(l)%saxr(j)=Fluxs(topos(l)%nbr(j))%say(topos(l)%Iindexr(j),topos(l)%Jindexrr(j))
  else if(topos(l)%topor(j)=='r') then
  Boundatas(l)%saxr(j)=-Fluxs(topos(l)%nbr(j))%sax(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  else if(topos(l)%topor(j)=='d') then
  Boundatas(l)%saxr(j)=-Fluxs(topos(l)%nbr(j))%say(topos(l)%Iindexr(j),topos(l)%Jindexr(j))
  else
  Boundatas(l)%saxr(j)=-Fluxs(l)%sax(Ig(l),j)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos(l)%topou(i)=='r') then
  Boundatas(l)%sayu(i)=Fluxs(topos(l)%nbu(i))%sax(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  else if(topos(l)%topou(i)=='d') then
  Boundatas(l)%sayu(i)=Fluxs(topos(l)%nbu(i))%say(topos(l)%Iindexu(i),topos(l)%Jindexu(i))
  else if(topos(l)%topou(i)=='l') then
  Boundatas(l)%sayu(i)=-Fluxs(topos(l)%nbu(i))%sax(topos(l)%Iindexuu(i),topos(l)%Jindexu(i))
  else if(topos(l)%topou(i)=='u') then
  Boundatas(l)%sayu(i)=-Fluxs(topos(l)%nbu(i))%say(topos(l)%Iindexu(i),topos(l)%Jindexuu(i))
  else
  Boundatas(l)%sayu(i)=-Fluxs(l)%say(i,1)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos(l)%topod(i)=='l') then
  Boundatas(l)%sayd(i)=Fluxs(topos(l)%nbd(i))%sax(topos(l)%Iindexdd(i),topos(l)%Jindexd(i))
  else if(topos(l)%topod(i)=='u') then
  Boundatas(l)%sayd(i)=Fluxs(topos(l)%nbd(i))%say(topos(l)%Iindexd(i),topos(l)%Jindexdd(i))
  else if(topos(l)%topod(i)=='r') then
  Boundatas(l)%sayd(i)=-Fluxs(topos(l)%nbd(i))%sax(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  else if(topos(l)%topod(i)=='d') then
  Boundatas(l)%sayd(i)=-Fluxs(topos(l)%nbd(i))%say(topos(l)%Iindexd(i),topos(l)%Jindexd(i))
  else
  Boundatas(l)%sayd(i)=-Fluxs(l)%say(i,Jg(l))
  end if
  end DO
end DO
end Subroutine saBCtransfer
