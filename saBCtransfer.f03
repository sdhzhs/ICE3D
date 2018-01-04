Subroutine saBCtransfer
use COM
implicit none
integer i,j,l
DO l=1,block
 DO j=1,Jg(l)
  if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
  Boundatas(l)%saxl(j)=Fluxs(topos%nbl(l))%sax(Ig(topos%nbl(l)),j)
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
  Boundatas(l)%saxl(j)=Fluxs(topos%nbl(l))%sax(Ig(topos%nbl(l)),Jg(l)+1-j)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
  Boundatas(l)%saxl(j)=Fluxs(topos%nbl(l))%say(Ig(topos%nbl(l))+1-j,Jg(topos%nbl(l)))
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
  Boundatas(l)%saxl(j)=Fluxs(topos%nbl(l))%say(j,Jg(topos%nbl(l)))
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
  Boundatas(l)%saxl(j)=-Fluxs(topos%nbl(l))%sax(2,Jg(topos%nbl(l))+1-j)
  else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
  Boundatas(l)%saxl(j)=-Fluxs(topos%nbl(l))%sax(2,j)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
  Boundatas(l)%saxl(j)=-Fluxs(topos%nbl(l))%say(Ig(topos%nbl(l))+1-j,2)
  else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
  Boundatas(l)%saxl(j)=-Fluxs(topos%nbl(l))%say(j,2)
  else
  Boundatas(l)%saxl(j)=-Fluxs(l)%sax(2,j)
  end if
  end DO
end DO
DO l=1,block
 DO j=1,Jg(l)
  if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
  Boundatas(l)%saxr(j)=Fluxs(topos%nbr(l))%sax(2,j)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
  Boundatas(l)%saxr(j)=Fluxs(topos%nbr(l))%sax(2,Jg(l)+1-j)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
  Boundatas(l)%saxr(j)=Fluxs(topos%nbr(l))%say(Ig(topos%nbr(l))+1-j,2)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
  Boundatas(l)%saxr(j)=Fluxs(topos%nbr(l))%say(j,2)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
  Boundatas(l)%saxr(j)=-Fluxs(topos%nbr(l))%sax(Ig(topos%nbr(l)),Jg(topos%nbr(l))+1-j)
  else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
  Boundatas(l)%saxr(j)=-Fluxs(topos%nbr(l))%sax(Ig(topos%nbr(l)),j)
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
  Boundatas(l)%saxr(j)=-Fluxs(topos%nbr(l))%say(Ig(topos%nbr(l))+1-j,Jg(topos%nbr(l)))
  else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
  Boundatas(l)%saxr(j)=-Fluxs(topos%nbr(l))%say(j,Jg(topos%nbr(l)))
  else
  Boundatas(l)%saxr(j)=-Fluxs(l)%sax(Ig(l),j)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
  Boundatas(l)%sayu(i)=Fluxs(topos%nbu(l))%sax(Ig(topos%nbu(l)),i)
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
  Boundatas(l)%sayu(i)=Fluxs(topos%nbu(l))%sax(Ig(topos%nbu(l)),Jg(topos%nbu(l))+1-i)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
  Boundatas(l)%sayu(i)=Fluxs(topos%nbu(l))%say(Ig(topos%nbu(l))+1-i,Jg(topos%nbu(l)))
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
  Boundatas(l)%sayu(i)=Fluxs(topos%nbu(l))%say(i,Jg(topos%nbu(l)))
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
  Boundatas(l)%sayu(i)=-Fluxs(topos%nbu(l))%sax(2,Jg(topos%nbu(l))+1-i)
  else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
  Boundatas(l)%sayu(i)=-Fluxs(topos%nbu(l))%sax(2,i)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
  Boundatas(l)%sayu(i)=-Fluxs(topos%nbu(l))%say(Ig(topos%nbu(l))+1-i,2)
  else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
  Boundatas(l)%sayu(i)=-Fluxs(topos%nbu(l))%say(i,2)
  else
  Boundatas(l)%sayu(i)=-Fluxs(l)%say(i,1)
  end if
  end DO
end DO
DO l=1,block
 DO i=1,Ig(l)
  if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
  Boundatas(l)%sayd(i)=Fluxs(topos%nbd(l))%sax(2,i)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
  Boundatas(l)%sayd(i)=Fluxs(topos%nbd(l))%sax(2,Jg(topos%nbd(l))+1-i)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
  Boundatas(l)%sayd(i)=Fluxs(topos%nbd(l))%say(Ig(topos%nbd(l))+1-i,2)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
  Boundatas(l)%sayd(i)=Fluxs(topos%nbd(l))%say(i,2)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
  Boundatas(l)%sayd(i)=-Fluxs(topos%nbd(l))%sax(Ig(topos%nbd(l)),Jg(topos%nbd(l))+1-i)
  else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
  Boundatas(l)%sayd(i)=-Fluxs(topos%nbd(l))%sax(Ig(topos%nbd(l)),i)
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
  Boundatas(l)%sayd(i)=-Fluxs(topos%nbd(l))%say(Ig(topos%nbd(l))+1-i,Jg(topos%nbd(l)))
  else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
  Boundatas(l)%sayd(i)=-Fluxs(topos%nbd(l))%say(i,Jg(topos%nbd(l)))
  else
  Boundatas(l)%sayd(i)=-Fluxs(l)%say(i,Jg(l))
  end if
  end DO
end DO
end Subroutine saBCtransfer
