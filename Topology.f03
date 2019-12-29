Subroutine Topology
use COM
implicit none
integer i,j,k,l,m,n
real(8)::tol=1e-8
DO l=1,block
topos(l)%nbl=0
topos(l)%nbr=0
topos(l)%nbu=0
topos(l)%nbd=0
topos(l)%topol='n'
topos(l)%topor='n'
topos(l)%topou='n'
topos(l)%topod='n'
topos(l)%nbpl=0
topos(l)%nbpr=0
topos(l)%nbpu=0
topos(l)%nbpd=0
topos(l)%topopl='n'
topos(l)%topopr='n'
topos(l)%topopu='n'
topos(l)%topopd='n'
end DO
DO l=1,block
 DO j=1,Jg(l)
  DO k=1,block
  DO n=1,Jg(k)
  if(abs(Grids(k)%Xp(In(k),n)-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Xp(In(k),n+1)-Grids(l)%Xp(1,j+1))<tol.and.&
  abs(Grids(k)%Yp(In(k),n)-Grids(l)%Yp(1,j))<tol.and.abs(Grids(k)%Yp(In(k),n+1)-Grids(l)%Yp(1,j+1))<tol.and.&
  abs(Grids(k)%Zp(In(k),n)-Grids(l)%Zp(1,j))<tol.and.abs(Grids(k)%Zp(In(k),n+1)-Grids(l)%Zp(1,j+1))<tol) then
  topos(l)%nbl(j)=k
  topos(l)%topol(j)='r'
  topos(l)%Iindexl(j)=Ig(k)
  topos(l)%Jindexl(j)=n
  topos(l)%Iindexll(j)=Ig(k)-1
  topos(l)%Jindexll(j)=n
  end if
  if(abs(Grids(k)%Xp(1,n+1)-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Xp(1,n)-Grids(l)%Xp(1,j+1))<tol.and.&
  abs(Grids(k)%Yp(1,n+1)-Grids(l)%Yp(1,j))<tol.and.abs(Grids(k)%Yp(1,n)-Grids(l)%Yp(1,j+1))<tol.and.&
  abs(Grids(k)%Zp(1,n+1)-Grids(l)%Zp(1,j))<tol.and.abs(Grids(k)%Zp(1,n)-Grids(l)%Zp(1,j+1))<tol) then
  topos(l)%nbl(j)=k
  topos(l)%topol(j)='l'
  topos(l)%Iindexl(j)=1
  topos(l)%Jindexl(j)=n
  topos(l)%Iindexll(j)=2
  topos(l)%Jindexll(j)=n
  end if
  if(abs(Grids(k)%Xp(In(k),n+1)-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Xp(In(k),n)-Grids(l)%Xp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Yp(In(k),n+1)-Grids(l)%Yp(In(l),j))<tol.and.abs(Grids(k)%Yp(In(k),n)-Grids(l)%Yp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Zp(In(k),n+1)-Grids(l)%Zp(In(l),j))<tol.and.abs(Grids(k)%Zp(In(k),n)-Grids(l)%Zp(In(l),j+1))<tol) then
  topos(l)%nbr(j)=k
  topos(l)%topor(j)='r'
  topos(l)%Iindexr(j)=Ig(k)
  topos(l)%Jindexr(j)=n
  topos(l)%Iindexrr(j)=Ig(k)-1
  topos(l)%Jindexrr(j)=n
  end if
  if(abs(Grids(k)%Xp(1,n)-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Xp(1,n+1)-Grids(l)%Xp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Yp(1,n)-Grids(l)%Yp(In(l),j))<tol.and.abs(Grids(k)%Yp(1,n+1)-Grids(l)%Yp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Zp(1,n)-Grids(l)%Zp(In(l),j))<tol.and.abs(Grids(k)%Zp(1,n+1)-Grids(l)%Zp(In(l),j+1))<tol) then
  topos(l)%nbr(j)=k
  topos(l)%topor(j)='l'
  topos(l)%Iindexr(j)=1
  topos(l)%Jindexr(j)=n
  topos(l)%Iindexrr(j)=2
  topos(l)%Jindexrr(j)=n
  end if
  end DO
  DO m=1,Ig(k)
  if(abs(Grids(k)%Xp(m+1,Jn(k))-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(1,j+1))<tol.and.&
  abs(Grids(k)%Yp(m+1,Jn(k))-Grids(l)%Yp(1,j))<tol.and.abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(1,j+1))<tol.and.&
  abs(Grids(k)%Zp(m+1,Jn(k))-Grids(l)%Zp(1,j))<tol.and.abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(1,j+1))<tol) then
  topos(l)%nbl(j)=k
  topos(l)%topol(j)='d'
  topos(l)%Iindexl(j)=m
  topos(l)%Jindexl(j)=Jg(k)
  topos(l)%Iindexll(j)=m
  topos(l)%Jindexll(j)=Jg(k)-1
  end if
  if(abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Xp(m+1,1)-Grids(l)%Xp(1,j+1))<tol.and.&
  abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(1,j))<tol.and.abs(Grids(k)%Yp(m+1,1)-Grids(l)%Yp(1,j+1))<tol.and.&
  abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(1,j))<tol.and.abs(Grids(k)%Zp(m+1,1)-Grids(l)%Zp(1,j+1))<tol) then
  topos(l)%nbl(j)=k
  topos(l)%topol(j)='u'
  topos(l)%Iindexl(j)=m
  topos(l)%Jindexl(j)=1
  topos(l)%Iindexll(j)=m
  topos(l)%Jindexll(j)=2
  end if
  if(abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Xp(m+1,Jn(k))-Grids(l)%Xp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(In(l),j))<tol.and.abs(Grids(k)%Yp(m+1,Jn(k))-Grids(l)%Yp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(In(l),j))<tol.and.abs(Grids(k)%Zp(m+1,Jn(k))-Grids(l)%Zp(In(l),j+1))<tol) then
  topos(l)%nbr(j)=k
  topos(l)%topor(j)='d'
  topos(l)%Iindexr(j)=m
  topos(l)%Jindexr(j)=Jg(k)
  topos(l)%Iindexrr(j)=m
  topos(l)%Jindexrr(j)=Jg(k)-1
  end if
  if(abs(Grids(k)%Xp(m+1,1)-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Yp(m+1,1)-Grids(l)%Yp(In(l),j))<tol.and.abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(In(l),j+1))<tol.and.&
  abs(Grids(k)%Zp(m+1,1)-Grids(l)%Zp(In(l),j))<tol.and.abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(In(l),j+1))<tol) then
  topos(l)%nbr(j)=k
  topos(l)%topor(j)='u'
  topos(l)%Iindexr(j)=m
  topos(l)%Jindexr(j)=1
  topos(l)%Iindexrr(j)=m
  topos(l)%Jindexrr(j)=2
  end if
  end DO
  end DO
 end DO
 DO i=1,Ig(l)
  DO k=1,block
  DO n=1,Jg(k)
  if(abs(Grids(k)%Xp(In(k),n+1)-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Xp(In(k),n)-Grids(l)%Xp(i+1,1))<tol.and.&
  abs(Grids(k)%Yp(In(k),n+1)-Grids(l)%Yp(i,1))<tol.and.abs(Grids(k)%Yp(In(k),n)-Grids(l)%Yp(i+1,1))<tol.and.&
  abs(Grids(k)%Zp(In(k),n+1)-Grids(l)%Zp(i,1))<tol.and.abs(Grids(k)%Zp(In(k),n)-Grids(l)%Zp(i+1,1))<tol) then
  topos(l)%nbu(i)=k
  topos(l)%topou(i)='r'
  topos(l)%Iindexu(i)=Ig(k)
  topos(l)%Jindexu(i)=n
  topos(l)%Iindexuu(i)=Ig(k)-1
  topos(l)%Jindexuu(i)=n
  end if
  if(abs(Grids(k)%Xp(1,n)-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Xp(1,n+1)-Grids(l)%Xp(i+1,1))<tol.and.&
  abs(Grids(k)%Yp(1,n)-Grids(l)%Yp(i,1))<tol.and.abs(Grids(k)%Yp(1,n+1)-Grids(l)%Yp(i+1,1))<tol.and.&
  abs(Grids(k)%Zp(1,n)-Grids(l)%Zp(i,1))<tol.and.abs(Grids(k)%Zp(1,n+1)-Grids(l)%Zp(i+1,1))<tol) then
  topos(l)%nbu(i)=k
  topos(l)%topou(i)='l'
  topos(l)%Iindexu(i)=1
  topos(l)%Jindexu(i)=n
  topos(l)%Iindexuu(i)=2
  topos(l)%Jindexuu(i)=n
  end if
  if(abs(Grids(k)%Xp(In(k),n)-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Xp(In(k),n+1)-Grids(l)%Xp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(In(k),n)-Grids(l)%Yp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),n+1)-Grids(l)%Yp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),n)-Grids(l)%Zp(i,Jn(l)))<tol.and.abs(Grids(k)%Zp(In(k),n+1)-Grids(l)%Zp(i+1,Jn(l)))<tol) then
  topos(l)%nbd(i)=k
  topos(l)%topod(i)='r'
  topos(l)%Iindexd(i)=Ig(k)
  topos(l)%Jindexd(i)=n
  topos(l)%Iindexdd(i)=Ig(k)-1
  topos(l)%Jindexdd(i)=n
  end if
  if(abs(Grids(k)%Xp(1,n+1)-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Xp(1,n)-Grids(l)%Xp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(1,n+1)-Grids(l)%Yp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,n)-Grids(l)%Yp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,n+1)-Grids(l)%Zp(i,Jn(l)))<tol.and.abs(Grids(k)%Zp(1,n)-Grids(l)%Zp(i+1,Jn(l)))<tol) then
  topos(l)%nbd(i)=k
  topos(l)%topod(i)='l'
  topos(l)%Iindexd(i)=1
  topos(l)%Jindexd(i)=n
  topos(l)%Iindexdd(i)=2
  topos(l)%Jindexdd(i)=n
  end if
  end DO
  DO m=1,Ig(k)
  if(abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Xp(m+1,Jn(k))-Grids(l)%Xp(i+1,1))<tol.and.&
  abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(i,1))<tol.and.abs(Grids(k)%Yp(m+1,Jn(k))-Grids(l)%Yp(i+1,1))<tol.and.&
  abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(i,1))<tol.and.abs(Grids(k)%Zp(m+1,Jn(k))-Grids(l)%Zp(i+1,1))<tol) then
  topos(l)%nbu(i)=k
  topos(l)%topou(i)='d'
  topos(l)%Iindexu(i)=m
  topos(l)%Jindexu(i)=Jg(k)
  topos(l)%Iindexuu(i)=m
  topos(l)%Jindexuu(i)=Jg(k)-1
  end if
  if(abs(Grids(k)%Xp(m+1,1)-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(i+1,1))<tol.and.&
  abs(Grids(k)%Yp(m+1,1)-Grids(l)%Yp(i,1))<tol.and.abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(i+1,1))<tol.and.&
  abs(Grids(k)%Zp(m+1,1)-Grids(l)%Zp(i,1))<tol.and.abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(i+1,1))<tol) then
  topos(l)%nbu(i)=k
  topos(l)%topou(i)='u'
  topos(l)%Iindexu(i)=m
  topos(l)%Jindexu(i)=1
  topos(l)%Iindexuu(i)=m
  topos(l)%Jindexuu(i)=2
  end if
  if(abs(Grids(k)%Xp(m+1,Jn(k))-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(m+1,Jn(k))-Grids(l)%Yp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(m+1,Jn(k))-Grids(l)%Zp(i,Jn(l)))<tol.and.abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(i+1,Jn(l)))<tol) then
  topos(l)%nbd(i)=k
  topos(l)%topod(i)='d'
  topos(l)%Iindexd(i)=m
  topos(l)%Jindexd(i)=Jg(k)
  topos(l)%Iindexdd(i)=m
  topos(l)%Jindexdd(i)=Jg(k)-1
  end if
  if(abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Xp(m+1,1)-Grids(l)%Xp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(m+1,1)-Grids(l)%Yp(i+1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(i,Jn(l)))<tol.and.abs(Grids(k)%Zp(m+1,1)-Grids(l)%Zp(i+1,Jn(l)))<tol) then
  topos(l)%nbd(i)=k
  topos(l)%topod(i)='u'
  topos(l)%Iindexd(i)=m
  topos(l)%Jindexd(i)=1
  topos(l)%Iindexdd(i)=m
  topos(l)%Jindexdd(i)=2
  end if
  end DO
  end DO
 end DO
end DO
DO l=1,block
 DO j=1,Jn(l)
  if(j==Jn(l)) then
   topos(l)%nbpl(j)=topos(l)%nbl(j-1)
   topos(l)%topopl(j)=topos(l)%topol(j-1)
   topos(l)%nbpr(j)=topos(l)%nbr(j-1)
   topos(l)%topopr(j)=topos(l)%topor(j-1)
  else
   topos(l)%nbpl(j)=topos(l)%nbl(j)
   topos(l)%topopl(j)=topos(l)%topol(j)
   topos(l)%nbpr(j)=topos(l)%nbr(j)
   topos(l)%topopr(j)=topos(l)%topor(j)
  end if
 end DO
 DO i=1,In(l)
  if(i==In(l)) then
   topos(l)%nbpu(i)=topos(l)%nbu(i-1)
   topos(l)%topopu(i)=topos(l)%topou(i-1)
   topos(l)%nbpd(i)=topos(l)%nbd(i-1)
   topos(l)%topopd(i)=topos(l)%topod(i-1)
  else
   topos(l)%nbpu(i)=topos(l)%nbu(i)
   topos(l)%topopu(i)=topos(l)%topou(i)
   topos(l)%nbpd(i)=topos(l)%nbd(i)
   topos(l)%topopd(i)=topos(l)%topod(i)
  end if
 end DO
end DO
DO l=1,block
 DO j=1,Jn(l)
  if(topos(l)%topopl(j)=='r') then
  if(j==Jn(l)) then
  topos(l)%Ipindexl(j)=Ig(topos(l)%nbpl(j))
  topos(l)%Jpindexl(j)=topos(l)%Jindexl(j-1)+1
  else
  topos(l)%Ipindexl(j)=Ig(topos(l)%nbpl(j))
  topos(l)%Jpindexl(j)=topos(l)%Jindexl(j)
  end if
  else if(topos(l)%topopl(j)=='d') then
  if(j==Jn(l)) then
  topos(l)%Ipindexl(j)=topos(l)%Iindexl(j-1)
  topos(l)%Jpindexl(j)=Jg(topos(l)%nbpl(j))
  else
  topos(l)%Ipindexl(j)=topos(l)%Iindexl(j)+1
  topos(l)%Jpindexl(j)=Jg(topos(l)%nbpl(j))
  end if
  else if(topos(l)%topopl(j)=='l') then
  if(j==Jn(l)) then
  topos(l)%Ipindexl(j)=2
  topos(l)%Jpindexl(j)=topos(l)%Jindexl(j-1)
  else
  topos(l)%Ipindexl(j)=2
  topos(l)%Jpindexl(j)=topos(l)%Jindexl(j)+1
  end if
  else if(topos(l)%topopl(j)=='u') then
  if(j==Jn(l)) then
  topos(l)%Ipindexl(j)=topos(l)%Iindexl(j-1)+1
  topos(l)%Jpindexl(j)=2
  else
  topos(l)%Ipindexl(j)=topos(l)%Iindexl(j)
  topos(l)%Jpindexl(j)=2
  end if
  end if
  if(topos(l)%topopr(j)=='l') then
  if(j==Jn(l)) then
  topos(l)%Ipindexr(j)=2
  topos(l)%Jpindexr(j)=topos(l)%Jindexr(j-1)+1
  else
  topos(l)%Ipindexr(j)=2
  topos(l)%Jpindexr(j)=topos(l)%Jindexr(j)
  end if
  else if(topos(l)%topopr(j)=='u') then
  if(j==Jn(l)) then
  topos(l)%Ipindexr(j)=topos(l)%Iindexr(j-1)
  topos(l)%Jpindexr(j)=2
  else
  topos(l)%Ipindexr(j)=topos(l)%Iindexr(j)+1
  topos(l)%Jpindexr(j)=2
  end if
  else if(topos(l)%topopr(j)=='r') then
  if(j==Jn(l)) then
  topos(l)%Ipindexr(j)=Ig(topos(l)%nbpr(j))
  topos(l)%Jpindexr(j)=topos(l)%Jindexr(j-1)
  else
  topos(l)%Ipindexr(j)=Ig(topos(l)%nbpr(j))
  topos(l)%Jpindexr(j)=topos(l)%Jindexr(j)+1
  end if
  else if(topos(l)%topopr(j)=='d') then
  if(j==Jn(l)) then
  topos(l)%Ipindexr(j)=topos(l)%Iindexr(j-1)+1
  topos(l)%Jpindexr(j)=Jg(topos(l)%nbpr(j))
  else
  topos(l)%Ipindexr(j)=topos(l)%Iindexr(j)
  topos(l)%Jpindexr(j)=Jg(topos(l)%nbpr(j))
  end if
  end if
 end DO
 DO i=1,In(l)
  if(topos(l)%topopu(i)=='r') then
  if(i==In(l)) then
  topos(l)%Ipindexu(i)=Ig(topos(l)%nbpu(i))
  topos(l)%Jpindexu(i)=topos(l)%Jindexu(i-1)
  else
  topos(l)%Ipindexu(i)=Ig(topos(l)%nbpu(i))
  topos(l)%Jpindexu(i)=topos(l)%Jindexu(i)+1
  end if
  else if(topos(l)%topopu(i)=='d') then
  if(i==In(l)) then
  topos(l)%Ipindexu(i)=topos(l)%Iindexu(i-1)+1
  topos(l)%Jpindexu(i)=Jg(topos(l)%nbpu(i))
  else
  topos(l)%Ipindexu(i)=topos(l)%Iindexu(i)
  topos(l)%Jpindexu(i)=Jg(topos(l)%nbpu(i))
  end if
  else if(topos(l)%topopu(i)=='l') then
  if(i==In(l)) then
  topos(l)%Ipindexu(i)=2
  topos(l)%Jpindexu(i)=topos(l)%Jindexu(i-1)+1
  else
  topos(l)%Ipindexu(i)=2
  topos(l)%Jpindexu(i)=topos(l)%Jindexu(i)
  end if
  else if(topos(l)%topopu(i)=='u') then
  if(i==In(l)) then
  topos(l)%Ipindexu(i)=topos(l)%Iindexu(i-1)
  topos(l)%Jpindexu(i)=2
  else
  topos(l)%Ipindexu(i)=topos(l)%Iindexu(i)+1
  topos(l)%Jpindexu(i)=2
  end if
  end if
  if(topos(l)%topopd(i)=='l') then
  if(i==In(l)) then
  topos(l)%Ipindexd(i)=2
  topos(l)%Jpindexd(i)=topos(l)%Jindexd(i-1)
  else
  topos(l)%Ipindexd(i)=2
  topos(l)%Jpindexd(i)=topos(l)%Jindexd(i)+1
  end if
  else if(topos(l)%topopd(i)=='u') then
  if(i==In(l)) then
  topos(l)%Ipindexd(i)=topos(l)%Iindexd(i-1)+1
  topos(l)%Jpindexd(i)=2
  else
  topos(l)%Ipindexd(i)=topos(l)%Iindexd(i)
  topos(l)%Jpindexd(i)=2
  end if
  else if(topos(l)%topopd(i)=='r') then
  if(i==In(l)) then
  topos(l)%Ipindexd(i)=Ig(topos(l)%nbpd(i))
  topos(l)%Jpindexd(i)=topos(l)%Jindexd(i-1)+1
  else
  topos(l)%Ipindexd(i)=Ig(topos(l)%nbpd(i))
  topos(l)%Jpindexd(i)=topos(l)%Jindexd(i)
  end if
  else if(topos(l)%topopd(i)=='d') then
  if(i==In(l)) then
  topos(l)%Ipindexd(i)=topos(l)%Iindexd(i-1)
  topos(l)%Jpindexd(i)=Jg(topos(l)%nbpd(i))
  else
  topos(l)%Ipindexd(i)=topos(l)%Iindexd(i)+1
  topos(l)%Jpindexd(i)=Jg(topos(l)%nbpd(i))
  end if
  end if
 end DO
end DO
end Subroutine Topology
