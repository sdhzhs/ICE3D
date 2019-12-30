Subroutine Cornersmooth(l)
use COM
implicit none
integer i,j,k,l,m
integer numcorn(4)
integer numlr(2:Jg(l),2),numud(2:Ig(l),2)
real(8) sumXcorn(4),sumYcorn(4),sumZcorn(4)
real(8) sumXlr(2:Jg(l),2),sumYlr(2:Jg(l),2),sumZlr(2:Jg(l),2),sumXud(2:Ig(l),2),sumYud(2:Ig(l),2),sumZud(2:Ig(l),2)
real(8)::tol=1e-8
sumXcorn(1)=Icecoordinates(l)%Xi(1,1)
sumYcorn(1)=Icecoordinates(l)%Yi(1,1)
sumZcorn(1)=Icecoordinates(l)%Zi(1,1)
sumXcorn(2)=Icecoordinates(l)%Xi(1,Jn(l))
sumYcorn(2)=Icecoordinates(l)%Yi(1,Jn(l))
sumZcorn(2)=Icecoordinates(l)%Zi(1,Jn(l))
sumXcorn(3)=Icecoordinates(l)%Xi(In(l),1)
sumYcorn(3)=Icecoordinates(l)%Yi(In(l),1)
sumZcorn(3)=Icecoordinates(l)%Zi(In(l),1)
sumXcorn(4)=Icecoordinates(l)%Xi(In(l),Jn(l))
sumYcorn(4)=Icecoordinates(l)%Yi(In(l),Jn(l))
sumZcorn(4)=Icecoordinates(l)%Zi(In(l),Jn(l))
sumXlr(2:Jg(l),1)=Icecoordinates(l)%Xi(1,2:Jg(l))
sumYlr(2:Jg(l),1)=Icecoordinates(l)%Yi(1,2:Jg(l))
sumZlr(2:Jg(l),1)=Icecoordinates(l)%Zi(1,2:Jg(l))
sumXlr(2:Jg(l),2)=Icecoordinates(l)%Xi(In(l),2:Jg(l))
sumYlr(2:Jg(l),2)=Icecoordinates(l)%Yi(In(l),2:Jg(l))
sumZlr(2:Jg(l),2)=Icecoordinates(l)%Zi(In(l),2:Jg(l))
sumXud(2:Ig(l),1)=Icecoordinates(l)%Xi(2:Ig(l),1)
sumYud(2:Ig(l),1)=Icecoordinates(l)%Yi(2:Ig(l),1)
sumZud(2:Ig(l),1)=Icecoordinates(l)%Zi(2:Ig(l),1)
sumXud(2:Ig(l),2)=Icecoordinates(l)%Xi(2:Ig(l),Jn(l))
sumYud(2:Ig(l),2)=Icecoordinates(l)%Yi(2:Ig(l),Jn(l))
sumZud(2:Ig(l),2)=Icecoordinates(l)%Zi(2:Ig(l),Jn(l))
numcorn=1
numlr=1
numud=1
DO k=1,block
if(k/=l) then
 DO j=1,Jn(k)
  if(abs(Grids(k)%Xp(In(k),j)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Yp(In(k),j)-Grids(l)%Yp(1,1))<tol.and.&
  abs(Grids(k)%Zp(In(k),j)-Grids(l)%Zp(1,1))<tol) then
  sumXcorn(1)=sumXcorn(1)+Icecoordinates(k)%Xi(In(k),j)
  sumYcorn(1)=sumYcorn(1)+Icecoordinates(k)%Yi(In(k),j)
  sumZcorn(1)=sumZcorn(1)+Icecoordinates(k)%Zi(In(k),j)
  numcorn(1)=numcorn(1)+1
  else if(abs(Grids(k)%Xp(1,j)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Yp(1,j)-Grids(l)%Yp(1,1))<tol.and.&
  abs(Grids(k)%Zp(1,j)-Grids(l)%Zp(1,1))<tol) then
  sumXcorn(1)=sumXcorn(1)+Icecoordinates(k)%Xi(1,j)
  sumYcorn(1)=sumYcorn(1)+Icecoordinates(k)%Yi(1,j)
  sumZcorn(1)=sumZcorn(1)+Icecoordinates(k)%Zi(1,j)
  numcorn(1)=numcorn(1)+1
  end if
  if(abs(Grids(k)%Xp(In(k),j)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Yp(In(k),j)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(In(k),j)-Grids(l)%Zp(In(l),1))<tol) then
  sumXcorn(3)=sumXcorn(3)+Icecoordinates(k)%Xi(In(k),j)
  sumYcorn(3)=sumYcorn(3)+Icecoordinates(k)%Yi(In(k),j)
  sumZcorn(3)=sumZcorn(3)+Icecoordinates(k)%Zi(In(k),j)
  numcorn(3)=numcorn(3)+1
  else if(abs(Grids(k)%Xp(1,j)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Yp(1,j)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(1,j)-Grids(l)%Zp(In(l),1))<tol) then
  sumXcorn(3)=sumXcorn(3)+Icecoordinates(k)%Xi(1,j)
  sumYcorn(3)=sumYcorn(3)+Icecoordinates(k)%Yi(1,j)
  sumZcorn(3)=sumZcorn(3)+Icecoordinates(k)%Zi(1,j)
  numcorn(3)=numcorn(3)+1
  end if
  if(abs(Grids(k)%Xp(In(k),j)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),j)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),j)-Grids(l)%Zp(1,Jn(l)))<tol) then
  sumXcorn(2)=sumXcorn(2)+Icecoordinates(k)%Xi(In(k),j)
  sumYcorn(2)=sumYcorn(2)+Icecoordinates(k)%Yi(In(k),j)
  sumZcorn(2)=sumZcorn(2)+Icecoordinates(k)%Zi(In(k),j)
  numcorn(2)=numcorn(2)+1
  else if(abs(Grids(k)%Xp(1,j)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,j)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,j)-Grids(l)%Zp(1,Jn(l)))<tol) then
  sumXcorn(2)=sumXcorn(2)+Icecoordinates(k)%Xi(1,j)
  sumYcorn(2)=sumYcorn(2)+Icecoordinates(k)%Yi(1,j)
  sumZcorn(2)=sumZcorn(2)+Icecoordinates(k)%Zi(1,j)
  numcorn(2)=numcorn(2)+1
  end if
  if(abs(Grids(k)%Xp(In(k),j)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),j)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),j)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  sumXcorn(4)=sumXcorn(4)+Icecoordinates(k)%Xi(In(k),j)
  sumYcorn(4)=sumYcorn(4)+Icecoordinates(k)%Yi(In(k),j)
  sumZcorn(4)=sumZcorn(4)+Icecoordinates(k)%Zi(In(k),j)
  numcorn(4)=numcorn(4)+1
  else if(abs(Grids(k)%Xp(1,j)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.abs(Grids(k)%Yp(1,j)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,j)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  sumXcorn(4)=sumXcorn(4)+Icecoordinates(k)%Xi(1,j)
  sumYcorn(4)=sumYcorn(4)+Icecoordinates(k)%Yi(1,j)
  sumZcorn(4)=sumZcorn(4)+Icecoordinates(k)%Zi(1,j)
  numcorn(4)=numcorn(4)+1
  end if
 end DO
 DO i=2,Ig(k)
  if(abs(Grids(k)%Xp(i,Jn(k))-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Yp(i,Jn(k))-Grids(l)%Yp(1,1))<tol.and.&
  abs(Grids(k)%Zp(i,Jn(k))-Grids(l)%Zp(1,1))<tol) then
  sumXcorn(1)=sumXcorn(1)+Icecoordinates(k)%Xi(i,Jn(k))
  sumYcorn(1)=sumYcorn(1)+Icecoordinates(k)%Yi(i,Jn(k))
  sumZcorn(1)=sumZcorn(1)+Icecoordinates(k)%Zi(i,Jn(k))
  numcorn(1)=numcorn(1)+1
  else if(abs(Grids(k)%Xp(i,1)-Grids(l)%Xp(1,1))<tol.and.abs(Grids(k)%Yp(i,1)-Grids(l)%Yp(1,1))<tol.and.&
  abs(Grids(k)%Zp(i,1)-Grids(l)%Zp(1,1))<tol) then
  sumXcorn(1)=sumXcorn(1)+Icecoordinates(k)%Xi(i,1)
  sumYcorn(1)=sumYcorn(1)+Icecoordinates(k)%Yi(i,1)
  sumZcorn(1)=sumZcorn(1)+Icecoordinates(k)%Zi(i,1)
  numcorn(1)=numcorn(1)+1
  end if
  if(abs(Grids(k)%Xp(i,Jn(k))-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Yp(i,Jn(k))-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(i,Jn(k))-Grids(l)%Zp(In(l),1))<tol) then
  sumXcorn(3)=sumXcorn(3)+Icecoordinates(k)%Xi(i,Jn(k))
  sumYcorn(3)=sumYcorn(3)+Icecoordinates(k)%Yi(i,Jn(k))
  sumZcorn(3)=sumZcorn(3)+Icecoordinates(k)%Zi(i,Jn(k))
  numcorn(3)=numcorn(3)+1
  else if(abs(Grids(k)%Xp(i,1)-Grids(l)%Xp(In(l),1))<tol.and.abs(Grids(k)%Yp(i,1)-Grids(l)%Yp(In(l),1))<tol.and.&
  abs(Grids(k)%Zp(i,1)-Grids(l)%Zp(In(l),1))<tol) then
  sumXcorn(3)=sumXcorn(3)+Icecoordinates(k)%Xi(i,1)
  sumYcorn(3)=sumYcorn(3)+Icecoordinates(k)%Yi(i,1)
  sumZcorn(3)=sumZcorn(3)+Icecoordinates(k)%Zi(i,1)
  numcorn(3)=numcorn(3)+1
  end if
  if(abs(Grids(k)%Xp(i,Jn(k))-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(i,Jn(k))-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(i,Jn(k))-Grids(l)%Zp(1,Jn(l)))<tol) then
  sumXcorn(2)=sumXcorn(2)+Icecoordinates(k)%Xi(i,Jn(k))
  sumYcorn(2)=sumYcorn(2)+Icecoordinates(k)%Yi(i,Jn(k))
  sumZcorn(2)=sumZcorn(2)+Icecoordinates(k)%Zi(i,Jn(k))
  numcorn(2)=numcorn(2)+1
  else if(abs(Grids(k)%Xp(i,1)-Grids(l)%Xp(1,Jn(l)))<tol.and.abs(Grids(k)%Yp(i,1)-Grids(l)%Yp(1,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(i,1)-Grids(l)%Zp(1,Jn(l)))<tol) then
  sumXcorn(2)=sumXcorn(2)+Icecoordinates(k)%Xi(i,1)
  sumYcorn(2)=sumYcorn(2)+Icecoordinates(k)%Yi(i,1)
  sumZcorn(2)=sumZcorn(2)+Icecoordinates(k)%Zi(i,1)
  numcorn(2)=numcorn(2)+1
  end if
  if(abs(Grids(k)%Xp(i,Jn(k))-Grids(l)%Xp(In(l),Jn(l)))<tol.and.abs(Grids(k)%Yp(i,Jn(k))-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(i,Jn(k))-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  sumXcorn(4)=sumXcorn(4)+Icecoordinates(k)%Xi(i,Jn(k))
  sumYcorn(4)=sumYcorn(4)+Icecoordinates(k)%Yi(i,Jn(k))
  sumZcorn(4)=sumZcorn(4)+Icecoordinates(k)%Zi(i,Jn(k))
  numcorn(4)=numcorn(4)+1
  else if(abs(Grids(k)%Xp(i,1)-Grids(l)%Xp(In(l),Jn(l)))<tol.and.abs(Grids(k)%Yp(i,1)-Grids(l)%Yp(In(l),Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(i,1)-Grids(l)%Zp(In(l),Jn(l)))<tol) then
  sumXcorn(4)=sumXcorn(4)+Icecoordinates(k)%Xi(i,1)
  sumYcorn(4)=sumYcorn(4)+Icecoordinates(k)%Yi(i,1)
  sumZcorn(4)=sumZcorn(4)+Icecoordinates(k)%Zi(i,1)
  numcorn(4)=numcorn(4)+1
  end if
 end DO
end if
end DO
DO j=2,Jg(l)
 DO k=1,block
  if(k/=l) then
  DO m=1,Jn(k)
  if(abs(Grids(k)%Xp(1,m)-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Yp(1,m)-Grids(l)%Yp(1,j))<tol.and.&
  abs(Grids(k)%Zp(1,m)-Grids(l)%Zp(1,j))<tol) then
  sumXlr(j,1)=sumXlr(j,1)+Icecoordinates(k)%Xi(1,m)
  sumYlr(j,1)=sumYlr(j,1)+Icecoordinates(k)%Yi(1,m)
  sumZlr(j,1)=sumZlr(j,1)+Icecoordinates(k)%Zi(1,m)
  numlr(j,1)=numlr(j,1)+1
  else if(abs(Grids(k)%Xp(In(k),m)-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Yp(In(k),m)-Grids(l)%Yp(1,j))<tol.and.&
  abs(Grids(k)%Zp(In(k),m)-Grids(l)%Zp(1,j))<tol) then
  sumXlr(j,1)=sumXlr(j,1)+Icecoordinates(k)%Xi(In(k),m)
  sumYlr(j,1)=sumYlr(j,1)+Icecoordinates(k)%Yi(In(k),m)
  sumZlr(j,1)=sumZlr(j,1)+Icecoordinates(k)%Zi(In(k),m)
  numlr(j,1)=numlr(j,1)+1
  end if
  if(abs(Grids(k)%Xp(1,m)-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Yp(1,m)-Grids(l)%Yp(In(l),j))<tol.and.&
  abs(Grids(k)%Zp(1,m)-Grids(l)%Zp(In(l),j))<tol) then
  sumXlr(j,2)=sumXlr(j,2)+Icecoordinates(k)%Xi(1,m)
  sumYlr(j,2)=sumYlr(j,2)+Icecoordinates(k)%Yi(1,m)
  sumZlr(j,2)=sumZlr(j,2)+Icecoordinates(k)%Zi(1,m)
  numlr(j,2)=numlr(j,2)+1
  else if(abs(Grids(k)%Xp(In(k),m)-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Yp(In(k),m)-Grids(l)%Yp(In(l),j))<tol.and.&
  abs(Grids(k)%Zp(In(k),m)-Grids(l)%Zp(In(l),j))<tol) then
  sumXlr(j,2)=sumXlr(j,2)+Icecoordinates(k)%Xi(In(k),m)
  sumYlr(j,2)=sumYlr(j,2)+Icecoordinates(k)%Yi(In(k),m)
  sumZlr(j,2)=sumZlr(j,2)+Icecoordinates(k)%Zi(In(k),m)
  numlr(j,2)=numlr(j,2)+1
  end if
  end DO
  DO m=2,Ig(k)
  if(abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(1,j))<tol.and.&
  abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(1,j))<tol) then
  sumXlr(j,1)=sumXlr(j,1)+Icecoordinates(k)%Xi(m,1)
  sumYlr(j,1)=sumYlr(j,1)+Icecoordinates(k)%Yi(m,1)
  sumZlr(j,1)=sumZlr(j,1)+Icecoordinates(k)%Zi(m,1)
  numlr(j,1)=numlr(j,1)+1
  else if(abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(1,j))<tol.and.abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(1,j))<tol.and.&
  abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(1,j))<tol) then
  sumXlr(j,1)=sumXlr(j,1)+Icecoordinates(k)%Xi(m,Jn(k))
  sumYlr(j,1)=sumYlr(j,1)+Icecoordinates(k)%Yi(m,Jn(k))
  sumZlr(j,1)=sumZlr(j,1)+Icecoordinates(k)%Zi(m,Jn(k))
  numlr(j,1)=numlr(j,1)+1
  end if
  if(abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(In(l),j))<tol.and.&
  abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(In(l),j))<tol) then
  sumXlr(j,2)=sumXlr(j,2)+Icecoordinates(k)%Xi(m,1)
  sumYlr(j,2)=sumYlr(j,2)+Icecoordinates(k)%Yi(m,1)
  sumZlr(j,2)=sumZlr(j,2)+Icecoordinates(k)%Zi(m,1)
  numlr(j,2)=numlr(j,2)+1
  else if(abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(In(l),j))<tol.and.abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(In(l),j))<tol.and.&
  abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(In(l),j))<tol) then
  sumXlr(j,2)=sumXlr(j,2)+Icecoordinates(k)%Xi(m,Jn(k))
  sumYlr(j,2)=sumYlr(j,2)+Icecoordinates(k)%Yi(m,Jn(k))
  sumZlr(j,2)=sumZlr(j,2)+Icecoordinates(k)%Zi(m,Jn(k))
  numlr(j,2)=numlr(j,2)+1
  end if
  end DO
  end if
 end DO
end DO
DO i=2,Ig(l)
 DO k=1,block
  if(k/=l) then
  DO m=1,Jn(k)
  if(abs(Grids(k)%Xp(1,m)-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Yp(1,m)-Grids(l)%Yp(i,1))<tol.and.&
  abs(Grids(k)%Zp(1,m)-Grids(l)%Zp(i,1))<tol) then
  sumXud(i,1)=sumXud(i,1)+Icecoordinates(k)%Xi(1,m)
  sumYud(i,1)=sumYud(i,1)+Icecoordinates(k)%Yi(1,m)
  sumZud(i,1)=sumZud(i,1)+Icecoordinates(k)%Zi(1,m)
  numud(i,1)=numud(i,1)+1
  else if(abs(Grids(k)%Xp(In(k),m)-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Yp(In(k),m)-Grids(l)%Yp(i,1))<tol.and.&
  abs(Grids(k)%Zp(In(k),m)-Grids(l)%Zp(i,1))<tol) then
  sumXud(i,1)=sumXud(i,1)+Icecoordinates(k)%Xi(In(k),m)
  sumYud(i,1)=sumYud(i,1)+Icecoordinates(k)%Yi(In(k),m)
  sumZud(i,1)=sumZud(i,1)+Icecoordinates(k)%Zi(In(k),m)
  numud(i,1)=numud(i,1)+1
  end if
  if(abs(Grids(k)%Xp(1,m)-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(1,m)-Grids(l)%Yp(i,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(1,m)-Grids(l)%Zp(i,Jn(l)))<tol) then
  sumXud(i,2)=sumXud(i,2)+Icecoordinates(k)%Xi(1,m)
  sumYud(i,2)=sumYud(i,2)+Icecoordinates(k)%Yi(1,m)
  sumZud(i,2)=sumZud(i,2)+Icecoordinates(k)%Zi(1,m)
  numud(i,2)=numud(i,2)+1
  else if(abs(Grids(k)%Xp(In(k),m)-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(In(k),m)-Grids(l)%Yp(i,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(In(k),m)-Grids(l)%Zp(i,Jn(l)))<tol) then
  sumXud(i,2)=sumXud(i,2)+Icecoordinates(k)%Xi(In(k),m)
  sumYud(i,2)=sumYud(i,2)+Icecoordinates(k)%Yi(In(k),m)
  sumZud(i,2)=sumZud(i,2)+Icecoordinates(k)%Zi(In(k),m)
  numud(i,2)=numud(i,2)+1
  end if
  end DO
  DO m=2,Ig(k)
  if(abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(i,1))<tol.and.&
  abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(i,1))<tol) then
  sumXud(i,1)=sumXud(i,1)+Icecoordinates(k)%Xi(m,1)
  sumYud(i,1)=sumYud(i,1)+Icecoordinates(k)%Yi(m,1)
  sumZud(i,1)=sumZud(i,1)+Icecoordinates(k)%Zi(m,1)
  numud(i,1)=numud(i,1)+1
  else if(abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(i,1))<tol.and.abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(i,1))<tol.and.&
  abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(i,1))<tol) then
  sumXud(i,1)=sumXud(i,1)+Icecoordinates(k)%Xi(m,Jn(k))
  sumYud(i,1)=sumYud(i,1)+Icecoordinates(k)%Yi(m,Jn(k))
  sumZud(i,1)=sumZud(i,1)+Icecoordinates(k)%Zi(m,Jn(k))
  numud(i,1)=numud(i,1)+1
  end if
  if(abs(Grids(k)%Xp(m,1)-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(m,1)-Grids(l)%Yp(i,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(m,1)-Grids(l)%Zp(i,Jn(l)))<tol) then
  sumXud(i,2)=sumXud(i,2)+Icecoordinates(k)%Xi(m,1)
  sumYud(i,2)=sumYud(i,2)+Icecoordinates(k)%Yi(m,1)
  sumZud(i,2)=sumZud(i,2)+Icecoordinates(k)%Zi(m,1)
  numud(i,2)=numud(i,2)+1
  else if(abs(Grids(k)%Xp(m,Jn(k))-Grids(l)%Xp(i,Jn(l)))<tol.and.abs(Grids(k)%Yp(m,Jn(k))-Grids(l)%Yp(i,Jn(l)))<tol.and.&
  abs(Grids(k)%Zp(m,Jn(k))-Grids(l)%Zp(i,Jn(l)))<tol) then
  sumXud(i,2)=sumXud(i,2)+Icecoordinates(k)%Xi(m,Jn(k))
  sumYud(i,2)=sumYud(i,2)+Icecoordinates(k)%Yi(m,Jn(k))
  sumZud(i,2)=sumZud(i,2)+Icecoordinates(k)%Zi(m,Jn(k))
  numud(i,2)=numud(i,2)+1
  end if
  end DO
  end if
 end DO
end DO
Icecoordinates(l)%Xi0(1,1)=sumXcorn(1)/numcorn(1)
Icecoordinates(l)%Yi0(1,1)=sumYcorn(1)/numcorn(1)
Icecoordinates(l)%Zi0(1,1)=sumZcorn(1)/numcorn(1)
Icecoordinates(l)%Xi0(1,Jn(l))=sumXcorn(2)/numcorn(2)
Icecoordinates(l)%Yi0(1,Jn(l))=sumYcorn(2)/numcorn(2)
Icecoordinates(l)%Zi0(1,Jn(l))=sumZcorn(2)/numcorn(2)
Icecoordinates(l)%Xi0(In(l),1)=sumXcorn(3)/numcorn(3)
Icecoordinates(l)%Yi0(In(l),1)=sumYcorn(3)/numcorn(3)
Icecoordinates(l)%Zi0(In(l),1)=sumZcorn(3)/numcorn(3)
Icecoordinates(l)%Xi0(In(l),Jn(l))=sumXcorn(4)/numcorn(4)
Icecoordinates(l)%Yi0(In(l),Jn(l))=sumYcorn(4)/numcorn(4)
Icecoordinates(l)%Zi0(In(l),Jn(l))=sumZcorn(4)/numcorn(4)
DO j=2,Jg(l)
Icecoordinates(l)%Xi0(1,j)=sumXlr(j,1)/numlr(j,1)
Icecoordinates(l)%Yi0(1,j)=sumYlr(j,1)/numlr(j,1)
Icecoordinates(l)%Zi0(1,j)=sumZlr(j,1)/numlr(j,1)
Icecoordinates(l)%Xi0(In(l),j)=sumXlr(j,2)/numlr(j,2)
Icecoordinates(l)%Yi0(In(l),j)=sumYlr(j,2)/numlr(j,2)
Icecoordinates(l)%Zi0(In(l),j)=sumZlr(j,2)/numlr(j,2)
end DO
DO i=2,Ig(l)
Icecoordinates(l)%Xi0(i,1)=sumXud(i,1)/numud(i,1)
Icecoordinates(l)%Yi0(i,1)=sumYud(i,1)/numud(i,1)
Icecoordinates(l)%Zi0(i,1)=sumZud(i,1)/numud(i,1)
Icecoordinates(l)%Xi0(i,Jn(l))=sumXud(i,2)/numud(i,2)
Icecoordinates(l)%Yi0(i,Jn(l))=sumYud(i,2)/numud(i,2)
Icecoordinates(l)%Zi0(i,Jn(l))=sumZud(i,2)/numud(i,2)
end DO
end Subroutine Cornersmooth
