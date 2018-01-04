Subroutine SaveResults
use COM
implicit none
integer l
filename(2)='Grid(t=00).xyz'
write(filename(2)(8:9),'(I2.2)') nint(4.*timestep/timeout)
filename(3)='iced(t=00).dat'
write(filename(3)(8:9),'(I2.2)') nint(4.*timestep/timeout)
filename(4)='Plot3D(t=00).xyz'
write(filename(4)(10:11),'(I2.2)') nint(4.*timestep/timeout)
if(icecoupled=='Y') then
open(unit=1,file=filename(2),status='replace')
write(1,*) 2*block
DO l=1,block
write(1,*) In(l),Jn(l),1
end DO
DO l=1,block
write(1,*) In(l),Jn(l),1
end DO
DO l=1,block
  write(1,*) Grids(l)%Xp
  write(1,*) Grids(l)%Yp
  write(1,*) Grids(l)%Zp
end DO
DO l=1,block
  write(1,*) Icecoordinates(l)%Xi
  write(1,*) Icecoordinates(l)%Yi
  write(1,*) Icecoordinates(l)%Zi
end DO
close(1)
filename(2)='Solution(t=00).dat'
write(filename(2)(12:13),'(I2.2)') nint(4.*timestep/timeout)
open(unit=1,file=filename(2),status='replace')
write(1,*) block
DO l=1,block
write(1,*) In(l),Jn(l),1,2
end DO
DO l=1,block
write(1,*) Icecoordinates(l)%hn
write(1,*) Icecoordinates(l)%bn
end DO
close(1)
open(unit=2,file=filename(3),status='replace')
DO l=1,block
write(2,formc(l)) Icecoordinates(l)%h
write(2,formc(l)) Icecoordinates(l)%b
end DO
write(2,*) timestep*dt+time
write(2,*) hp
close(2)
open(unit=3,file=filename(4),status='replace')
write(3,*) block
DO l=1,block
write(3,*) In(l),Jn(l),1
end DO
DO l=1,block
write(3,*) Icecoordinates(l)%Xi
write(3,*) Icecoordinates(l)%Yi
write(3,*) Icecoordinates(l)%Zi
end DO
close(3)
else if(icecoupled=='N') then
open(unit=3,file=filename(2),status='replace')
write(3,*) block
DO l=1,block
write(3,*) Ig(l),Jg(l),1
end DO
DO l=1,block
write(3,*) Grids(l)%X
write(3,*) Grids(l)%Y
write(3,*) Grids(l)%Z
end DO
close(3)
filename(2)='Solution(t=00).dat'
write(filename(2)(12:13),'(I2.2)') nint(4.*timestep/timeout)
open(unit=1,file=filename(2),status='replace')
write(1,*) block
DO l=1,block
write(1,*) Ig(l),Jg(l),1,1
end DO
DO l=1,block
write(1,formc(l)) Icecoordinates(l)%h
end DO
write(1,*) timestep*dt+time
write(1,*) hp
close(1)
end if
print *,'Output results is completed!'
end Subroutine SaveResults
