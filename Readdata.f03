Subroutine Readdata
use COM
implicit none
integer i,j,l,ib
!real(8) au1
character(1) ch1
open(unit=1,file=filename(2),status='old')
read(1,*) block
allocate(Grids(block),Forces(block),Energys(block),Fluxs(block),Imps(block),Icecoordinates(block),Boundatas(block),&
topos(block),In(block),Jn(block),Ig(block),Jg(block),formn(block),formc(block))
DO i=1,block
read(1,*) In(i),Jn(i)
Ig(i)=In(i)-1
Jg(i)=Jn(i)-1
allocate(Grids(i)%Xp(In(i),Jn(i)),Grids(i)%Yp(In(i),Jn(i)),Grids(i)%Zp(In(i),Jn(i)),Grids(i)%Xp0(In(i),Jn(i)),Grids(i)%Yp0(In(i),Jn(i)),&
Grids(i)%Zp0(In(i),Jn(i)),Grids(i)%X(Ig(i),Jg(i)),Grids(i)%Y(Ig(i),Jg(i)),Grids(i)%Z(Ig(i),Jg(i)),Grids(i)%X0(Ig(i),Jg(i)),&
Grids(i)%Y0(Ig(i),Jg(i)),Grids(i)%Z0(Ig(i),Jg(i)),Grids(i)%panel(Ig(i),Jg(i)),Grids(i)%DE(Ig(i),Jg(i)),Grids(i)%DG(Ig(i),Jg(i)),&
Grids(i)%DF(Ig(i),Jg(i)),Grids(i)%DE1(In(i),Jg(i)),Grids(i)%DE2(Ig(i),Jn(i)),Grids(i)%DG1(In(i),Jg(i)),Grids(i)%DG2(Ig(i),Jn(i)),&
Grids(i)%DF1(In(i),Jg(i)),Grids(i)%DF2(Ig(i),Jn(i)))
allocate(Forces(i)%P(Ig(i),Jg(i)),Forces(i)%Ax(Ig(i),Jg(i)),Forces(i)%Ay(Ig(i),Jg(i)),Forces(i)%Az(Ig(i),Jg(i)),&
Forces(i)%beta(Ig(i),Jg(i)),Forces(i)%dropv(Ig(i),Jg(i)),Forces(i)%A1(Ig(i),Jg(i)),Forces(i)%A2(Ig(i),Jg(i)),Forces(i)%G1(Ig(i),Jg(i)),&
Forces(i)%G2(Ig(i),Jg(i)),Forces(i)%G3(Ig(i),Jg(i)))
allocate(Energys(i)%Uae(Ig(i),Jg(i)),Energys(i)%hcv(Ig(i),Jg(i)),Energys(i)%Tb(Ig(i),Jg(i)),Energys(i)%qe0(Ig(i),Jg(i)),&
Energys(i)%qe1(Ig(i),Jg(i)),Energys(i)%qr0(Ig(i),Jg(i)),Energys(i)%qr1(Ig(i),Jg(i)),Energys(i)%mev(Ig(i),Jg(i)),&
Energys(i)%msv(Ig(i),Jg(i)))
allocate(Fluxs(i)%Q1(In(i),Jg(i)),Fluxs(i)%Q2(Ig(i),Jn(i)),Fluxs(i)%Qp1(Ig(i),Jg(i)),Fluxs(i)%Qp2(Ig(i),Jg(i)),Fluxs(i)%sax(In(i),Jg(i)),&
Fluxs(i)%say(Ig(i),Jn(i)),Fluxs(i)%dQp1(Ig(i),Jg(i)),Fluxs(i)%dQp2(Ig(i),Jg(i)))
allocate(Imps(i)%ahM(5,Ig(i),Jg(i)),Imps(i)%bh(Ig(i),Jg(i)),Imps(i)%dh(Ig(i),Jg(i)),Imps(i)%dh0(Ig(i),Jg(i)))
allocate(Icecoordinates(i)%h(Ig(i),Jg(i)),Icecoordinates(i)%b(Ig(i),Jg(i)),Icecoordinates(i)%b0(Ig(i),Jg(i)),&
Icecoordinates(i)%Xi(In(i),Jn(i)),Icecoordinates(i)%Yi(In(i),Jn(i)),Icecoordinates(i)%Zi(In(i),Jn(i)),&
Icecoordinates(i)%Xi0(In(i),Jn(i)),Icecoordinates(i)%Yi0(In(i),Jn(i)),Icecoordinates(i)%Zi0(In(i),Jn(i)),&
Icecoordinates(i)%Xw(In(i),Jn(i)),Icecoordinates(i)%Yw(In(i),Jn(i)),Icecoordinates(i)%Zw(In(i),Jn(i)),&
Icecoordinates(i)%hn(In(i),Jn(i)),Icecoordinates(i)%bn(In(i),Jn(i)))
allocate(Boundatas(i)%Xbcl(Jg(i)),Boundatas(i)%Ybcl(Jg(i)),Boundatas(i)%Zbcl(Jg(i)),Boundatas(i)%Xpbcl(Jn(i)),Boundatas(i)%Ypbcl(Jn(i)),&
Boundatas(i)%Zpbcl(Jn(i)),Boundatas(i)%Pbcl(Jg(i)),Boundatas(i)%A1bcl(Jg(i)),Boundatas(i)%G1bcl(Jg(i)),Boundatas(i)%hbcl(Jg(i)),&
Boundatas(i)%Xbcr(Jg(i)),Boundatas(i)%Ybcr(Jg(i)),Boundatas(i)%Zbcr(Jg(i)),Boundatas(i)%Xpbcr(Jn(i)),Boundatas(i)%Ypbcr(Jn(i)),&
Boundatas(i)%Zpbcr(Jn(i)),Boundatas(i)%Pbcr(Jg(i)),Boundatas(i)%A1bcr(Jg(i)),Boundatas(i)%G1bcr(Jg(i)),Boundatas(i)%hbcr(Jg(i)),&
Boundatas(i)%Xbcu(Ig(i)),Boundatas(i)%Ybcu(Ig(i)),Boundatas(i)%Zbcu(Ig(i)),Boundatas(i)%Xpbcu(In(i)),Boundatas(i)%Ypbcu(In(i)),&
Boundatas(i)%Zpbcu(In(i)),Boundatas(i)%Pbcu(Ig(i)),Boundatas(i)%A2bcu(Ig(i)),Boundatas(i)%G2bcu(Ig(i)),Boundatas(i)%hbcu(Ig(i)),&
Boundatas(i)%Xbcd(Ig(i)),Boundatas(i)%Ybcd(Ig(i)),Boundatas(i)%Zbcd(Ig(i)),Boundatas(i)%Xpbcd(In(i)),Boundatas(i)%Ypbcd(In(i)),&
Boundatas(i)%Zpbcd(In(i)),Boundatas(i)%Pbcd(Ig(i)),Boundatas(i)%A2bcd(Ig(i)),Boundatas(i)%G2bcd(Ig(i)),Boundatas(i)%hbcd(Ig(i)),&
Boundatas(i)%Qp1bcl(Jg(i)),Boundatas(i)%Qp1bcr(Jg(i)),Boundatas(i)%Qp2bcu(Ig(i)),Boundatas(i)%Qp2bcd(Ig(i)),Boundatas(i)%dQp1bcl(Jg(i)),&
Boundatas(i)%dQp1bcr(Jg(i)),Boundatas(i)%dQp2bcu(Ig(i)),Boundatas(i)%dQp2bcd(Ig(i)),Boundatas(i)%dhbcl(Jg(i)),Boundatas(i)%dhbcr(Jg(i)),&
Boundatas(i)%dhbcu(Ig(i)),Boundatas(i)%dhbcd(Ig(i)),Boundatas(i)%dul(Jg(i)),Boundatas(i)%dur(Jg(i)),Boundatas(i)%dvu(Ig(i)),&
Boundatas(i)%dvd(Ig(i)),Boundatas(i)%saxl(Jg(i)),Boundatas(i)%saxr(Jg(i)),Boundatas(i)%sayu(Ig(i)),Boundatas(i)%sayd(Ig(i)),&
Boundatas(i)%bbcl(Jg(i)),Boundatas(i)%bbcr(Jg(i)),Boundatas(i)%bbcu(Ig(i)),Boundatas(i)%bbcd(Ig(i)),Boundatas(i)%bbcl0(Jg(i)),&
Boundatas(i)%bbcr0(Jg(i)),Boundatas(i)%bbcu0(Ig(i)),Boundatas(i)%bbcd0(Ig(i)),Boundatas(i)%DEbcl(Jg(i)),Boundatas(i)%DEbcr(Jg(i)),&
Boundatas(i)%DEbcu(Ig(i)),Boundatas(i)%DEbcd(Ig(i)),Boundatas(i)%DGbcl(Jg(i)),Boundatas(i)%DGbcr(Jg(i)),Boundatas(i)%DGbcu(Ig(i)),&
Boundatas(i)%DGbcd(Ig(i)),Boundatas(i)%DFbcl(Jg(i)),Boundatas(i)%DFbcr(Jg(i)),Boundatas(i)%DFbcu(Ig(i)),Boundatas(i)%DFbcd(Ig(i)),&
Boundatas(i)%hbcll(Jg(i)),Boundatas(i)%hbcrr(Jg(i)),Boundatas(i)%hbcuu(Ig(i)),Boundatas(i)%hbcdd(Ig(i)),Boundatas(i)%bbcll(Jg(i)),&
Boundatas(i)%bbcrr(Jg(i)),Boundatas(i)%bbcuu(Ig(i)),Boundatas(i)%bbcdd(Ig(i)))
allocate(topos(i)%nbl(Jg(i)),topos(i)%nbr(Jg(i)),topos(i)%nbu(Ig(i)),topos(i)%nbd(Ig(i)),topos(i)%topol(Jg(i)),topos(i)%topor(Jg(i)),&
topos(i)%topou(Ig(i)),topos(i)%topod(Ig(i)),topos(i)%Iindexl(Jg(i)),topos(i)%Iindexr(Jg(i)),topos(i)%Iindexu(Ig(i)),&
topos(i)%Iindexd(Ig(i)),topos(i)%Jindexl(Jg(i)),topos(i)%Jindexr(Jg(i)),topos(i)%Jindexu(Ig(i)),topos(i)%Jindexd(Ig(i)),&
topos(i)%Iindexll(Jg(i)),topos(i)%Iindexrr(Jg(i)),topos(i)%Iindexuu(Ig(i)),topos(i)%Iindexdd(Ig(i)),topos(i)%Jindexll(Jg(i)),&
topos(i)%Jindexrr(Jg(i)),topos(i)%Jindexuu(Ig(i)),topos(i)%Jindexdd(Ig(i)),topos(i)%nbpl(Jn(i)),topos(i)%nbpr(Jn(i)),&
topos(i)%nbpu(In(i)),topos(i)%nbpd(In(i)),topos(i)%topopl(Jn(i)),topos(i)%topopr(Jn(i)),topos(i)%topopu(In(i)),topos(i)%topopd(In(i)),&
topos(i)%Ipindexl(Jn(i)),topos(i)%Ipindexr(Jn(i)),topos(i)%Ipindexu(In(i)),topos(i)%Ipindexd(In(i)),topos(i)%Jpindexl(Jn(i)),&
topos(i)%Jpindexr(Jn(i)),topos(i)%Jpindexu(In(i)),topos(i)%Jpindexd(In(i)))
formc(i)='(1X,000(ES11.4,2X))'
formn(i)='(1X,000(ES11.4,2X))'
write(formc(i)(5:7),'(I3.3)') Ig(i)
write(formn(i)(5:7),'(I3.3)') In(i)
end DO
DO l=1,block
READ(1,*) ((Grids(l)%Xp(I,J),I=1,In(l)),J=1,Jn(l))
READ(1,*) ((Grids(l)%Yp(I,J),I=1,In(l)),J=1,Jn(l))
READ(1,*) ((Grids(l)%Zp(I,J),I=1,In(l)),J=1,Jn(l))
end DO
close(1)
print *,'Read grid coordinates completed!'
if(advancemethod=='multi') then
open(unit=1,file=filename(1),status='old')
read(1,*) ib
DO i=1,block
read(1,*) ib
end DO
DO l=1,block
READ(1,*) ((Grids(l)%Xp0(I,J),I=1,In(l)),J=1,Jn(l))
READ(1,*) ((Grids(l)%Yp0(I,J),I=1,In(l)),J=1,Jn(l))
READ(1,*) ((Grids(l)%Zp0(I,J),I=1,In(l)),J=1,Jn(l))
end DO
close(1)
print *,'Read initial grid coordinates completed!'
end if
open(unit=2,file=filename(3),status='old')
read(2,*) ib
DO i=1,block
read(2,*) ib
end DO
DO l=1,block
read(2,*) ((Forces(l)%P(i,j),i=1,Ig(l)),j=1,Jg(l))
read(2,*) ((Energys(l)%Uae(i,j),i=1,Ig(l)),j=1,Jg(l))
read(2,*) ((Energys(l)%hcv(i,j),i=1,Ig(l)),j=1,Jg(l))
read(2,*) ((Forces(l)%Ax(i,j),i=1,Ig(l)),j=1,Jg(l))
read(2,*) ((Forces(l)%Ay(i,j),i=1,Ig(l)),j=1,Jg(l))
read(2,*) ((Forces(l)%Az(i,j),i=1,Ig(l)),j=1,Jg(l))
end DO
close(2)
print *,'Read air fluid property completed!'
open(unit=3,file=filename(4),status='old')
read(3,*) ib
DO i=1,block
read(3,*) ib
end DO
DO l=1,block
read(3,*) ((Forces(l)%beta(i,j),i=1,Ig(l)),j=1,Jg(l))
read(3,*) ((Forces(l)%dropv(i,j),i=1,Ig(l)),j=1,Jg(l))
end DO
close(3)
print *,'Read drop property completed!'
end Subroutine Readdata
