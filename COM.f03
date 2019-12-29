Module COM
implicit none
type Grid
real(8),allocatable,dimension(:,:)::Xp,Yp,Zp,Xp0,Yp0,Zp0,X,Y,Z,X0,Y0,Z0,panel,DE,DG,DF,DE1,DE2,DG1,DG2,DF1,DF2
end type
type Force
real(8),allocatable,dimension(:,:)::P,Ax,Ay,Az,beta,dropv,A1,A2,G1,G2,G3
end type
type Energy
real(8),allocatable,dimension(:,:)::Uae,hcv,Tb,qe0,qe1,qr0,qr1,mev,msv
end type
type Flux
real(8),allocatable,dimension(:,:)::Q1,Q2,Qp1,Qp2,dQp1,dQp2,sax,say
end type
type Imp
real(8),allocatable,dimension(:,:)::ahP,ahW,ahE,ahN,ahS,bh,dh,dh0
end type
type Icecoordinate
real(8),allocatable,dimension(:,:)::h,b,b0,Xi,Yi,Zi,Xi0,Yi0,Zi0,Xw,Yw,Zw,hn,bn
end type
type Boundata
real(8),allocatable,dimension(:)::Xbcl,Ybcl,Zbcl,Xpbcl,Ypbcl,Zpbcl,Pbcl,A1bcl,G1bcl,hbcl,Xbcr,Ybcr,Zbcr,Xpbcr,Ypbcr,Zpbcr,Pbcr,A1bcr,&
G1bcr,hbcr,Xbcu,Ybcu,Zbcu,Xpbcu,Ypbcu,Zpbcu,Pbcu,A2bcu,G2bcu,hbcu,Xbcd,Ybcd,Zbcd,Xpbcd,Ypbcd,Zpbcd,Pbcd,A2bcd,G2bcd,hbcd,Qp1bcl,&
Qp1bcr,Qp2bcu,Qp2bcd,dQp1bcl,dQp1bcr,dQp2bcu,dQp2bcd,dhbcl,dhbcr,dhbcu,dhbcd,DEbcl,DEbcr,DEbcu,DEbcd,DGbcl,DGbcr,DGbcu,DGbcd,DFbcl,&
DFbcr,DFbcu,DFbcd,dul,dur,dvu,dvd,saxl,saxr,sayu,sayd,bbcl,bbcr,bbcu,bbcd,bbcl0,bbcr0,bbcu0,bbcd0,hbcll,hbcrr,hbcuu,hbcdd,bbcll,&
bbcrr,bbcuu,bbcdd
end type
type topo
integer,allocatable,dimension(:)::nbl,nbr,nbu,nbd,Iindexl,Jindexl,Iindexr,Jindexr,Iindexu,Jindexu,Iindexd,Jindexd,Iindexll,Jindexll,&
Iindexrr,Jindexrr,Iindexuu,Jindexuu,Iindexdd,Jindexdd,nbpl,nbpr,nbpu,nbpd,Ipindexl,Jpindexl,Ipindexr,Jpindexr,Ipindexu,Jpindexu,&
Ipindexd,Jpindexd
character(1),allocatable,dimension(:)::topol,topor,topou,topod,topopl,topopr,topopu,topopd
end type
save
integer block,maxl,timestep,timeout
real(8) pi,g,R,Ma,rhoa,rhow,rhoi,mua,muw,sigma,ka,kw,ki,ca,cw,Pr,Prt,Tf,Lf,Lv,P0,epsi,sigmar,lwc,mvd,Ta,Td,Ts,Wf,c,hp,bp,dt,t,&
time,err,alpha
character(16) surtempcontrol,solutioncontrol,initialtimecontrol,discretecontrol,icecoupled,advancemethod,fluidproperty,Scpt
character(64) filename(5)
integer,allocatable,dimension(:)::In,Jn,Ig,Jg
character(32),allocatable,dimension(:)::formn,formc
type(Grid),allocatable,dimension(:),target::Grids
type(Force),allocatable,dimension(:)::Forces
type(Energy),allocatable,dimension(:)::Energys
type(Flux),allocatable,dimension(:),target::Fluxs
type(Imp),allocatable,dimension(:)::Imps
type(Icecoordinate),allocatable,dimension(:),target::Icecoordinates
type(Boundata),allocatable,dimension(:),target::Boundatas
type(topo),allocatable,dimension(:)::topos

contains

integer Function sgn(x)
real(8) x
if(x>0) then
sgn=1
else if(x<0) then
sgn=-1
else
sgn=0
end if
return
end Function sgn

end module COM
