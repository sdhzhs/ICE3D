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
real(8),allocatable,dimension(:,:)::h,b,b0,Xi,Yi,Zi,Xw,Yw,Zw,hn,bn
end type
type Boundata
real(8),allocatable,dimension(:)::Xbcl,Ybcl,Zbcl,Xpbcl,Ypbcl,Zpbcl,Pbcl,A1bcl,G1bcl,hbcl,Xbcr,Ybcr,Zbcr,Xpbcr,Ypbcr,Zpbcr,Pbcr,A1bcr,&
G1bcr,hbcr,Xbcu,Ybcu,Zbcu,Xpbcu,Ypbcu,Zpbcu,Pbcu,A2bcu,G2bcu,hbcu,Xbcd,Ybcd,Zbcd,Xpbcd,Ypbcd,Zpbcd,Pbcd,A2bcd,G2bcd,hbcd,Qp1bcl,&
Qp1bcr,Qp2bcu,Qp2bcd,dQp1bcl,dQp1bcr,dQp2bcu,dQp2bcd,dhbcl,dhbcr,dhbcu,dhbcd,DEbcl,DEbcr,DEbcu,DEbcd,DGbcl,DGbcr,DGbcu,DGbcd,DFbcl,&
DFbcr,DFbcu,DFbcd,dul,dur,dvu,dvd,saxl,saxr,sayu,sayd,bbcl,bbcr,bbcu,bbcd,bbcl0,bbcr0,bbcu0,bbcd0,hbcll,hbcrr,hbcuu,hbcdd,bbcll,&
bbcrr,bbcuu,bbcdd
end type
type topo
integer,allocatable,dimension(:)::nbl,nbr,nbu,nbd
character(1),allocatable,dimension(:)::topol,topor,topou,topod,orientl,orientr,orientu,orientd,positl,positr,positu,positd
end type
save
integer block,maxl,timestep,timeout
real(8) pi,g,R,Ma,roua,rouw,roui,miua,miuw,ka,kw,ki,ca,cw,Pr,Prt,P0,Wf,epsi,sigmar,lwc,mvd,Ta,Td,Ts,Tf,Lf,Lv,c,hp,bp,dt,t,&
time,err,alpha
character(8) surtempcontrol,solutioncontrol,initialtimecontrol,discretecontrol,icecoupled,advancemethod,fluidproperty,Scpt
character(64) filename(5)
integer,allocatable,dimension(:)::In,Jn,Ig,Jg
character(32),allocatable,dimension(:)::formn,formc
type(Grid),allocatable,dimension(:)::Grids
type(Force),allocatable,dimension(:)::Forces
type(Energy),allocatable,dimension(:)::Energys
type(Flux),allocatable,dimension(:)::Fluxs
type(Imp),allocatable,dimension(:)::Imps
type(Icecoordinate),allocatable,dimension(:)::Icecoordinates
type(Boundata),allocatable,dimension(:)::Boundatas
type(topo)::topos

contains

integer Function sgn(x)
real(8) x
if(x>0) then
sgn=1
else if(x<0) then
sgn=-1
else if(x==0) then
sgn=0
end if
return
end Function sgn

end module COM

! Following is a list of identifiers in module COM with their meanings
! Grid                Data structure for surface multi-block structured mesh
! Force               Data structure for driven force and collection property
! Energy              Data structure for heat transfer related coefficients
! Flux                Data structure for flow flux of water film
! Imp                 Data structure for matrix coefficients and source terms of an implicit water film flow equation
! Icecoordinate       Data structure for thickness of water and ice (unknown variables) and coordinates of ice shape
! Boundata            Data structure for boundary field
! topo                Data structure for topology information
! block               Patches of the surface structured mesh
! maxl                Maximum iteration steps for solving the linear equations in an implicit time marching
! timestep            Current time steps in a time marching
! timeout             Total time steps in a time marching
! pi                  Circular constant
! g                   Gravitational acceleration
! R                   Universal gas constant
! Ma                  Molecular weight of air
! roua                Density of air
! rouw                Density of water
! roui                Density of ice
! miua                Viscosity of air
! miuw                Viscosity of water
! ka                  Thermal conductivity of air
! kw                  Thermal conductivity of water
! ki                  Thermal conductivity of ice
! ca                  Specific heat at constant pressure of air
! cw                  Specific heat of water
! Pr                  Prandtl number
! Prt                 Turbulent Prandtl number
! P0                  Pressure of free stream
! Wf                  Velocity of free stream
! epsi                Heat emissivity
! sigmar              Stefan constant
! lwc                 Liquid Water Content
! mvd                 Mean Volume Diameter
! Ta                  Temperature of air
! Td                  Temperature of droplets
! Ts                  Temperature of substract
! Tf                  Fusion point of ice
! Lf                  Latent heat of fusion
! Lv                  Latent heat of evaporation
! c                   Scaling factor
! hp,bp               Precursor thickness of water and ice
! dt                  Time step
! t                   Total time
! time                Current time
! err                 Minimum residuals for solving the linear equations in an implicit time marching
! alpha               Empirical factor of thermal conduction in ice layer
! surtempcontrol      Whether using an inner iteration to calulate the surface temperature
! solutioncontrol     Whether using explicit or implicit time marching method
! initialtimecontrol  Whether initializing from a clean surface
! discretecontrol     The discrete method for flow flux at cell interface
! icecoupled          Whether coupling with the icing equation
! advancemethod       The pseudo-steady method
! fluidproperty       The air property
! Scpt                Whether using a script file
! filename            File name
! In,Jn               Dimensions of the grid nodes in each direction of one patch
! Ig,Jg               Dimensions of the grid cells in each direction of one patch
! formn,formc         Formatted strings
! sgn()               Sign function