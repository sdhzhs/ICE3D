Subroutine Constants
use COM
implicit none
pi=3.1415926535897932d+0
epsi=0
sigmar=5.67e-8
R=8.31
Ma=0.029
g=9.8
P0=95840
rhoa=Ma*P0/(R*Ta)
mua=1.716e-5*(Ta/273.11)**1.5*383.67/(Ta+110.56)
ka=0.00008*(Ta-Tf)+0.0244
ca=1014
Pr=mua*ca/ka
Prt=0.85
rhow=999.8
muw=1.793e-3
kw=0.561
cw=4218
rhoi=917
ki=2.14
Tf=273.15
Lf=3.336e+5
Lv=2.838e+6
alpha=1.0
end subroutine Constants
