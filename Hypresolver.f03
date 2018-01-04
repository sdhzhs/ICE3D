Subroutine hypresolve(solid)
use COM
implicit none
include 'HYPREf.h'

integer  i,j,k,l,m,solid
integer  ierr,ndims,nentries,nparts,nvars,part,var,object_type,itmax,prlv,iter,nb
integer  ilower(2),iupper(2),stencil_indices(5),offsets(2,5),vartypes(1),bclower(2),bcupper(2),nblower(2),nbupper(2),map(2),dir(2)
real(8)  tol,res,values(5*maxval(Ig)*maxval(Jg))

integer(8)  gridmb
integer(8)  stencil
integer(8)  graph
integer(8)  A
integer(8)  b
integer(8)  x
integer(8)  parA
integer(8)  parb
integer(8)  parx
integer(8)  solver

integer(8)  MPI_COMM_WORLD

integer HYPRE_SSTRUCT_VARIABLE_CELL
parameter(HYPRE_SSTRUCT_VARIABLE_CELL = 0)

ndims = 2
nparts = block
nvars = 1
nentries = 5
var = 0
object_type = HYPRE_PARCSR
vartypes(1) = HYPRE_SSTRUCT_VARIABLE_CELL

Call HYPRE_SStructGridCreate(MPI_COMM_WORLD,ndims,nparts,gridmb,ierr)
DO l=1,block
 ilower(1) = 1
 ilower(2) = 1
 iupper(1) = Ig(l)
 iupper(2) = Jg(l)
 part = l-1
 Call HYPRE_SStructGridSetExtents(gridmb,part,ilower,iupper,ierr)
 Call HYPRE_SStructGridSetVariables(gridmb,part,nvars,vartypes,ierr)
end DO
DO l=1,block
 part = l-1
 nb = topos%nbl(l)-1
 bclower(1)=0
 bclower(2)=1
 bcupper(1)=0
 bcupper(2)=Jg(l)
 if(topos%topol(l)=='J'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='+') then
 nblower(1)=Ig(nb+1)
 nblower(2)=1
 nbupper(1)=Ig(nb+1)
 nbupper(2)=Jg(nb+1)
 map(1)=0
 map(2)=1
 dir(1)=1
 dir(2)=1
 else if(topos%topol(l)=='I'.and.topos%positl(l)=='S'.and.topos%orientl(l)=='-') then
 nblower(1)=Ig(nb+1)
 nblower(2)=Jg(nb+1)
 nbupper(1)=1
 nbupper(2)=Jg(nb+1)
 map(1)=1
 map(2)=0
 dir(1)=1
 dir(2)=-1
 else if(topos%topol(l)=='J'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='-') then
 nblower(1)=1
 nblower(2)=Jg(nb+1)
 nbupper(1)=1
 nbupper(2)=1
 map(1)=0
 map(2)=1
 dir(1)=-1
 dir(2)=-1
 else if(topos%topol(l)=='I'.and.topos%positl(l)=='N'.and.topos%orientl(l)=='+') then
 nblower(1)=1
 nblower(2)=1
 nbupper(1)=Ig(nb+1)
 nbupper(2)=1
 map(1)=1
 map(2)=0
 dir(1)=-1
 dir(2)=1
 end if
 if(topos%nbl(l)/=0) then
 Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
 end if
 nb = topos%nbr(l)-1
 bclower(1)=In(l)
 bclower(2)=1
 bcupper(1)=In(l)
 bcupper(2)=Jg(l)
 if(topos%topor(l)=='J'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='+') then
 nblower(1)=1
 nblower(2)=1
 nbupper(1)=1
 nbupper(2)=Jg(nb+1)
 map(1)=0
 map(2)=1
 dir(1)=1
 dir(2)=1
 else if(topos%topor(l)=='I'.and.topos%positr(l)=='S'.and.topos%orientr(l)=='-') then
 nblower(1)=Ig(nb+1)
 nblower(2)=1
 nbupper(1)=1
 nbupper(2)=1
 map(1)=1
 map(2)=0
 dir(1)=1
 dir(2)=-1
 else if(topos%topor(l)=='J'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='-') then
 nblower(1)=Ig(nb+1)
 nblower(2)=Jg(nb+1)
 nbupper(1)=Ig(nb+1)
 nbupper(2)=1
 map(1)=0
 map(2)=1
 dir(1)=-1
 dir(2)=-1
 else if(topos%topor(l)=='I'.and.topos%positr(l)=='N'.and.topos%orientr(l)=='+') then
 nblower(1)=1
 nblower(2)=Jg(nb+1)
 nbupper(1)=Ig(nb+1)
 nbupper(2)=Jg(nb+1)
 map(1)=1
 map(2)=0
 dir(1)=-1
 dir(2)=1
 end if
 if(topos%nbr(l)/=0) then
 Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
 end if
 nb = topos%nbu(l)-1
 bclower(1)=1
 bclower(2)=0
 bcupper(1)=Ig(l)
 bcupper(2)=0
 if(topos%topou(l)=='I'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='+') then
 nblower(1)=1
 nblower(2)=Jg(nb+1)
 nbupper(1)=Ig(nb+1)
 nbupper(2)=Jg(nb+1)
 map(1)=0
 map(2)=1
 dir(1)=1
 dir(2)=1
 else if(topos%topou(l)=='J'.and.topos%positu(l)=='S'.and.topos%orientu(l)=='-') then
 nblower(1)=Ig(nb+1)
 nblower(2)=Jg(nb+1)
 nbupper(1)=Ig(nb+1)
 nbupper(2)=1
 map(1)=1
 map(2)=0
 dir(1)=-1
 dir(2)=1
 else if(topos%topou(l)=='I'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='-') then
 nblower(1)=Ig(nb+1)
 nblower(2)=1
 nbupper(1)=1
 nbupper(2)=1
 map(1)=0
 map(2)=1
 dir(1)=-1
 dir(2)=-1
 else if(topos%topou(l)=='J'.and.topos%positu(l)=='N'.and.topos%orientu(l)=='+') then
 nblower(1)=1
 nblower(2)=1
 nbupper(1)=1
 nbupper(2)=Jg(nb+1)
 map(1)=1
 map(2)=0
 dir(1)=1
 dir(2)=-1
 end if
 if(topos%nbu(l)/=0) then
 Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
 end if
 nb = topos%nbd(l)-1
 bclower(1)=1
 bclower(2)=Jn(l)
 bcupper(1)=Ig(l)
 bcupper(2)=Jn(l)
 if(topos%topod(l)=='I'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='+') then
 nblower(1)=1
 nblower(2)=1
 nbupper(1)=Ig(nb+1)
 nbupper(2)=1
 map(1)=0
 map(2)=1
 dir(1)=1
 dir(2)=1
 else if(topos%topod(l)=='J'.and.topos%positd(l)=='S'.and.topos%orientd(l)=='-') then
 nblower(1)=1
 nblower(2)=Jg(nb+1)
 nbupper(1)=1
 nbupper(2)=1
 map(1)=1
 map(2)=0
 dir(1)=-1
 dir(2)=1
 else if(topos%topod(l)=='I'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='-') then
 nblower(1)=Ig(nb+1)
 nblower(2)=Jg(nb+1)
 nbupper(1)=1
 nbupper(2)=Jg(nb+1)
 map(1)=0
 map(2)=1
 dir(1)=-1
 dir(2)=-1
 else if(topos%topod(l)=='J'.and.topos%positd(l)=='N'.and.topos%orientd(l)=='+') then
 nblower(1)=Ig(nb+1)
 nblower(2)=1
 nbupper(1)=Ig(nb+1)
 nbupper(2)=Jg(nb+1)
 map(1)=1
 map(2)=0
 dir(1)=1
 dir(2)=-1
 end if
 if(topos%nbd(l)/=0) then
 Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
 end if
end DO
Call HYPRE_SStructGridAssemble(gridmb,ierr)

Call HYPRE_SStructStencilCreate(ndims,nentries,stencil,ierr)
offsets(1,1) = 0
offsets(2,1) = 0
offsets(1,2) = -1
offsets(2,2) = 0
offsets(1,3) = 1
offsets(2,3) = 0
offsets(1,4) = 0
offsets(2,4) = -1
offsets(1,5) = 0
offsets(2,5) = 1
DO i=1,nentries
 stencil_indices(i) = i-1
 Call HYPRE_SStructStencilSetEntry(stencil,stencil_indices(i),offsets(1,i),var,ierr)
end DO

Call HYPRE_SStructGraphCreate(MPI_COMM_WORLD, gridmb, graph, ierr)
Call HYPRE_SStructGraphSetObjectType(graph,object_type,ierr)
DO part=0,block-1
Call HYPRE_SStructGraphSetStencil(graph,part,var,stencil,ierr)
end DO
Call HYPRE_SStructGraphAssemble(graph,ierr)

Call HYPRE_SStructMatrixCreate(MPI_COMM_WORLD,graph,A,ierr)
Call HYPRE_SStructMatrixSetObjectTyp(A,object_type,ierr)
Call HYPRE_SStructMatrixInitialize(A,ierr)
DO m=1,block
 ilower(1) = 1
 ilower(2) = 1
 iupper(1) = Ig(m)
 iupper(2) = Jg(m)
 part = m-1
 l=1
 DO j=1,Jg(m)
  DO i=1,Ig(m)
   DO k=1,nentries
    if(k==1) then
    values(l)=Imps(m)%ahP(i,j)
    else if(k==2) then
    values(l)=-Imps(m)%ahW(i,j)
    else if(k==3) then
    values(l)=-Imps(m)%ahE(i,j)
    else if(k==4) then
    values(l)=-Imps(m)%ahS(i,j)
    else if(k==5) then
    values(l)=-Imps(m)%ahN(i,j)
    end if
    l=l+1
   end DO
  end DO
 end DO
 Call HYPRE_SStructMatrixSetBoxValues(A,part,ilower,iupper,var,nentries,stencil_indices,values,ierr)
end DO
Call HYPRE_SStructMatrixAssemble(A,ierr)
Call HYPRE_SStructMatrixGetObject(A, parA, ierr)

Call HYPRE_SStructVectorCreate(MPI_COMM_WORLD,gridmb,b,ierr)
Call HYPRE_SStructVectorCreate(MPI_COMM_WORLD,gridmb,x,ierr)
Call HYPRE_SStructVectorSetObjectTyp(b,object_type,ierr)
Call HYPRE_SStructVectorSetObjectTyp(x,object_type,ierr)
Call HYPRE_SStructVectorInitialize(b,ierr)
Call HYPRE_SStructVectorInitialize(x,ierr)
DO l=1,block
 ilower(1) = 1
 ilower(2) = 1
 iupper(1) = Ig(l)
 iupper(2) = Jg(l)
 part = l-1
 Call HYPRE_SStructVectorSetBoxValues(b,part,ilower,iupper,var,Imps(l)%bh,ierr)
 Call HYPRE_SStructVectorSetBoxValues(x,part,ilower,iupper,var,Imps(l)%dh,ierr)
end DO
Call HYPRE_SStructVectorAssemble(b,ierr)
Call HYPRE_SStructVectorAssemble(x,ierr)
Call HYPRE_SStructVectorGetObject(b, parb, ierr)
Call HYPRE_SStructVectorGetObject(x, parx, ierr)

itmax = maxl
prlv = 0
tol = err

if(solid==1) then
Call HYPRE_SStructBiCGSTABCreate(MPI_COMM_WORLD, solver, ierr)
Call HYPRE_SStructBiCGSTABSetTol(solver, tol, ierr)
Call HYPRE_SStructBiCGSTABSetPrintLe(solver, prlv, ierr)
Call HYPRE_SStructBiCGSTABSetMaxIter(solver, itmax, ierr)
Call HYPRE_SStructBiCGSTABSetup(solver, A, b, x, ierr)
Call HYPRE_SStructBiCGSTABSolve(solver, A, b, x, ierr)
else if(solid==2) then
Call HYPRE_SStructPCGCreate(MPI_COMM_WORLD, solver, ierr)
Call HYPRE_SStructPCGSetTol(solver, tol, ierr)
Call HYPRE_SStructPCGSetPrintLevel(solver, prlv, ierr)
Call HYPRE_SStructPCGSetMaxIter(solver, itmax, ierr)
Call HYPRE_SStructPCGSetup(solver, A, b, x, ierr)
Call HYPRE_SStructPCGSolve(solver, A, b, x, ierr)
else if(solid==3) then
Call HYPRE_BoomerAMGCreate(solver, ierr)
Call HYPRE_BoomerAMGSetTol(solver, tol, ierr)
Call HYPRE_BoomerAMGSetPrintLevel(solver, prlv, ierr)
Call HYPRE_BoomerAMGSetMaxIter(solver, itmax, ierr)
!Call HYPRE_BoomerAMGSetCoarsenType(solver, 10, ierr)
!Call HYPRE_BoomerAMGSetInterpType(solver, 6, ierr)
!Call HYPRE_BoomerAMGSetRelaxType(solver, 6, ierr)
Call HYPRE_BoomerAMGSetup(solver, parA, parb, parx, ierr)
Call HYPRE_BoomerAMGSolve(solver, parA, parb, parx, ierr)
end if

Call HYPRE_SStructVectorGather(x,ierr)
DO l=1,block
 ilower(1) = 1
 ilower(2) = 1
 iupper(1) = Ig(l)
 iupper(2) = Jg(l)
 part = l-1
 Call HYPRE_SStructVectorGetBoxValues(x,part,ilower,iupper,var,Imps(l)%dh,ierr)
end DO

if(solid==1) then
 Call HYPRE_SStructBiCGSTABGetNumIter(solver, iter, ierr)
 Call HYPRE_SStructBiCGSTABGetFinalRe(solver, res, ierr)
else if(solid==2) then
 Call HYPRE_SStructPCGGetNumIteration(solver, iter, ierr)
 Call HYPRE_SStructPCGGetFinalRelativ(solver, res, ierr)
else if(solid==3) then
 Call HYPRE_BoomerAMGGetNumIterations(solver, iter, ierr)
 Call HYPRE_BoomerAMGGetFinalReltvRes(solver, res, ierr)
end if

if(mod(timestep,10)==1.or.timestep==timeout) then
 write(8,*) timestep,res,iter
end if

Call HYPRE_SStructGridDestroy(gridmb, ierr)
Call HYPRE_SStructStencilDestroy(stencil, ierr)
Call HYPRE_SStructGraphDestroy(graph, ierr)
Call HYPRE_SStructMatrixDestroy(A, ierr)
Call HYPRE_SStructVectorDestroy(b, ierr)
Call HYPRE_SStructVectorDestroy(x, ierr)
if(solid==1) then
Call HYPRE_SStructBiCGSTABDestroy(solver, ierr)
else if(solid==2) then
Call HYPRE_SStructPCGDestroy(solver, ierr)
else if(solid==3) then
Call HYPRE_BoomerAMGDestroy(solver, ierr)
end if

end Subroutine hypresolve
