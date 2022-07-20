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
 bclower(1)=0
 bcupper(1)=0
 DO j=1,Jg(l)
  if(topos(l)%nbl(j)/=0) then
  nb = topos(l)%nbl(j)-1
  bclower(2)=j
  bcupper(2)=j
  nblower(1)=topos(l)%Iindexl(j)
  nblower(2)=topos(l)%Jindexl(j)
  nbupper(1)=topos(l)%Iindexl(j)
  nbupper(2)=topos(l)%Jindexl(j)
  if(topos(l)%Iindexl(j)/=topos(l)%Iindexll(j).and.topos(l)%Iindexl(j)==Ig(nb+1)) then
  map(1)=0
  map(2)=1
  dir(1)=1
  dir(2)=1
  else if(topos(l)%Jindexl(j)/=topos(l)%Jindexll(j).and.topos(l)%Jindexl(j)==Jg(nb+1)) then
  map(1)=1
  map(2)=0
  dir(1)=1
  dir(2)=-1
  else if(topos(l)%Iindexl(j)/=topos(l)%Iindexll(j).and.topos(l)%Iindexl(j)==1) then
  map(1)=0
  map(2)=1
  dir(1)=-1
  dir(2)=-1
  else if(topos(l)%Jindexl(j)/=topos(l)%Jindexll(j).and.topos(l)%Jindexl(j)==1) then
  map(1)=1
  map(2)=0
  dir(1)=-1
  dir(2)=1
  end if
  Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
  end if
 end DO
 bclower(1)=In(l)
 bcupper(1)=In(l)
 DO j=1,Jg(l)
  if(topos(l)%nbr(j)/=0) then
  nb = topos(l)%nbr(j)-1
  bclower(2)=j
  bcupper(2)=j
  nblower(1)=topos(l)%Iindexr(j)
  nblower(2)=topos(l)%Jindexr(j)
  nbupper(1)=topos(l)%Iindexr(j)
  nbupper(2)=topos(l)%Jindexr(j)
  if(topos(l)%Iindexr(j)/=topos(l)%Iindexrr(j).and.topos(l)%Iindexr(j)==1) then
  map(1)=0
  map(2)=1
  dir(1)=1
  dir(2)=1
  else if(topos(l)%Jindexr(j)/=topos(l)%Jindexrr(j).and.topos(l)%Jindexr(j)==1) then
  map(1)=1
  map(2)=0
  dir(1)=1
  dir(2)=-1
  else if(topos(l)%Iindexr(j)/=topos(l)%Iindexrr(j).and.topos(l)%Iindexr(j)==Ig(nb+1)) then
  map(1)=0
  map(2)=1
  dir(1)=-1
  dir(2)=-1
  else if(topos(l)%Jindexr(j)/=topos(l)%Jindexrr(j).and.topos(l)%Jindexr(j)==Jg(nb+1)) then
  map(1)=1
  map(2)=0
  dir(1)=-1
  dir(2)=1
  end if
  Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
  end if
 end DO
 bclower(2)=0
 bcupper(2)=0
 DO i=1,Ig(l)
  if(topos(l)%nbu(i)/=0) then
  nb = topos(l)%nbu(i)-1
  bclower(1)=i
  bcupper(1)=i
  nblower(1)=topos(l)%Iindexu(i)
  nblower(2)=topos(l)%Jindexu(i)
  nbupper(1)=topos(l)%Iindexu(i)
  nbupper(2)=topos(l)%Jindexu(i)
  if(topos(l)%Jindexu(i)/=topos(l)%Jindexuu(i).and.topos(l)%Jindexu(i)==Jg(nb+1)) then
  map(1)=0
  map(2)=1
  dir(1)=1
  dir(2)=1
  else if(topos(l)%Iindexu(i)/=topos(l)%Iindexuu(i).and.topos(l)%Iindexu(i)==Ig(nb+1)) then
  map(1)=1
  map(2)=0
  dir(1)=-1
  dir(2)=1
  else if(topos(l)%Jindexu(i)/=topos(l)%Jindexuu(i).and.topos(l)%Jindexu(i)==1) then
  map(1)=0
  map(2)=1
  dir(1)=-1
  dir(2)=-1
  else if(topos(l)%Iindexu(i)/=topos(l)%Iindexuu(i).and.topos(l)%Iindexu(i)==1) then
  map(1)=1
  map(2)=0
  dir(1)=1
  dir(2)=-1
  end if
  Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
  end if
 end DO
 bclower(2)=Jn(l)
 bcupper(2)=Jn(l)
 DO i=1,Ig(l)
  if(topos(l)%nbd(i)/=0) then
  nb = topos(l)%nbd(i)-1
  bclower(1)=i
  bcupper(1)=i
  nblower(1)=topos(l)%Iindexd(i)
  nblower(2)=topos(l)%Jindexd(i)
  nbupper(1)=topos(l)%Iindexd(i)
  nbupper(2)=topos(l)%Jindexd(i)
  if(topos(l)%Jindexd(i)/=topos(l)%Jindexdd(i).and.topos(l)%Jindexd(i)==1) then
  map(1)=0
  map(2)=1
  dir(1)=1
  dir(2)=1
  else if(topos(l)%Iindexd(i)/=topos(l)%Iindexdd(i).and.topos(l)%Iindexd(i)==1) then
  map(1)=1
  map(2)=0
  dir(1)=-1
  dir(2)=1
  else if(topos(l)%Jindexd(i)/=topos(l)%Jindexdd(i).and.topos(l)%Jindexd(i)==Jg(nb+1)) then
  map(1)=0
  map(2)=1
  dir(1)=-1
  dir(2)=-1
  else if(topos(l)%Iindexd(i)/=topos(l)%Iindexdd(i).and.topos(l)%Iindexd(i)==Ig(nb+1)) then
  map(1)=1
  map(2)=0
  dir(1)=1
  dir(2)=-1
  end if
  Call HYPRE_SStructGridSetNeighborPart(gridmb,part,bclower,bcupper,nb,nblower,nbupper,map,dir,ierr)
  end if
 end DO
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
     values(l)=Imps(m)%ahM(1,i,j)
    else
     values(l)=-Imps(m)%ahM(k,i,j)
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
