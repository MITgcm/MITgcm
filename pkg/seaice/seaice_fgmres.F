C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_fgmres.F,v 1.1 2012/10/16 06:50:53 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C--   File seaice_fgmres.F: seaice fgmres dynamical (linear) solver S/R:
C--   Contents
C--   o SEAICE_FGMRES_DRIVER
C--   o SEAICE_MAP2VEC
C--   o SEAICE_FGMRES
C--   o SCALPROD

CBOP
C     !ROUTINE: SEAICE_FGMRES_DRIVER
C     !INTERFACE:

      SUBROUTINE SEAICE_FGMRES_DRIVER(
     I     uIceRes, vIceRes, 
     U     duIce, dvIce, 
     U     iCode, 
     I     FGMRESeps,  
     I     newtonIter, krylovIter, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_FGMRES_DRIVER
C     | o driver routine for fgmres
C     | o does the conversion between 2D fields and 1D vector
C     |   back and forth
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SEAICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     newtonIter :: current iterate of Newton iteration
C     krylovIter :: current iterate of Newton iteration
C     iCode  :: FGMRES parameter to determine next step
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER newtonIter
      INTEGER krylovIter
      INTEGER iCode
C     FGMRESeps :: tolerance for FGMRES
      _RL FGMRESeps
C     du/vIce   :: solution vector
      _RL duIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL dvIce(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     u/vIceRes :: residual F(u)
      _RL uIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceRes(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if ( defined (SEAICE_CGRID) && \
      defined (SEAICE_ALLOW_JFNK) && \
      defined (SEAICE_ALLOW_DYNAMICS) )
C     Local variables:
C     k :: loop indices
      INTEGER k
C     FGMRES parameters
C     n  :: size of the input vector(s)
C     im :: size of Krylov space
C     ifgmres :: interation counter
C     iout :: control output of fgmres
      INTEGER n
      PARAMETER ( n  = 2*sNx*sNy*nSx*nSy )
      INTEGER im
      PARAMETER ( im = 50 )
      INTEGER ifgmres, iout
C     work arrays
      _RL rhs(n), sol(n)
      _RL vv(n,im+1), w(n,im)
      _RL wk1(n), wk2(n)
C     need to store some of the fgmres parameters and fields so that
C     they are not forgotten between Krylov iterations
      COMMON /FGMRES_I/ ifgmres
      COMMON /FGMRES_RL/ sol, rhs, vv, w
CEOP

C     iout=1 give a little bit of output
      iout=1

C     For now, let only the master thread do all the work
C     - copy from 2D arrays to 1D-vector
C     - perform fgmres step (including global sums)
C     - copy from 1D-vector to 2D arrays
C     not sure if this works properly

      _BEGIN_MASTER ( myThid )
      IF ( iCode .EQ. 0 ) THEN
C     first guess is zero because it is a correction
C     wk2 needs to reset for iCode = 0, because it contains
C     remains of the previous Krylov iteration
       DO k=1,n
        sol(k) = 0. _d 0
        wk2(k) = 0. _d 0
       ENDDO
      ELSEIF ( iCode .EQ. 3 ) THEN
       CALL SEAICE_MAP2VEC(n,uIceRes,vIceRes,rhs,.TRUE.,myThid)
C     change sign because we are solving J*u = -F
       DO k=1,n
        rhs(k) = -rhs(k)
       ENDDO
      ELSE
C     map preconditioner results or Jacobian times vector, 
C     stored in du/vIce to wk2
       CALL SEAICE_MAP2VEC(n,duIce,dvIce,wk2,.TRUE.,myThid)
      ENDIF
C     
      CALL SEAICE_FGMRES (n,im,rhs,sol,ifgmres,vv,w,wk1,wk2, 
     &     FGMRESeps,SEAICEkrylovIterMax,
     &     iout,icode,krylovIter,myThid) 
C     
      IF ( iCode .EQ. 0 ) THEN
C     map sol(ution) vector to du/vIce
       CALL SEAICE_MAP2VEC(n,duIce,dvIce,sol,.FALSE.,myThid)
      ELSE
C     map work vector to du/vIce to either compute a preconditioner
C     solution (wk1=rhs) or a Jacobian times wk1
       CALL SEAICE_MAP2VEC(n,duIce,dvIce,wk1,.FALSE.,myThid)
      ENDIF
      _END_MASTER ( myThid )
        
C     Fill overlaps in updated fields
      CALL EXCH_UV_XY_RL( duIce, dvIce,.TRUE.,myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_MAP2VEC
C     !INTERFACE:

      SUBROUTINE SEAICE_MAP2VEC(
     I     n, 
     O     xfld2d, yfld2d, 
     U     vector, 
     I     map2vec, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_MAP2VEC
C     | o maps 2 2D-fields to vector and back
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
C     === Routine arguments ===
      INTEGER n
      LOGICAL map2vec
      INTEGER myThid
      _RL xfld2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL yfld2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vector (n)
C     === local variables ===
      INTEGER I, J, bi, bj
      INTEGER ii, jj, ib, jb, m
CEOP
      
      m = n/2
      IF ( map2vec ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        jb = nSx*sNy*sNx*(bj-1)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         ib = jb + sNy*sNx*(bi-1)
         DO J=1,sNy
          jj = ib + sNx*(J-1)
          DO I=1,sNx
           ii = jj + I
           vector(ii)   = xfld2d(I,J,bi,bj)
           vector(ii+m) = yfld2d(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO       
      ELSE
       DO bj=myByLo(myThid),myByHi(myThid)
        jb = nSx*sNy*sNx*(bj-1)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         ib = jb + sNy*sNx*(bi-1)
         DO J=1,sNy
          jj = ib + sNx*(J-1)
          DO I=1,sNx
           ii = jj + I
           xfld2d(I,J,bi,bj) = vector(ii)
           yfld2d(I,J,bi,bj) = vector(ii+m)
          ENDDO
         ENDDO
        ENDDO
       ENDDO       
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_FGMRES
C     !INTERFACE:

      SUBROUTINE SEAICE_FGMRES (n,im,rhs,sol,i,vv,w,wk1, wk2, 
     &     eps,maxits,iout,icode,its,myThid) 

C-----------------------------------------------------------------------
C mlosch Oct 2012: modified the routine further to be compliant with
C MITgcm standards:
C f90 -> F
C !-comment -> C-comment
C double precision -> _RL
C implicit none
C 
C jfl Dec 1st 2006. We modified the routine so that it is double precison.
C Here are the modifications:
C 1) implicit real (a-h,o-z) becomes implicit real*8 (a-h,o-z) 
C 2) real bocomes real*8
C 3) subroutine scopy.f has been changed for dcopy.f
C 4) subroutine saxpy.f has been changed for daxpy.f
C 5) function sdot.f has been changed for ddot.f
C 6) 1e-08 becomes 1d-08
C
C Be careful with the dcopy, daxpy and ddot code...there is a slight 
C difference with the single precision versions (scopy, saxpy and sdot).
C In the single precision versions, the array are declared sightly differently.
C It is written for single precision:
C
C modified 12/3/93, array(1) declarations changed to array(*)
C-----------------------------------------------------------------------

      implicit none
CML   implicit double precision (a-h,o-z) !jfl modification
      integer myThid
      integer n, im, maxits, iout, icode
      _RL rhs(*), sol(*), vv(n,im+1), w(n,im)
      _RL wk1(n), wk2(n), eps
C-----------------------------------------------------------------------
C flexible GMRES routine. This is a version of GMRES which allows a 
C a variable preconditioner. Implemented with a reverse communication 
C protocole for flexibility -
C DISTRIBUTED VERSION (USES DISTDOT FOR DDOT) 
C explicit (exact) residual norms for restarts  
C written by Y. Saad, modified by A. Malevsky, version February 1, 1995
C-----------------------------------------------------------------------
C This Is A Reverse Communication Implementation. 
C------------------------------------------------- 
C USAGE: (see also comments for icode below). FGMRES
C should be put in a loop and the loop should be active for as
C long as icode is not equal to 0. On return fgmres will
C    1) either be requesting the new preconditioned vector applied
C       to wk1 in case icode.eq.1 (result should be put in wk2) 
C    2) or be requesting the product of A applied to the vector wk1
C       in case icode.eq.2 (result should be put in wk2) 
C    3) or be terminated in case icode .eq. 0. 
C on entry always set icode = 0. So icode should be set back to zero
C upon convergence.
C-----------------------------------------------------------------------
C Here is a typical way of running fgmres: 
C
C      icode = 0
C 1    continue
C      call fgmres (n,im,rhs,sol,i,vv,w,wk1, wk2,eps,maxits,iout,icode)
C
C      if (icode .eq. 1) then
C         call  precon(n, wk1, wk2)    <--- user's variable preconditioning
C         goto 1
C      else if (icode .ge. 2) then
C         call  matvec (n,wk1, wk2)    <--- user's matrix vector product. 
C         goto 1
C      else 
C         ----- done ---- 
C         .........
C-----------------------------------------------------------------------
C list of parameters 
C------------------- 
C
C n     == integer. the dimension of the problem
C im    == size of Krylov subspace:  should not exceed 50 in this
C          version (can be reset in code. looking at comment below)
C rhs   == vector of length n containing the right hand side
C sol   == initial guess on input, approximate solution on output
C vv    == work space of size n x (im+1)
C w     == work space of length n x im 
C wk1,
C wk2,  == two work vectors of length n each used for the reverse
C          communication protocole. When on return (icode .ne. 1)
C          the user should call fgmres again with wk2 = precon * wk1
C          and icode untouched. When icode.eq.1 then it means that
C          convergence has taken place.
C          
C eps   == tolerance for stopping criterion. process is stopped
C          as soon as ( ||.|| is the euclidean norm):
C          || current residual||/||initial residual|| <= eps
C
C maxits== maximum number of iterations allowed
C
C iout  == output unit number number for printing intermediate results
C          if (iout .le. 0) no statistics are printed.
C 
C icode = integer. indicator for the reverse communication protocole.
C         ON ENTRY : icode should be set to icode = 0.
C         ON RETURN: 
C       * icode .eq. 1 value means that fgmres has not finished
C         and that it is requesting a preconditioned vector before
C         continuing. The user must compute M**(-1) wk1, where M is
C         the preconditioing  matrix (may vary at each call) and wk1 is
C         the vector as provided by fgmres upun return, and put the 
C         result in wk2. Then fgmres must be called again without
C         changing any other argument. 
C       * icode .eq. 2 value means that fgmres has not finished
C         and that it is requesting a matrix vector product before
C         continuing. The user must compute  A * wk1, where A is the
C         coefficient  matrix and wk1 is the vector provided by 
C         upon return. The result of the operation is to be put in
C         the vector wk2. Then fgmres must be called again without
C         changing any other argument. 
C       * icode .eq. 0 means that fgmres has finished and sol contains 
C         the approximate solution.
C         comment: typically fgmres must be implemented in a loop
C         with fgmres being called as long icode is returned with 
C         a value .ne. 0. 
C-----------------------------------------------------------------------
C     local variables -- !jfl modif
      integer imax 
      parameter ( imax = 50 )
      _RL hh(4*imax+1,4*imax),c(4*imax),s(4*imax)
      _RL rs(4*imax+1),t,ro
C-------------------------------------------------------------
C     arnoldi size should not exceed 50 in this version..
C-------------------------------------------------------------
      integer i, its, i1, ii, j, jj, k, k1!, n1
      _RL r0, gam, epsmac, eps1

CEOP
      save
      data epsmac/1.d-16/
C     
C     computed goto 
C     
      if ( im .gt. imax ) stop 'size of krylov space > 50'
      goto (100,200,300,11) icode +1
 100  continue
CML      n1 = n + 1
      its = 0
C-------------------------------------------------------------
C     **  outer loop starts here..
C--------------compute initial residual vector --------------
C 10   continue
CML      call dcopy (n, sol, 1, wk1, 1) !jfl modification
      do k=1,n
       wk1(k)=sol(k)
      enddo
      icode = 3
      return
 11   continue
      do j=1,n
         vv(j,1) = rhs(j) - wk2(j) 
      enddo
CML 20   ro = ddot(n, vv, 1, vv,1) !jfl modification
 20   call scalprod(n, vv, vv, ro, myThid)
      ro = sqrt(ro)
      if (ro .eq. 0.0d0) goto 999 
      t = 1.0d0/ ro 
      do j=1, n
         vv(j,1) = vv(j,1)*t 
      enddo
      if (its .eq. 0) eps1=eps
      if (its .eq. 0) r0 = ro
      if (iout .gt. 0) write(*, 199) its, ro!&
C           print *,'chau',its, ro !write(iout, 199) its, ro
C     
C     initialize 1-st term  of rhs of hessenberg system..
C     
      rs(1) = ro
      i = 0
 4    i=i+1
      its = its + 1
      i1 = i + 1
      do k=1, n
         wk1(k) = vv(k,i) 
      enddo
C     
C     return
C     
      icode = 1

      return
 200  continue
      do k=1, n
         w(k,i) = wk2(k) 
      enddo
C     
C     call matvec operation
C     
      icode = 2
CML      call dcopy(n, wk2, 1, wk1, 1) !jfl modification
      do k=1,n
       wk1(k)=wk2(k)
      enddo
C
C     return
C     
      return
 300  continue
C     
C     first call to ope corresponds to intialization goto back to 11.
C     
C      if (icode .eq. 3) goto 11
CML      call  dcopy (n, wk2, 1, vv(1,i1), 1) !jfl modification
      do k=1,n
       vv(k,i1)=wk2(k)
      enddo
C     
C     modified gram - schmidt...
C     
      do j=1, i
CML         t = ddot(n, vv(1,j), 1, vv(1,i1), 1) !jfl modification
         call scalprod(n, vv(1,j), vv(1,i1), t, myThid)
         hh(j,i) = t
CML         call daxpy(n, -t, vv(1,j), 1, vv(1,i1), 1) !jfl modification
CML      enddo
CML      do j=1, i
CML         t = hh(j,i)
         do k=1,n
          vv(k,i1) = vv(k,i1) - t*vv(k,j)
         enddo
      enddo
CML      t = sqrt(ddot(n, vv(1,i1), 1, vv(1,i1), 1)) !jfl modification
      call scalprod(n, vv(1,i1), vv(1,i1), t, myThid)
      t = sqrt(t)
      hh(i1,i) = t
      if (t .eq. 0.0d0) goto 58
      t = 1.0d0 / t
      do k=1,n
         vv(k,i1) = vv(k,i1)*t
      enddo
C     
C     done with modified gram schimd and arnoldi step. 
C     now  update factorization of hh
C     
 58   if (i .eq. 1) goto 121
C     
C     perfrom previous transformations  on i-th column of h
C     
      do k=2,i
         k1 = k-1
         t = hh(k1,i)
         hh(k1,i) = c(k1)*t + s(k1)*hh(k,i)
         hh(k,i) = -s(k1)*t + c(k1)*hh(k,i)
      enddo
 121  gam = sqrt(hh(i,i)**2 + hh(i1,i)**2)
      if (gam .eq. 0.0d0) gam = epsmac
C-----------#determine next plane rotation  #-------------------
      c(i) = hh(i,i)/gam
      s(i) = hh(i1,i)/gam
      rs(i1) = -s(i)*rs(i)
      rs(i) =  c(i)*rs(i)
C     
C     determine res. norm. and test for convergence-
C     
      hh(i,i) = c(i)*hh(i,i) + s(i)*hh(i1,i)
      ro = abs(rs(i1))
      if (iout .gt. 0) write(*, 199) its, ro
      if (i .lt. im .and. (ro .gt. eps1))  goto 4
C     
C     now compute solution. first solve upper triangular system.
C     
      rs(i) = rs(i)/hh(i,i)
      do ii=2,i
         k=i-ii+1
         k1 = k+1
         t=rs(k)
         do j=k1,i
            t = t-hh(k,j)*rs(j)
         enddo
         rs(k) = t/hh(k,k)
      enddo
C     
C     done with back substitution..
C     now form linear combination to get solution
C     
      do j=1, i
       t = rs(j)
C         call daxpy(n, t, w(1,j), 1, sol,1) !jfl modification
       do k=1,n
        sol(k) = sol(k) + t*w(k,j)
       enddo
      enddo
C     
C     test for return 
C     
      print *, 'ml-fgmres: its, maxits: ', its, maxits, ro, '<', eps1
      if (ro .le. eps1 .or. its .ge. maxits) goto 999
C     
C     else compute residual vector and continue..
C     
C       goto 10

      do j=1,i
        jj = i1-j+1
        rs(jj-1) = -s(jj-1)*rs(jj)
        rs(jj) = c(jj-1)*rs(jj)
      enddo
      do j=1,i1
        t = rs(j)
        if (j .eq. 1)  t = t-1.0d0
CML        call daxpy (n, t, vv(1,j), 1,  vv, 1)
        do k=1,n
         vv(k,1) = vv(k,1) + t*vv(k,j)
        enddo
      enddo
C     
C     restart outer loop.
C     
      goto 20
 999  icode = 0

 199  format('   -- fgmres its =', i4, ' res. norm =', d26.16)
C     
      return 
C-----end-of-fgmres----------------------------------------------------- 
C-----------------------------------------------------------------------
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SCALPROD
C     !INTERFACE:

      subroutine scalprod(n,dx,dy,t,myThid)

C
C     forms the dot product of two vectors.
C     uses unrolled loops for increments equal to one.
C     jack dongarra, linpack, 3/11/78.
C     ML: code stolen from BLAS and adapted for parallel applications
C
      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
      integer myThid
      _RL dx(n),dy(n)
      real*8 dtemp
      real*8 t
      integer i,m,mp1,n
#ifdef ALLOW_USE_MPI
      INTEGER mpiRC
#endif   /* ALLOW_USE_MPI */
CEOP
C
      m = mod(n,5)
      dtemp = 0. _d 0
      t     = 0. _d 0
      if( m .eq. 0 ) go to 40
      do i = 1,m
       dtemp = dtemp + dx(i)*dy(i)
      enddo
      if( n .lt. 5 ) go to 60
   40 mp1 = m + 1
      do i = mp1,n,5
       dtemp = dtemp + dx(i)*dy(i) + dx(i + 1)*dy(i + 1) + 
     &      dx(i + 2)*dy(i + 2) + dx(i + 3)*dy(i + 3) + 
     &      dx(i + 4)*dy(i + 4)
      enddo
   60 continue
C     sum over all processors
#ifdef ALLOW_USE_MPI
      t = dtemp
      IF ( usingMPI ) THEN
       CALL MPI_Allreduce(t,dtemp,1,MPI_DOUBLE_PRECISION,MPI_SUM,
     &                   MPI_COMM_MODEL,mpiRC)
      ENDIF
#endif /* ALLOW_USE_MPI */
      t = dtemp
      
CML      return
CML      end
CML
CML      subroutine daxpy(n,da,dx,incx,dy,incy)
CMLC                                                                                                                                                
CMLC     constant times a vector plus a vector.                
CMLC     uses unrolled loops for increments equal to one.      
CMLC     jack dongarra, linpack, 3/11/78.                      
CMLC                                                           
CML      _RL dx(n),dy(n),da
CML      integer i,incx,incy,ix,iy,m,mp1,n
CMLC                                                           
CML      if(n.le.0)return
CML      if (da .eq. 0.0d0) return
CML      if(incx.eq.1.and.incy.eq.1)go to 20
CMLC                                                           
CMLC        code for unequal increments or equal increments    
CMLC          not equal to 1                                   
CMLC                                                           
CML      ix = 1
CML      iy = 1
CML      if(incx.lt.0)ix = (-n+1)*incx + 1
CML      if(incy.lt.0)iy = (-n+1)*incy + 1
CML      do 10 i = 1,n
CML        dy(iy) = dy(iy) + da*dx(ix)
CML        ix = ix + incx
CML        iy = iy + incy
CML   10 continue
CML      return
CMLC                                                           
CMLC        code for both increments equal to 1                
CMLC                                                           
CMLC                                                           
CMLC        clean-up loop                                      
CMLC                                                           
CML   20 m = mod(n,4)
CML      if( m .eq. 0 ) go to 40
CML      do 30 i = 1,m
CML        dy(i) = dy(i) + da*dx(i)
CML   30 continue
CML      if( n .lt. 4 ) return
CML   40 mp1 = m + 1
CML      do 50 i = mp1,n,4
CML        dy(i) = dy(i) + da*dx(i)
CML        dy(i + 1) = dy(i + 1) + da*dx(i + 1)
CML        dy(i + 2) = dy(i + 2) + da*dx(i + 2)
CML        dy(i + 3) = dy(i + 3) + da*dx(i + 3)
CML   50 continue
CML      return
CML      end
CML
CML      subroutine  dcopy(n,dx,incx,dy,incy)
CMLC                                                           
CMLC     copies a vector, x, to a vector, y.                   
CMLC     uses unrolled loops for increments equal to one.      
CMLC     jack dongarra, linpack, 3/11/78.                      
CMLC                                                           
CML      _RL dx(n),dy(n)
CML      integer i,incx,incy,ix,iy,m,mp1,n
CMLC                                                           
CML      if(n.le.0)return
CML      if(incx.eq.1.and.incy.eq.1)go to 20
CMLC                                                           
CMLC        code for unequal increments or equal increments    
CMLC          not equal to 1                                   
CMLC                                                           
CML      ix = 1
CML      iy = 1
CML      if(incx.lt.0)ix = (-n+1)*incx + 1
CML      if(incy.lt.0)iy = (-n+1)*incy + 1
CML      do 10 i = 1,n
CML        dy(iy) = dx(ix)
CML        ix = ix + incx
CML        iy = iy + incy
CML   10 continue
CML      return
CMLC                                                           
CMLC        code for both increments equal to 1                
CMLC                                                           
CMLC                                                           
CMLC        clean-up loop                                      
CMLC                                                           
CML   20 m = mod(n,7)
CML      if( m .eq. 0 ) go to 40
CML      do 30 i = 1,m
CML        dy(i) = dx(i)
CML   30 continue
CML      if( n .lt. 7 ) return
CML   40 mp1 = m + 1
CML      do 50 i = mp1,n,7
CML        dy(i) = dx(i)
CML        dy(i + 1) = dx(i + 1)
CML        dy(i + 2) = dx(i + 2)
CML        dy(i + 3) = dx(i + 3)
CML        dy(i + 4) = dx(i + 4)
CML        dy(i + 5) = dx(i + 5)
CML        dy(i + 6) = dx(i + 6)
CML   50 continue

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID and SEAICE_ALLOW_JFNK */

      RETURN
      END
