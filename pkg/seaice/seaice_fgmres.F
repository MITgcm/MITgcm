#include "SEAICE_OPTIONS.h"

C--   File seaice_fgmres.F: seaice fgmres dynamical (linear) solver S/R:
C--   Contents
C--   o SEAICE_MAP2VEC
C--   o SEAICE_MAP_RS2VEC
C--   o SEAICE_FGMRES
C--   o SEAICE_SCALPROD

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
      _RL vector (n,nSx,nSy)
#if (defined SEAICE_ALLOW_JFNK) || (defined SEAICE_ALLOW_KRYLOV)
C     === local variables ===
      INTEGER I, J, bi, bj
      INTEGER ii, jj, m
CEOP

      m = n/2
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_JFNK_MAP_REORDER
        ii = 0
        IF ( map2vec ) THEN
         DO J=1,sNy
          jj = 2*sNx*(J-1)
          DO I=1,sNx
           ii = jj + 2*I
           vector(ii-1,bi,bj) = xfld2d(I,J,bi,bj)
           vector(ii,  bi,bj) = yfld2d(I,J,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO J=1,sNy
          jj = 2*sNx*(J-1)
          DO I=1,sNx
           ii = jj + 2*I
           xfld2d(I,J,bi,bj) = vector(ii-1,bi,bj)
           yfld2d(I,J,bi,bj) = vector(ii,  bi,bj)
          ENDDO
         ENDDO
        ENDIF
#else
        IF ( map2vec ) THEN
         DO J=1,sNy
          jj = sNx*(J-1)
          DO I=1,sNx
           ii = jj + I
           vector(ii,  bi,bj) = xfld2d(I,J,bi,bj)
           vector(ii+m,bi,bj) = yfld2d(I,J,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO J=1,sNy
          jj = sNx*(J-1)
          DO I=1,sNx
           ii = jj + I
           xfld2d(I,J,bi,bj) = vector(ii,  bi,bj)
           yfld2d(I,J,bi,bj) = vector(ii+m,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_JFNK_MAP_REORDER */
C     bi,bj-loops
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_JFNK or SEAICE_ALLOW_KRYLOV */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_MAP_RS2VEC
C     !INTERFACE:

      SUBROUTINE SEAICE_MAP_RS2VEC(
     I     n,
     O     xfld2d, yfld2d,
     U     vector,
     I     map2vec, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_MAP_RS2VEC
C     | o maps 2 2D-RS-fields to vector and back
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
      _RS xfld2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS yfld2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vector (n,nSx,nSy)
#if (defined SEAICE_ALLOW_JFNK) || (defined SEAICE_ALLOW_KRYLOV)
C     === local variables ===
      INTEGER I, J, bi, bj
      INTEGER ii, jj, m
CEOP

      m = n/2
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_JFNK_MAP_REORDER
        ii = 0
        IF ( map2vec ) THEN
         DO J=1,sNy
          jj = 2*sNx*(J-1)
          DO I=1,sNx
           ii = jj + 2*I
           vector(ii-1,bi,bj) = xfld2d(I,J,bi,bj)
           vector(ii,  bi,bj) = yfld2d(I,J,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO J=1,sNy
          jj = 2*sNx*(J-1)
          DO I=1,sNx
           ii = jj + 2*I
           xfld2d(I,J,bi,bj) = vector(ii-1,bi,bj)
           yfld2d(I,J,bi,bj) = vector(ii,  bi,bj)
          ENDDO
         ENDDO
        ENDIF
#else
        IF ( map2vec ) THEN
         DO J=1,sNy
          jj = sNx*(J-1)
          DO I=1,sNx
           ii = jj + I
           vector(ii,  bi,bj) = xfld2d(I,J,bi,bj)
           vector(ii+m,bi,bj) = yfld2d(I,J,bi,bj)
          ENDDO
         ENDDO
        ELSE
         DO J=1,sNy
          jj = sNx*(J-1)
          DO I=1,sNx
           ii = jj + I
           xfld2d(I,J,bi,bj) = vector(ii,  bi,bj)
           yfld2d(I,J,bi,bj) = vector(ii+m,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* SEAICE_JFNK_MAP_REORDER */
C     bi,bj-loops
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_JFNK or SEAICE_ALLOW_KRYLOV */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_FGMRES
C     !INTERFACE:
      SUBROUTINE SEAICE_FGMRES (
     I     n,im,rhs,
     U     sol,i,its,vv,w,wk1,wk2,
     I     eps,maxits,iout,
     U     icode,
     I     myThid )

C-----------------------------------------------------------------------
C mlosch Oct 2012: modified the routine further to be compliant with
C MITgcm standards:
C f90 -> F
C !-comment -> C-comment
C add its to list of arguments
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
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
      integer myThid
      integer i, n, im, its, maxits, iout, icode
      _RL rhs(n,nSx,nSy), sol(n,nSx,nSy)
      _RL vv(n,im+1,nSx,nSy), w(n,im,nSx,nSy)
      _RL wk1(n,nSx,nSy), wk2(n,nSx,nSy), eps
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
C      call fgmres (n,im,rhs,sol,i,vv,w,wk1,wk2,eps,maxits,iout,
C     &             icode,its,mythid)
C
C      if (icode .eq. 1) then
C         call  precon(n, wk1, wk2)    <--- user variable preconditioning
C         goto 1
C      else if (icode .ge. 2) then
C         call  matvec (n,wk1, wk2)    <--- user matrix vector product.
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
C i     == internal iteration counter, updated in this routine
C its   == current (Krylov) iteration counter, updated in this routine
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
#if (defined SEAICE_ALLOW_JFNK) || (defined SEAICE_ALLOW_KRYLOV)
C     local variables -- !jfl modif
      integer imax
      parameter ( imax = 50 )
      _RL hh(4*imax+1,4*imax,MAX_NO_THREADS)
      _RL c(4*imax,MAX_NO_THREADS),s(4*imax,MAX_NO_THREADS)
      _RL rs(4*imax+1,MAX_NO_THREADS),t,ro
C-------------------------------------------------------------
C     arnoldi size should not exceed 50 in this version..
C-------------------------------------------------------------
      integer i1Thid(MAX_NO_THREADS)
      integer i1, ii, j, jj, k, k1!, n1
      integer bi, bj
      _RL r0, gam, eps1(MAX_NO_THREADS)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

CEOP
C     local common blocks to replace a save statement in the original code
      COMMON /SEAICE_FMRES_LOC_I/ i1Thid
      COMMON /SEAICE_FMRES_LOC_RL/ hh, c, s, rs, eps1
      _RL epsmac
      parameter ( epsmac =  1.d-16 )
C
C     computed goto
C
      if ( im .gt. imax ) stop 'size of krylov space > 50'
      goto (100,200,300,11) icode +1
 100  continue
      its = 0
C-------------------------------------------------------------
C     **  outer loop starts here..
C--------------compute initial residual vector --------------
C 10   continue
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do j=1,n
         wk1(j,bi,bj)=sol(j,bi,bj)
        enddo
       enddo
      enddo
      icode = 3
      RETURN
 11   continue
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do j=1,n
         vv(j,1,bi,bj) = rhs(j,bi,bj) - wk2(j,bi,bj)
        enddo
       enddo
      enddo
 20   continue
      call SEAICE_SCALPROD(n, im+1, 1, 1, vv, vv, ro, myThid)
      ro = sqrt(ro)
      if (ro .eq. 0.0 _d 0) goto 999
      t = 1.0 _d 0/ ro
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do j=1, n
         vv(j,1,bi,bj) = vv(j,1,bi,bj)*t
        enddo
       enddo
      enddo
      if (its .eq. 0) eps1(myThid)=eps
C     not sure what this is, r0 is never used again
      if (its .eq. 0) r0 = ro
      if (iout .gt. 0) then
       _BEGIN_MASTER( myThid )
       write(msgBuf, 199) its, ro
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
C           print *,'chau',its, ro !write(iout, 199) its, ro
       _END_MASTER( myThid )
      endif
C
C     initialize 1-st term  of rhs of hessenberg system..
C
      rs(1,myThid) = ro
      i = 0
 4    continue
      i=i+1
      its = its + 1
      i1Thid(myThid) = i + 1
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do k=1, n
         wk1(k,bi,bj) = vv(k,i,bi,bj)
        enddo
       enddo
      enddo
C
C     return
C
      icode = 1
      RETURN
 200  continue
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do k=1, n
         w(k,i,bi,bj) = wk2(k,bi,bj)
        enddo
       enddo
      enddo
C
C     call matvec operation
C
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do k=1,n
         wk1(k,bi,bj)=wk2(k,bi,bj)
        enddo
       enddo
      enddo
C
C     return
C
      icode = 2
      RETURN
 300  continue
C
C     first call to ope corresponds to intialization goto back to 11.
C
C      if (icode .eq. 3) goto 11
      i1 = i1Thid(myThid)
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do k=1,n
         vv(k,i1,bi,bj)=wk2(k,bi,bj)
        enddo
       enddo
      enddo
C
C     modified gram - schmidt...
C
      do j=1, i
       call SEAICE_SCALPROD(n, im+1, j, i1, vv, vv, t, myThid)
       hh(j,i,myThid) = t
       do bj=myByLo(myThid),myByHi(myThid)
        do bi=myBxLo(myThid),myBxHi(myThid)
         do k=1,n
          vv(k,i1,bi,bj) = vv(k,i1,bi,bj) - t*vv(k,j,bi,bj)
         enddo
        enddo
       enddo
      enddo
      call SEAICE_SCALPROD(n, im+1, i1, i1, vv, vv, t, myThid)
      t = sqrt(t)
      hh(i1,i,myThid) = t
      if (t .ne. 0.0 _d 0) then
       t = 1.0 _d 0 / t
       do bj=myByLo(myThid),myByHi(myThid)
        do bi=myBxLo(myThid),myBxHi(myThid)
         do k=1,n
          vv(k,i1,bi,bj) = vv(k,i1,bi,bj)*t
         enddo
        enddo
       enddo
      endif
C
C     done with modified gram schmidt and arnoldi step.
C     now  update factorization of hh
C
      if (i .ne. 1) then
C
C     perfrom previous transformations  on i-th column of h
C
       do k=2,i
        k1 = k-1
        t = hh(k1,i,myThid)
        hh(k1,i,myThid) = c(k1,myThid)*t+s(k1,myThid)*hh(k,i,myThid)
        hh(k,i,myThid) = -s(k1,myThid)*t+c(k1,myThid)*hh(k,i,myThid)
       enddo
      endif
      gam = sqrt(hh(i,i,myThid)**2 + hh(i1,i,myThid)**2)
      if (gam .eq. 0.0 _d 0) gam = epsmac
C-----------#determine next plane rotation  #-------------------
      c(i,myThid)   = hh(i, i,myThid)/gam
      s(i,myThid)   = hh(i1,i,myThid)/gam
C     numerically more stable Givens rotation, but the results
C     are not better
CML      c(i,myThid)=1. _d 0
CML      s(i,myThid)=0. _d 0
CML      if ( abs(hh(i1(myThid,myThid),i)) .gt. 0.0 _d 0) then
CML       if ( abs(hh(i1,i,myThid)) .gt. abs(hh(i,i,myThid)) ) then
CML        gam = hh(i,i,myThid)/hh(i1,i,myThid)
CML        s(i,myThid) = 1./sqrt(1.+gam*gam)
CML        c(i,myThid) = s(i,myThid)*gam
CML       else
CML        gam = hh(i1,i,myThid)/hh(i,i,myThid)
CML        c(i,myThid) = 1./sqrt(1.+gam*gam)
CML        s(i,myThid) = c(i,myThid)*gam
CML       endif
CML      endif
      rs(i1,myThid) = -s(i,myThid)*rs(i,myThid)
      rs(i ,myThid) =  c(i,myThid)*rs(i,myThid)
C
C     determine res. norm. and test for convergence
C
      hh(i,i,myThid) = c(i,myThid)*hh(i, i,myThid)
     &     +           s(i,myThid)*hh(i1,i,myThid)
      ro = abs(rs(i1,myThid))
      if (iout .gt. 0) then
       _BEGIN_MASTER( myThid )
       write(msgBuf, 199) its, ro
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )
      endif
CML   The original code uses only im as the maximum allowed iterations, but
CML   I want no more than maxits iterations, so we change the code here.
CML      if (i .lt. im .and. (ro .gt. eps1(myThid)))  goto 4
      if (i .lt. im .and. its .lt. maxits
     &     .and. (ro .gt. eps1(myThid))) goto 4
C
C     now compute solution. first solve upper triangular system.
C
      rs(i,myThid) = rs(i,myThid)/hh(i,i,myThid)
      do ii=2,i
       k=i-ii+1
       k1 = k+1
       t=rs(k,myThid)
       do j=k1,i
        t = t-hh(k,j,myThid)*rs(j,myThid)
       enddo
       rs(k,myThid) = t/hh(k,k,myThid)
      enddo
C
C     done with back substitution..
C     now form linear combination to get solution
C
      do j=1, i
       t = rs(j,myThid)
       do bj=myByLo(myThid),myByHi(myThid)
        do bi=myBxLo(myThid),myBxHi(myThid)
         do k=1,n
          sol(k,bi,bj) = sol(k,bi,bj) + t*w(k,j,bi,bj)
         enddo
        enddo
       enddo
      enddo
C
C     test for return
C
      if (ro .le. eps1(myThid) .or. its .ge. maxits) goto 999
C
C     else compute residual vector and continue..
C
C       goto 10

      i1 = i1Thid(myThid)
      do j=1,i
       jj = i1-j+1
       rs(jj-1,myThid) = -s(jj-1,myThid)*rs(jj,myThid)
       rs(jj  ,myThid) =  c(jj-1,myThid)*rs(jj,myThid)
      enddo
      do j=1,i1
       t = rs(j,myThid)
       if (j .eq. 1)  t = t - 1.0 _d 0
       do bj=myByLo(myThid),myByHi(myThid)
        do bi=myBxLo(myThid),myBxHi(myThid)
         do k=1,n
          vv(k,1,bi,bj) = vv(k,1,bi,bj) + t*vv(k,j,bi,bj)
         enddo
        enddo
       enddo
      enddo
C
C     restart outer loop.
C
      goto 20
 999  icode = 0

 199  format(' SEAICE_FGMRES: its =', i4, ' res. norm =', d26.16)
C
#endif /* SEAICE_ALLOW_JFNK or SEAICE_ALLOW_KRYLOV */
      RETURN
C-----end-of-fgmres-----------------------------------------------------
C-----------------------------------------------------------------------
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_SCALPROD
C     !INTERFACE:

      subroutine SEAICE_SCALPROD(n,im,i1,i2,dx,dy,t,myThid)

C     forms the dot product of two vectors.
C     uses unrolled loops for increments equal to one.
C     jack dongarra, linpack, 3/11/78.
C     ML: code stolen from BLAS-ddot and adapted for parallel applications

      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
      integer n, im, i1, i2
      _RL dx(n,im,nSx,nSy),dy(n,im,nSx,nSy)
      _RL t
      integer myThid
#if (defined SEAICE_ALLOW_JFNK) || (defined SEAICE_ALLOW_KRYLOV)
C     local arrays
      _RL dtemp(nSx,nSy)
      integer i,m,mp1,bi,bj
CEOP

      m = mod(n,5)
      mp1 = m + 1
      t     = 0. _d 0
c     if( m .eq. 0 ) go to 40
      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        dtemp(bi,bj) = 0. _d 0
        if ( m .ne. 0 ) then
         do i = 1,m
          dtemp(bi,bj) = dtemp(bi,bj) + dx(i,i1,bi,bj)*dy(i,i2,bi,bj)
     &         * scalarProductMetric(i,1,bi,bj)
         enddo
        endif
        if ( n .ge. 5 ) then
c     if( n .lt. 5 ) go to 60
c40   mp1 = m + 1
         do i = mp1,n,5
          dtemp(bi,bj) = dtemp(bi,bj)               +
     &        dx(i,    i1,bi,bj)*dy(i,    i2,bi,bj)
     &        * scalarProductMetric(i,    1, bi,bj) +
     &        dx(i + 1,i1,bi,bj)*dy(i + 1,i2,bi,bj)
     &        * scalarProductMetric(i + 1,1, bi,bj) +
     &        dx(i + 2,i1,bi,bj)*dy(i + 2,i2,bi,bj)
     &        * scalarProductMetric(i + 2,1, bi,bj) +
     &        dx(i + 3,i1,bi,bj)*dy(i + 3,i2,bi,bj)
     &        * scalarProductMetric(i + 3,1, bi,bj) +
     &        dx(i + 4,i1,bi,bj)*dy(i + 4,i2,bi,bj)
     &        * scalarProductMetric(i + 4,1, bi,bj)
         enddo
c60   continue
        endif
       enddo
      enddo
      CALL GLOBAL_SUM_TILE_RL( dtemp,t,myThid )

#endif /* SEAICE_ALLOW_JFNK or SEAICE_ALLOW_KRYLOV */
      RETURN
      END
