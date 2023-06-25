#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_VEL_SOLVE_OPENAD ( myThid, maxNLIter,
     &                                maxCGiter, myiter )
C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
      INTEGER maxNLIter
      INTEGER maxCGIter
      INTEGER myIter

#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES

      INTEGER i, j, k, l, bi, bj, loopiter
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER NL_iter
      _RL err_last_change, cgtol, err_init
      LOGICAL CONVERGED

#ifdef ALLOW_OPENAD
      INTEGER ADJ_ITER
      LOGICAL ADJ_CONVERGED
#endif

#ifdef ALLOW_STREAMICE_OAD_FP
      isinloop0 =0
      isinloop1 =1
      isinloop2 =2
#endif

#if (defined (ALLOW_STREAMICE_OAD_FP))

      IF (STREAMICE_ppm_driving_stress) THEN
       CALL STREAMICE_DRIVING_STRESS_PPM (myThid)
      ELSE
       CALL STREAMICE_DRIVING_STRESS (myThid)
      ENDIF

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      _EXCH_XY_RL( taudx_SI , myThid )
      _EXCH_XY_RL( taudy_SI , myThid )
      CALL STREAMICE_FORCED_BUTTRESS (myThid)
#endif

      CALL TIMER_START ('STREAMICE_VEL_SOLVE',myThid)

      cgtol = streamice_cg_tol
      nl_iter = 0
      CONVERGED = .false.
      err_last_change = 1 _d 1

      _EXCH_XY_RL( taudx_SI , myThid )
      _EXCH_XY_RL( taudy_SI , myThid )

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           U_streamice (i,j,bi,bj)=U_streamice (i,j,bi,bj)
     & + 0*u_new_SI (i,j,bi,bj)
           V_streamice (i,j,bi,bj)=V_streamice (i,j,bi,bj)
     & + 0*v_new_SI (i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

#ifdef STREAMICE_HYBRID_STRESS
       CALL STREAMICE_VISC_BETA_HYBRID ( myThid )
#else
       CALL STREAMICE_VISC_BETA ( myThid )
#endif

       if (STREAMICE_chkresidconvergence .or.
     &    (streamice_maxnliter_cpl.eq.0 .and. myIter.eq.0)) then

        CALL STREAMICE_GET_VEL_RESID_ERR_OAD ( err_init, myThid )
        err_last_change = err_init

        WRITE(msgBuf,'(A,E15.7)') 'initial nonlinear resid (error): ',
     &                       err_init
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

       endif

#ifndef ALLOW_OPENAD
      CALL STREAMICE_VEL_PHISTAGE (
     I                   myThid,
     I                   maxNLIter,
     I                   maxCGiter,
     O                   cgtol,
     O                   nL_iter,
     O                   CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop0)
#else
      CALL STREAMICE_VEL_PHISTAGE (
     I                   myThid,
     I                   maxNLIter,
     I                   maxCGiter,
     O                   cgtol,
     O                   nL_iter,
     O                   adj_iter,
     O                   CONVERGED,
     O                   ADJ_CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop0)
#endif

      DO loopiter=1,maxNLIter

C     To avoid using "exit", loop goes through all iterations
C       but after convergence loop does nothing

!        IF (.not.CONVERGED) THEN

#ifndef ALLOW_OPENAD
      CALL STREAMICE_VEL_PHISTAGE (
     I                   myThid,
     I                   maxNLIter,
     I                   maxCGiter,
     O                   cgtol,
     O                   nL_iter,
     O                   CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop1)
#else
      CALL STREAMICE_VEL_PHISTAGE (
     I                   myThid,
     I                   maxNLIter,
     I                   maxCGiter,
     O                   cgtol,
     O                   nL_iter,
     O                   adj_iter,
     O                   CONVERGED,
     O                   ADJ_CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop1)
#endif

       !DO bj = myByLo(myThid), myByHi(myThid)
       ! DO bi = myBxLo(myThid), myBxHi(myThid)
       !  DO j=1-OLy,sNy+OLy
       !   DO i=1-OLx,sNx+OLx
       !    U_streamice (i,j,bi,bj)=u_new_SI (i,j,bi,bj)
       !    V_streamice (i,j,bi,bj)=v_new_SI (i,j,bi,bj)
       !   ENDDO
       !  ENDDO
       ! ENDDO
       !ENDDO

      ENDDO

#ifndef ALLOW_OPENAD
      CALL STREAMICE_VEL_PHISTAGE (
     I                   myThid,
     I                   maxNLIter,
     I                   maxCGiter,
     O                   cgtol,
     O                   nL_iter,
     O                   CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop2)
#else
      CALL STREAMICE_VEL_PHISTAGE (
     I                   myThid,
     I                   maxNLIter,
     I                   maxCGiter,
     O                   cgtol,
     O                   nL_iter,
     O                   adj_iter,
     O                   CONVERGED,
     O                   ADJ_CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop2)
#endif

C END NL ITER. LOOP
C-------------------------------------------------------------------

      if (nl_iter .lt. streamice_max_nl_iter) then
       WRITE(msgBuf,'(A,I5,A)') 'VELOCITY SOLVE CONVERGED, ',
     &         nl_iter, ' iterations'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      else
       WRITE(msgBuf,'(A,I5,A)') 'VELOCITY SOLVE NOT CONVERGED IN ',
     &         nl_iter, ' iterations'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      endif

      _EXCH_XY_RL(U_streamice, myThid)
      _EXCH_XY_RL(V_streamice, myThid)

      CALL TIMER_STOP ('STREAMICE_VEL_SOLVE',myThid)

#endif
#endif
      RETURN
      END
