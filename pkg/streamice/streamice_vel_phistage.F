C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_vel_phistage.F,v 1.3 2015/03/23 14:07:17 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
#ifndef ALLOW_OPENAD
      SUBROUTINE STREAMICE_VEL_PHISTAGE ( 
     I                   myThid, 
     I                   maxNLIter, 
     I                   maxCGiter, 
     O                   cgtol, 
     O                   nL_iter,
     O                   CONVERGED,
     O                   err_last_change,
     O                   err_init,
     I                   isinloop)
#else
      SUBROUTINE STREAMICE_VEL_PHISTAGE ( 
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
     I                   isinloop)
#endif

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


#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT ARGUMENTS
      INTEGER myThid
      INTEGER maxNLIter
      INTEGER maxCGiter
      _RL     cgtol
      _RL     err_last_change
      _RL     err_init
      _RL     err_max
      INTEGER nL_iter
      LOGICAL CONVERGED
      INTEGER isinloop
#ifdef ALLOW_OPENAD
      INTEGER adj_iter
      LOGICAL adj_CONVERGED
#endif

#ifdef ALLOW_STREAMICE
#if (defined (ALLOW_STREAMICE_OAD_FP))

C     LOCAL VARIABLES

      INTEGER i, j, bi, bj, cg_iters, m
      _RL err_max_fp, errmax
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      
#ifdef ALLOW_OPENAD
      adj_iter=0
      adj_CONVERGED=.false.
#endif

      IF (isinloop.eq.1)  THEN
      IF (.not.CONVERGED) THEN

       BJ=BI+I+J

       nl_iter = nl_iter + 1   

       CALL STREAMICE_VEL_PHI (
     I           myThid, 
     I           maxNLIter, 
     I           maxCGiter, 
     I           cgtol, 
     O           cg_iters,err_max)

!---------------------------give num iters--------------------------------
       WRITE(msgBuf,'(A,I5,A,I4,A)') 'streamice linear solve number',
     &                       nl_iter, " ",
     &                       cg_iters,
     &                       ' iterations '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
!---------------------------check conv and lower cg tolerance -------------
!---------------------------to be done for adjoint?           -------------

       if (STREAMICE_chkfixedptconvergence) then

        CALL STREAMICE_GET_VEL_FP_ERR ( err_max_fp, myThid )

        WRITE(msgBuf,'(A,1PE22.14)') 'STREAMICE_FP_ERROR =',
     &                       err_max_fp
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

        IF (err_max_fp .LE. streamice_nonlin_tol_fp) THEN
         CONVERGED = .true.
        ENDIF


       endif

       if (STREAMICE_chkresidconvergence) then


           WRITE(msgBuf,'(A,E15.7)') 'err/err_init',
     &                       err_max/err_init
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

           IF (err_max .LE. streamice_nonlin_tol * err_init) THEN
            CONVERGED = .true.
           ENDIF

           IF (err_max<err_last_change*1.e-2 .and.
     &      STREAMICE_lower_cg_tol) THEN
            cgtol = cgtol / 20.
            err_last_change = err_max_fp
            WRITE(msgBuf,'(A,E15.7)') 'new cg tol: ',
     &                       cgtol
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
           ENDIF

       endif


       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           U_streamice (i,j,bi,bj)=u_new_SI (i,j,bi,bj)
           V_streamice (i,j,bi,bj)=v_new_SI (i,j,bi,bj) 
#ifdef STREAMICE_HYBRID_STRESS
           streamice_taubx(i,j,bi,bj)=taubx_new_si(i,j,bi,bj)
           streamice_tauby(i,j,bi,bj)=tauby_new_si(i,j,bi,bj)
           do m=1,Nr
            visc_streamice_full(i,j,m,bi,bj)=
     &       visc_full_new_si(i,j,m,bi,bj)
           enddo
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO

!-----------------------------------------------------------------------------
      ENDIF
      ENDIF

#endif
#endif
      RETURN
      END
