C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_vel_solve.F,v 1.10 2015/02/16 16:46:45 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_VEL_SOLVE( myThid, maxNLIter, maxCGiter,
     &                                myiter )
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
      INTEGER maxCGIter
      INTEGER myIter

#ifdef ALLOW_STREAMICE

C     LOCAL VARIABLES

!   real, dimension(:,:), pointer :: TAUDX, TAUDY, u_prev_iterate, v_prev_iterate, &
!                         u_bdry_cont, v_bdry_cont, Au, Av, err_u, err_v, &
!                         geolonq, geolatq, u_last, v_last, float_cond, H_node
!   type(ocean_grid_type), pointer      :: G
!   integer                 :: conv_flag, i, j, k,l, iter, isym, &
!                         isdq, iedq, jsdq, jedq, isd, ied, jsd, jed, isumstart, jsumstart, nodefloat, nsub
!   real                     :: err_max, err_tempu, err_tempv, err_init, area, max_vel, tempu, tempv, rhoi, rhow

      INTEGER i, j, k, l, iter, cg_iters, bi, bj
      INTEGER iter_numconv
      INTEGER ikey_nl, myThidTemp
      _RL err_max, err_tempu, err_tempv, err_init, area, err_max_fp
      _RL max_vel, tempu, tempv, err_lastchange, cgtol
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL CONVERGED

!       _RL taudx (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!       _RL taudy (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      IF (STREAMICE_ppm_driving_stress) THEN
       CALL STREAMICE_DRIVING_STRESS_PPM (myThid)
      ELSE
       CALL STREAMICE_DRIVING_STRESS (myThid)
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
!$TAF STORE taudx_SI = comlev1, key=ikey_dynamics
!$TAF STORE taudy_SI = comlev1, key=ikey_dynamics
#endif
#endif

#ifdef STREAMICE_STRESS_BOUNDARY_CONTROL
      _EXCH_XY_RL( taudx_SI , myThid )
      _EXCH_XY_RL( taudy_SI , myThid )
      CALL STREAMICE_FORCED_BUTTRESS (myThid)
#endif

      CALL TIMER_START ('STREAMICE_VEL_SOLVE',myThid)

      cgtol = streamice_cg_tol
      CONVERGED = .false.
      err_max = 0.
      err_max_fp = 0.

      _EXCH_XY_RL( taudx_SI , myThid )
      _EXCH_XY_RL( taudy_SI , myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          u_old_SI (i,j,bi,bj) = U_streamice (i,j,bi,bj)
          v_old_SI (i,j,bi,bj) = V_streamice (i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE U_streamice = comlev1, key=ikey_dynamics
!$TAF STORE V_streamice = comlev1, key=ikey_dynamics
#ifdef STREAMICE_HYBRID_STRESS
!$TAF STORE streamice_taubx = comlev1, key=ikey_dynamics
!$TAF STORE streamice_tauby = comlev1, key=ikey_dynamics
!$TAF STORE visc_streamice_full = comlev1, key=ikey_dynamics
#endif
#endif

#ifdef STREAMICE_HYBRID_STRESS
       CALL STREAMICE_VISC_BETA_HYBRID ( myThid )
#else
       CALL STREAMICE_VISC_BETA ( myThid )
#endif

#ifdef STREAMICE_HYBRID_STRESS
!$TAF STORE visc_streamice_full = comlev1, key=ikey_dynamics
#endif

      _EXCH_XY_RL( tau_beta_eff_streamice , myThid )
      _EXCH_XY_RL( visc_streamice , myThid )

      if (STREAMICE_chkresidconvergence .or.
     &    (streamice_maxnliter_cpl.eq.0 .and. myIter.eq.0)) then

        CALL STREAMICE_GET_VEL_RESID_ERR ( err_init, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE err_init = comlev1, key=ikey_dynamics
#endif

       WRITE(msgBuf,'(A,E15.7)') 'initial nonlinear resid (error): ',
     &                       err_init
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      endif !STREAMICE_chkresidconvergence

      iter_numconv = 0
      err_max = err_init
      err_max_fp = streamice_nonlin_tol_fp * 10.
      err_lastchange = err_init

C START NL ITER. LOOP
C-------------------------------------------------------------------

      DO iter=1,maxNLIter

C     To avoid using "exit", loop goes through all iterations
C       but after convergence loop does nothing

#ifdef ALLOW_AUTODIFF_TAMC
         ikey_nl = (ikey_dynamics-1)*streamice_max_nl + iter
#endif
#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE cgtol = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_max = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_max_fp = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_tempu = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_tempv = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_lastchange = comlev1_stream_nl, key=ikey_nl
!$TAF STORE ru_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE rv_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE u_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE v_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a1 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a2 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a3 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a4 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE tau_beta_eff_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE visc_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE zu_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE zv_old_si = comlev1_stream_nl, key=ikey_nl
#endif

!        IF ((err_max .GT. streamice_nonlin_tol * err_init) .and.
!      &     (err_max_fp .GT. streamice_nonlin_tol_fp)) THEN

#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE CONVERGED = comlev1_stream_nl, key=ikey_nl
#endif

       IF (.not.CONVERGED) THEN

       iter_numconv = iter_numconv + 1

#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE cgtol = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_max = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_tempu = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_tempv = comlev1_stream_nl, key=ikey_nl
!$TAF STORE err_lastchange = comlev1_stream_nl, key=ikey_nl
!$TAF STORE ru_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE rv_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a1 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a2 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a3 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_cg_a4 = comlev1_stream_nl, key=ikey_nl
!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE tau_beta_eff_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE visc_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE zu_old_si = comlev1_stream_nl, key=ikey_nl
!$TAF STORE zv_old_si = comlev1_stream_nl, key=ikey_nl
#endif

       CALL STREAMICE_CG_WRAPPER(
     &       U_streamice,
     &       V_streamice,
     &       taudx_SI,
     &       taudy_SI,
     &       cgtol,
     &       cg_iters,
     &       maxCGIter,
     &       myThid )

#ifdef STREAMICE_HYBRID_STRESS
#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
#endif
#endif

#ifdef STREAMICE_HYBRID_STRESS
        CALL STREAMICE_TAUB (myThid)
#endif

!-----------------------------------------------------------------------------
       WRITE(msgBuf,'(A,I5,A,I4,A)') 'streamice linear solve number',
     &                       iter, " ",
     &                       cg_iters,
     &                       ' iterations '
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
!-----------------------------------------------------------------------------

#ifdef ALLOW_AUTODIFF_TAMC
!$TAF STORE U_streamice = comlev1_stream_nl, key=ikey_nl
!$TAF STORE V_streamice = comlev1_stream_nl, key=ikey_nl
#ifdef STREAMICE_HYBRID_STRESS
!$TAF STORE streamice_taubx = comlev1_stream_nl, key=ikey_nl
!$TAF STORE streamice_tauby = comlev1_stream_nl, key=ikey_nl
!$TAF STORE visc_streamice_full = comlev1_stream_nl, key=ikey_nl
#endif
#endif

#ifdef STREAMICE_HYBRID_STRESS
       CALL STREAMICE_VISC_BETA_HYBRID ( myThid )
#else
       CALL STREAMICE_VISC_BETA ( myThid )
#endif

      _EXCH_XY_RL( tau_beta_eff_streamice , myThid )
      _EXCH_XY_RL( visc_streamice , myThid )

!----------------------CONVERGENCE TESTS-------------------------------

      if (STREAMICE_chkresidconvergence .or.
     &    (streamice_maxnliter_cpl.eq.0 .and. myIter.eq.0)) then

        CALL STREAMICE_GET_VEL_RESID_ERR ( err_max, myThid )

       WRITE(msgBuf,'(A,E15.7)') 'err/err_init',
     &                       err_max/err_init
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

       IF (err_max .LE. streamice_nonlin_tol * err_init) THEN
        CONVERGED = .true.
       ENDIF

      endif

      if (STREAMICE_chkfixedptconvergence .or.
     &    (streamice_maxnliter_cpl.eq.0 .and. myIter.eq.0)) then

        CALL STREAMICE_GET_VEL_FP_ERR ( err_max_fp, myThid )

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           u_old_SI (i,j,bi,bj) = U_streamice (i,j,bi,bj)
           v_old_SI (i,j,bi,bj) = V_streamice (i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       WRITE(msgBuf,'(A,1PE22.14)') 'STREAMICE_FP_ERROR =',
     &                       err_max_fp
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

       IF (err_max_fp .LE. streamice_nonlin_tol_fp) THEN
        CONVERGED = .true.
       ENDIF

      endif

!----------------------END CONVERGENCE TESTS-------------------------------

       IF (err_max<err_lastchange*1.e-2 .and.
     &   STREAMICE_lower_cg_tol) THEN
         cgtol = cgtol * 5.e-2
         err_lastchange = err_max
         WRITE(msgBuf,'(A,E15.7)') 'new cg tol: ',
     &                       cgtol
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
       ENDIF

       ENDIF ! (err_max .GT. streamice_nonlin_tol * err_init)
      ENDDO



C END NL ITER. LOOP
C-------------------------------------------------------------------

      if (iter_numconv .lt. streamice_max_nl_iter) then
       WRITE(msgBuf,'(A,I5,A)') 'VELOCITY SOLVE CONVERGED, ',
     &         iter_numconv, ' iterations'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      else
       WRITE(msgBuf,'(A,I5,A)') 'VELOCITY SOLVE NOT CONVERGED IN ',
     &         iter_numconv, ' iterations'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , 1)
      endif

      _EXCH_XY_RL(U_streamice, myThid)
      _EXCH_XY_RL(V_streamice, myThid)

      CALL TIMER_STOP ('STREAMICE_VEL_SOLVE',myThid)

#endif
      RETURN
      END
