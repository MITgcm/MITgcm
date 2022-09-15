#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_TIMESTEP ( myThid, myIter,
     &                                        iLoop, myTime )

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#ifdef ALLOW_AUTODIFF
# include "STREAMICE_ADV.h"
# include "STREAMICE_BDRY.h"
# include "STREAMICE_CG.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

      INTEGER myThid, myIter, iLoop
      _RL myTime
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj, ki, kj
c       _RL Iratio, Imin_ratio, time_step_remain, local_u_max
c       _RL ratio, min_ratio
c       _RL local_v_max, time_step_int, min_time_step
      CHARACTER*(MAX_LEN_MBUF) msgBuf
       LOGICAL do_vel, tmp_residcheck, tmp_fpcheck
       _RL sum_square_vel_tile (nSx,nSy)
       _RL sum_square_vel

#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "streamice_ad_check_lev1_dir.h"
c**************************************
#endif

c       time_step_remain = deltaT
c       min_time_step = 1000.0
c       n_interm = 0

      do_vel = .false.

#ifdef ALLOW_AUTODIFF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          STREAMICE_ufacemask(i,j,bi,bj) = 0. _d 0
          STREAMICE_vfacemask(i,j,bi,bj) = 0. _d 0
          ru_old_si(i,j,bi,bj) = 0. _d 0
          rv_old_si(i,j,bi,bj) = 0. _d 0
          zu_old_si(i,j,bi,bj) = 0. _d 0
          zv_old_si(i,j,bi,bj) = 0. _d 0
c          h_after_uflux_si(i,j,bi,bj) = 0. _d 0
#ifdef STREAMICE_HYBRID_STRESS
          streamice_taubx (i,j,bi,bj) = 0. _d 0
          streamice_tauby (i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_AUTODIFF */

      CALL TIMER_START('STREAMICE_TIMESTEP [FORWARD_STEP]',
     &       myThid)

      WRITE(msgBuf,'(A,I10.10,E9.2,A)')
     &  'streamice solo_time_step: nIter',
     &  myIter, myTime/86400.0/365.0, 'seconds'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

      CALL STREAMICE_DUMP( myTime, myIter, myThid )

c NEW DIRECTIVES - DNG
c#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE float_frac_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE surf_el_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
cCADJ STORE base_el_streamice  = comlev1, key = ikey_dynamics,
cCADJ &     kind = isbyte
c#endif
c NEW DIRECTIVES - DNG

      do_vel = DIFFERENT_MULTIPLE( streamice_vel_update,
     &                             myTime, deltaT )

      IF (myIter.eq.0) THEN
      CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )
      CALL WRITE_FLD_XY_RL
     & ("surf_el_init","",surf_el_streamice,0,myThid)
      ENDIF

      CALL STREAMICE_VELMASK_UPD (myThid)

#ifdef ALLOW_STREAMICE_TIMEDEP_FORCING
      CALL STREAMICE_FIELDS_LOAD( myTime, myIter, myThid )
#endif

      IF (.not.(STREAMICE_vel_ext.OR.STREAMICE_vel_ext_cgrid)) THEN

#if (defined (ALLOW_STREAMICE_OAD_FP))

       CALL STREAMICE_VEL_SOLVE_OPENAD ( myThid,
     &                           streamice_max_nl_iter,
     &                           streamice_max_cg_iter,
     &                           myIter )

#else

       IF (streamice_maxnliter_cpl.eq.0 .OR. myIter.eq.0) THEN

        CALL STREAMICE_VEL_SOLVE( myThid,
     &                           streamice_max_nl_iter,
     &                           streamice_max_cg_iter,
     &                           myIter )

#ifdef STREAMICE_ECSECRYO_DOSUM

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          sum_square_vel_tile (bi,bj) = 0. _d 0
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            IF (streamice_hmask(i,j,bi,bj).eq.1) THEN
             sum_square_vel_tile (bi,bj) =
     &        sum_square_vel_tile (bi,bj) +
     &        U_streamice(i,j,bi,bj)**2 +
     &        V_streamice(i,j,bi,bj)**2
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        CALL GLOBAL_SUM_TILE_RL(
     &                sum_square_vel_tile, sum_square_vel, myThid )
        WRITE(msgBuf,'(A,I3,A,1PE22.14)') 'ECSE_CRYO_SUM ', myIter, ', '
     &                       ,sum_square_vel
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

#endif

       ELSEIF (do_vel) THEN

        CALL STREAMICE_VEL_SOLVE( myThid,
     &                           streamice_maxnliter_cpl,
     &                           streamice_maxcgiter_cpl,
     &                           myIter )
       ENDIF

#endif /* OAD_FP */

      ELSE

        WRITE(msgBuf,'(A)')
     &   'STREAMICE_TIMESTEP: bypassing velocity solve'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      ENDIF

      IF (.not.STREAMICE_diagnostic_only) THEN

       CALL STREAMICE_ADVECT_THICKNESS ( myThid, myIter, deltaT )

      ENDIF

c CALL AT END INSTEAD OF BEGINNING - DNG
      CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )
c      print *, "GOT HERE TIMESTEP ", H_streamice(1,50,1,1)
c      call write_fld_xy_rl("h_got_here","",H_streamice,0,myThid)
c      call write_fld_xy_rl("u_got_here","",U_streamice,0,myThid)
c      call write_fld_xy_rl("v_got_here","",V_streamice,0,myThid)

      CALL TIMER_STOP('STREAMICE_TIMESTEP [FORWARD_STEP]',
     &       myThid)

#endif
      RETURN
      END
