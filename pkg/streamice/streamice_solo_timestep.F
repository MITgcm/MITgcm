C $Header: /u/gcmpack/MITgcm/pkg/streamice/Attic/streamice_solo_timestep.F,v 1.2 2014/04/24 12:01:50 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_SOLO_TIMESTEP ( myThid, myIter,
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
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "STREAMICE_ADV.h"
# include "STREAMICE_BDRY.h"
# include "STREAMICE_CG.h"
#endif


      INTEGER myThid, myIter, iLoop
      _RL myTime
      

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj, ki, kj
!       _RL Iratio, Imin_ratio, time_step_remain, local_u_max
!       _RL ratio, min_ratio
!       _RL local_v_max, time_step_int, min_time_step
      CHARACTER*(MAX_LEN_MBUF) msgBuf
!       LOGICAL modelEnd

#ifdef ALLOW_AUTODIFF_TAMC
c**************************************
#include "streamice_ad_check_lev1_dir.h"
c**************************************
#endif

!       time_step_remain = deltaT    
!       min_time_step = 1000.0
!       n_interm = 0

#ifdef ALLOW_AUTODIFF_TAMC

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
!          h_after_uflux_si(i,j,bi,bj) = 0. _d 0
#ifdef STREAMICE_HYBRID_STRESS
          streamice_taubx (i,j,bi,bj) = 0. _d 0
          streamice_tauby (i,j,bi,bj) = 0. _d 0
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif



      WRITE(msgBuf,'(A,I10.10,E9.2,A)') 
     &  'streamice solo_time_step: nIter',
     &  myIter, myTime/86400.0/365.0, 'seconds'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

      CALL STREAMICE_DUMP( mytime, myiter, myThid )

! NEW DIRECTIVES - DNG
!#ifdef ALLOW_AUTODIFF_TAMC
!CADJ STORE float_frac_streamice  = comlev1, key = ikey_dynamics,
!CADJ &     kind = isbyte
!CADJ STORE surf_el_streamice  = comlev1, key = ikey_dynamics,
!CADJ &     kind = isbyte
!CADJ STORE base_el_streamice  = comlev1, key = ikey_dynamics,
!CADJ &     kind = isbyte
!#endif
! NEW DIRECTIVES - DNG

!#ifdef ALLOW_GENTIM2D_CONTROL
!      CALL CTRL_MAP_GENTIM2D (myTime, myIter, myThid)
!#endif

      if (myIter.eq.0) then
      CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )
      CALL WRITE_FLD_XY_RL
     & ("surf_el_init","",surf_el_streamice,0,myThid)
      endif
      CALL STREAMICE_VELMASK_UPD (myThid)

#ifdef ALLOW_STREAMICE_FLUX_CONTROL
      CALL STREAMICE_APPLY_FLUX_CTRL ( myTime, myIter, myThid )
#endif      

      CALL STREAMICE_VEL_SOLVE( myThid )
      
      if(.not.STREAMICE_diagnostic_only) THEN

       CALL STREAMICE_ADVECT_THICKNESS ( myThid, myIter, deltaT )  

      endif

! CALL AT END INSTEAD OF BEGINNING - DNG
      CALL STREAMICE_UPD_FFRAC_UNCOUPLED ( myThid )




#endif
      RETURN
      END
