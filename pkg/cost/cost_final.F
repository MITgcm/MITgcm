#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE COST_FINAL( myThid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "cost.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_GENARR.h"
#endif
#ifdef ALLOW_DIC
# include "DIC_COST.h"
#endif
#ifdef ALLOW_COST_SHELFICE
# include "SHELFICE_COST.h"
#endif

#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

c     == routine arguments ==
      INTEGER myThid

#ifdef ALLOW_COST
C     === Functions ====
#ifdef ALLOW_CTRL
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

c     == local variables ==
      INTEGER bi,bj
      _RL glob_fc, loc_fc
#ifndef ALLOW_ECCO
# ifdef ALLOW_CTRL
#  ifdef ALLOW_GENTIM2D_CONTROL
      _RL f_gentim2d(maxCtrlTim2D)
      _RL no_gentim2d(maxCtrlTim2D)
#  endif
#  ifdef ALLOW_GENARR2D_CONTROL
      _RL f_genarr2d(maxCtrlArr2D)
      _RL no_genarr2d(maxCtrlArr2D)
#  endif
#  ifdef ALLOW_GENARR3D_CONTROL
      _RL f_genarr3d(maxCtrlArr3D)
      _RL no_genarr3d(maxCtrlArr3D)
#  endif
# endif /* ALLOW_CTRL */
#endif /* ndef ALLOW_ECCO */
#ifdef ALLOW_PROFILES
      INTEGER num_file
#endif
#if defined ALLOW_PROFILES || defined ALLOW_CTRL
      INTEGER num_var
#endif
#ifdef ALLOW_CTRL
      INTEGER IL
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

#ifdef ALLOW_SEAICE
      IF (useSEAICE) CALL SEAICE_COST_FINAL( myThid )
#endif

#ifdef ALLOW_SHELFICE
      IF (useShelfice) CALL SHELFICE_COST_FINAL (myThid)
#endif

#if (defined(ALLOW_STREAMICE) && defined(ALLOW_COST_STREAMICE))
      IF (useStreamice) CALL STREAMICE_COST_FINAL (myThid)
#endif

#ifdef ALLOW_THSICE
      IF (useThSIce) CALL THSICE_COST_FINAL (myThid)
#endif

#ifndef ALLOW_ECCO
# ifdef ALLOW_CTRL
      IF (useCTRL) THEN
#  ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('CTRL_COST_DRIVER',myThid)
#  endif
       CALL TIMER_START('CTRL_COST_DRIVER [COST_FINAL]', myThid)
       CALL CTRL_COST_DRIVER( myThid )
       CALL TIMER_STOP ('CTRL_COST_DRIVER [COST_FINAL]', myThid)
C     Initialise cost function variables zo zero
#  ifdef ALLOW_GENTIM2D_CONTROL
       DO num_var=1,maxCtrlTim2D
        f_gentim2d(num_var)= 0. _d 0
        no_gentim2d(num_var)= 0. _d 0
       ENDDO
#  endif
#  ifdef ALLOW_GENARR2D_CONTROL
       DO num_var=1,maxCtrlArr2D
        f_genarr2d(num_var)= 0. _d 0
        no_genarr2d(num_var)= 0. _d 0
       ENDDO
#  endif
#  ifdef ALLOW_GENARR3D_CONTROL
       DO num_var=1,maxCtrlArr3D
        f_genarr3d(num_var)= 0. _d 0
        no_genarr3d(num_var)= 0. _d 0
       ENDDO
#  endif
      ENDIF
# endif /* ALLOW_CTRL */
#endif /* ndef ALLOW_ECCO */

#ifdef ALLOW_ECCO
      IF (useECCO) CALL ECCO_COST_FINAL (myThid)
#endif /* ALLOW_ECCO */

#ifdef ALLOW_COST_STATE_FINAL
      CALL COST_STATE_FINAL (myThid)
cgf : effectively using this in adjoint requires the
c     use of adjoint_state_final. That will activate the
c     objf_state_final vector in place of the fc scalar.
c     objf_state_final is therefore not added to fc.
#endif

#ifdef ALLOW_COST_VECTOR
cgf : same idea as for ALLOW_COST_STATE_FINAL
      CALL COST_VECTOR (myThid)
#endif

# ifdef ALLOW_COST_TEST
      CALL COST_TEST (myThid)
# endif

# ifdef ALLOW_COST_ATLANTIC_HEAT
      CALL COST_ATLANTIC_HEAT (myThid)
# endif

#ifdef ALLOW_COST_HFLUXM
      CALL COST_HFLUX (myThid)
cgf : to compile previous line user is expected to provide cost_hflux.F
#endif

#ifdef ALLOW_COST_TEMP
      CALL COST_TEMP (myThid)
cgf : to compile previous line user is expected to provide cost_temp.F
#endif

#ifdef ALLOW_COST_DEPTH
C     This makes no longer sense and should be removed. If defined
C     together with ALLOW_CTRL, this will cause recomputation warnings
C     and wrong gradient (not clear why).
# ifndef ALLOW_CTRL
      CALL COST_DEPTH( myThid )
# endif
#endif

      WRITE(msgBuf,'(A,D22.15)') '  early fc = ', fc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c--   Sum up all contributions.
      loc_fc = 0.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifndef ALLOW_ECCO
# ifdef ALLOW_GENTIM2D_CONTROL
        DO num_var=1,maxCtrlTim2D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &        + mult_gentim2d(num_var)
     &        *objf_gentim2d(bi,bj,num_var)
         f_gentim2d(num_var) = f_gentim2d(num_var)
     &        +objf_gentim2d(bi,bj,num_var)
         no_gentim2d(num_var) = no_gentim2d(num_var)
     &        +num_gentim2d(bi,bj,num_var)
        ENDDO
# endif
# ifdef ALLOW_GENARR2D_CONTROL
        DO num_var=1,maxCtrlArr2D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &        + mult_genarr2d(num_var)
     &        *objf_genarr2d(bi,bj,num_var)
         f_genarr2d(num_var) = f_genarr2d(num_var)
     &        +objf_genarr2d(bi,bj,num_var)
         no_genarr2d(num_var) = no_genarr2d(num_var)
     &        +num_genarr2d(bi,bj,num_var)
        ENDDO
# endif
# ifdef ALLOW_GENARR3D_CONTROL
        DO num_var=1,maxCtrlArr3D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &        + mult_genarr3d(num_var)
     &        *objf_genarr3d(bi,bj,num_var)
         f_genarr3d(num_var) = f_genarr3d(num_var)
     &        +objf_genarr3d(bi,bj,num_var)
         no_genarr3d(num_var) = no_genarr3d(num_var)
     &        +num_genarr3d(bi,bj,num_var)
        ENDDO
# endif
#endif /* ndef ALLOW_ECCO */

#ifdef ALLOW_COST_TEST
         WRITE(standardMessageUnit,'(A,D22.15)')
     &       ' --> objf_test(bi,bj)        = ', objf_test(bi,bj)
#endif
#ifdef ALLOW_COST_TRACER
         WRITE(standardMessageUnit,'(A,D22.15)')
     &         ' --> objf_tracer(bi,bj)    = ', objf_tracer(bi,bj)
#endif
#ifdef ALLOW_COST_ATLANTIC_HEAT
         WRITE(standardMessageUnit,'(A,D22.15)')
     &         ' --> objf_atl(bi,bj)       = ', objf_atl(bi,bj)
#endif
#ifdef ALLOW_COST_TEMP
         WRITE(standardMessageUnit,'(A,D22.15)')
     &         ' --> objf_temp_tut(bi,bj)  = ', objf_temp_tut(bi,bj)
#endif
#ifdef ALLOW_COST_HFLUXM
         WRITE(standardMessageUnit,'(A,D22.15)')
     &         ' --> objf_hflux_tut(bi,bj) = ', objf_hflux_tut(bi,bj)
#endif
#ifdef ALLOW_COST_DEPTH
         WRITE(standardMessageUnit,'(A,D22.15)')
     &         ' --> objf_depth(bi,bj)     = ', objf_depth(bi,bj)
#endif

         tile_fc(bi,bj) = tile_fc(bi,bj)
#ifdef ALLOW_COST_TEST
     &            + mult_test   * objf_test(bi,bj)
#endif
#ifdef ALLOW_COST_TRACER
     &            + mult_tracer * objf_tracer(bi,bj)
#endif
#ifdef ALLOW_COST_ATLANTIC_HEAT
     &            + mult_atl    * objf_atl(bi,bj)
#endif
#ifdef ALLOW_COST_TEMP
     &            + mult_temp_tut  * objf_temp_tut(bi,bj)
#endif
#ifdef ALLOW_COST_HFLUXM
     &            + mult_hflux_tut * objf_hflux_tut(bi,bj)
#endif
#ifdef ALLOW_COST_DEPTH
     &            + mult_depth     * objf_depth(bi,bj)
#endif

#ifdef ALLOW_PROFILES
         IF (.NOT.useECCO) THEN
          DO num_file=1,NFILESPROFMAX
           DO num_var=1,NVARMAX
            tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_profiles(num_file,num_var)
     &            *objf_profiles(num_file,num_var,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif

         loc_fc = loc_fc + tile_fc(bi,bj)

       ENDDO
      ENDDO

#ifndef ALLOW_ECCO
# ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       _GLOBAL_SUM_RL(f_gentim2d(num_var), myThid )
       _GLOBAL_SUM_RL(no_gentim2d(num_var), myThid )
       IF (no_gentim2d(num_var).GT.0. _d 0) THEN
        IL  = ILNBLNK( xx_gentim2d_file(num_var) )
        WRITE(msgBuf,'(A,D22.15,I2.0,1X,3A)')
     &     ' --> f_gentim2d =',f_gentim2d(num_var),
     &      num_var,'(',xx_gentim2d_file(num_var)(1:IL),')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
# endif
# ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       _GLOBAL_SUM_RL(f_genarr2d(num_var), myThid )
       _GLOBAL_SUM_RL(no_genarr2d(num_var), myThid )
       IF (no_genarr2d(num_var).GT.0. _d 0) THEN
        IL  = ILNBLNK( xx_genarr2d_file(num_var) )
        WRITE(msgBuf,'(A,D22.15,I2.0,1X,3A)')
     &     ' --> f_genarr2d =',f_genarr2d(num_var),
     &      num_var,'(',xx_genarr2d_file(num_var)(1:IL),')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       _GLOBAL_SUM_RL(f_genarr3d(num_var), myThid )
       _GLOBAL_SUM_RL(no_genarr3d(num_var), myThid )
       IF (no_genarr3d(num_var).GT.0. _d 0) THEN
        IL  = ILNBLNK( xx_genarr3d_file(num_var) )
        WRITE(msgBuf,'(A,D22.15,I2.0,1X,3A)')
     &     ' --> f_genarr3d =',f_genarr3d(num_var),
     &      num_var,'(',xx_genarr3d_file(num_var)(1:IL),')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
# endif
#endif /* ndef ALLOW_ECCO */

      WRITE(msgBuf,'(A,D22.15)') '  local fc = ', loc_fc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c--   Do global summation.
      CALL GLOBAL_SUM_TILE_RL( tile_fc, glob_fc, myThid )
      _BEGIN_MASTER( myThid )
      fc = fc + glob_fc
      _END_MASTER( myThid )

c--   Add contributions from global mean constraints
      _BEGIN_MASTER( myThid )
      fc = fc + glofc
      _END_MASTER( myThid )

#ifdef ALLOW_DIC_COST
cph-- quickly for testing
      fc = totcost
#endif

      WRITE(msgBuf,'(A,D22.15)') ' global fc = ', fc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c--   to avoid re-write of output in reverse checkpointing loops,
c--   switch off output flag :
      CALL TURNOFF_MODEL_IO( 0, myThid )

#endif /* ALLOW_COST */

      RETURN
      END
