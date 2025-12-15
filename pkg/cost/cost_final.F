#include "COST_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: COST_FINAL

C !INTERFACE:
      SUBROUTINE COST_FINAL( myThid )

C     !DESCRIPTION:
C     Sum of all cost function contributions.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "cost.h"
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
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
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_COST
C     !FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD

C     !LOCAL VARIABLES:
      INTEGER bi,bj
      _RL glob_fc, loc_fc
#ifndef ALLOW_CTRL
C     dummy parameter for this unlikely case
      INTEGER optimcycle
#endif
      INTEGER ifc
      CHARACTER*16 cfname
#ifdef ALLOW_OBSFIT
      INTEGER num_file_obs
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      WRITE(msgBuf,'(A)')
     &  '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// Start of S/R COST_FINAL'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &  '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

#ifndef ALLOW_CTRL
C     dummy value for this unlikely case
      optimcycle = 0
#endif

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

C     It is important to do the ecco part first, because it creates a
C     file costfunctionXXXX that will be reopened again by the ctrl and
C     obcs contributions if available.
#ifdef ALLOW_ECCO
      IF (useECCO) CALL ECCO_COST_FINAL (myThid)
#endif

#ifdef ALLOW_PROFILES
      IF (usePROFILES) CALL PROFILES_COST_FINAL( myThid )
#endif

#ifdef ALLOW_CTRL
      IF (useCTRL) CALL CTRL_COST_FINAL( myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) CALL OBCS_COST_FINAL( myThid )
#endif

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

      WRITE(msgBuf,'(A,D22.15)') '  early fc = ', fc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c--   Sum up all other contributions.
      loc_fc = 0.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

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

         loc_fc = loc_fc + tile_fc(bi,bj)

       ENDDO
      ENDDO

      WRITE(msgBuf,'(A,D22.15)') '  local fc = ', loc_fc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_OBSFIT
      IF (.NOT.useECCO) THEN
       DO num_file_obs=1,NFILESMAX_OBS
        glofc = glofc
     &          +  mult_obsfit(num_file_obs)
     &            *objf_obsfit(num_file_obs)
       ENDDO
      ENDIF
#endif

c--   Do global summation.
      CALL GLOBAL_SUM_TILE_RL( tile_fc, glob_fc, myThid )
      _BEGIN_MASTER( myThid )
      fc = fc + glob_fc

c--   Add contributions from global mean constraints
      fc = fc + glofc
      _END_MASTER( myThid )

#ifdef ALLOW_DIC_COST
cph-- quickly for testing
      fc = totcost
#endif

      IF ( MASTER_CPU_THREAD(myThid) .AND. costWriteCostFunction ) THEN
       ifc = 30
       WRITE(cfname,'(A,I4.4)') 'costfunction',optimcycle
       WRITE(msgBuf,'(A,A)')
     &      'Writing global cost function info to ', cfname
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       OPEN(UNIT=ifc,FILE=cfname)
       WRITE(UNIT=ifc,FMT='(A,2D22.15)')  'fc =', fc, 0.
C     Collect and copy other cost function contributions in a separate
C     file to hide irreducible statements from TAF
       CALL COST_COPY_FILE(ifc, myThid)

       CLOSE(ifc)
C     Do not ever write this cost function again to cfname
       costWriteCostFunction = .FALSE.
      ENDIF

      WRITE(msgBuf,'(A,D22.15)') ' global fc = ', fc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(A)')
     &  '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') '// End of S/R COST_FINAL'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &  '// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

c--   to avoid re-write of output in reverse checkpointing loops,
c--   switch off output flag :
      CALL TURNOFF_MODEL_IO( 0, myThid )

#endif /* ALLOW_COST */

      RETURN
      END
