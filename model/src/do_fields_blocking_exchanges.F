#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_FIELDS_BLOCKING_EXCHANGES
C     !INTERFACE:
      SUBROUTINE DO_FIELDS_BLOCKING_EXCHANGES(myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_FIELDS_BLOCKING_EXCHANGES
C     | o Controlling routine for exchanging edge info.
C     *==========================================================*
C     | One key trick used in UV us that we over-compute and
C     | arrange our time-stepping loop so that we only need one
C     | edge exchange for the explicit code per timestep.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_LONGSTEP
#include "LONGSTEP.h"
#endif
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_CD_CODE
# include "CD_CODE_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     bi, bj :: tile indices
#ifdef ALLOW_OBCS
c     INTEGER bi, bj
#endif /* ALLOW_OBCS */

      IF ( .NOT.useOffLine ) THEN
C-    Apply Exchanges on dynamics state variable, except in Off-Line mode

      IF ( .NOT.staggerTimeStep ) THEN
       IF ( .NOT.applyExchUV_early )
     &  CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
       IF ( .NOT.implicitIntGravWave )
     & _EXCH_XYZ_RL( wVel , myThid )
      ENDIF
c     IF ( exactConserv .AND. implicDiv2Dflow .NE. 0. _d 0)
c    & _EXCH_XY_RL( etaN , myThid )
      IF ( .NOT.implicitIntGravWave ) THEN
       _EXCH_XYZ_RL( theta, myThid )
       _EXCH_XYZ_RL( salt , myThid )
      ENDIF
#ifdef ALLOW_OBCS
c     IF ( useOBCS ) THEN
c      DO bj = myByLo(myThid), myByHi(myThid)
c       DO bi = myBxLo(myThid), myBxHi(myThid)
c        CALL OBCS_COPY_TRACER( theta(1-OLx,1-OLy,1,bi,bj),
c    I                          Nr, bi, bj, myThid )
c        CALL OBCS_COPY_TRACER( salt (1-OLx,1-OLy,1,bi,bj),
c    I                          Nr, bi, bj, myThid )
c       ENDDO
c      ENDDO
c     ENDIF
#endif /* ALLOW_OBCS */

#ifdef ALLOW_GENERIC_ADVDIFF
      IF ( tempSOM_Advection .OR. saltSOM_Advection )
     &  CALL GAD_SOM_EXCHANGES( myThid )
#endif

#ifdef ALLOW_CD_CODE
      CALL EXCH_UV_DGRID_3D_RL( uVelD,vVelD, .TRUE., Nr, myThid )
#endif
      IF ( storePhiHyd4Phys )
     & _EXCH_XYZ_RL( totPhiHyd , myThid )
C-    if not useOffLine: end
      ENDIF

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_LONGSTEP
      IF ( LS_doTimeStep ) THEN
#else
      IF ( .TRUE. ) THEN
#endif
      IF (usePTRACERS)
     &  CALL PTRACERS_FIELDS_BLOCKING_EXCH(myThid)
C     endif LS_doTimeStep
      ENDIF
#endif /* ALLOW PTRACERS */

      RETURN
      END
