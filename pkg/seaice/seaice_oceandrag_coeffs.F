#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_OCEANDRAG_COEFFS
C     !INTERFACE:
      SUBROUTINE SEAICE_OCEANDRAG_COEFFS(
     I     uIceLoc, vIceLoc, HEFFMLoc,
     O     CwatC,
     I     iStep, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_OCEANDRAG_COEFFS
C     | o Compute the drag coefficients for ice-ocean drag,
C     |   so that we can use the same code for different solvers
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     iStep  :: current sub-time step iterate
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iStep
C     u/vIceLoc :: local copies of the current ice velocity
      _RL uIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     HEFFMLoc  :: local copy of land-sea masks
      _RL HEFFMLoc  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     CwatC     :: drag coefficients
      _RL CwatC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if ( (defined SEAICE_CGRID) && (defined SEAICE_ALLOW_DYNAMICS) )
C     === local variables ===
C     i,j,bi,bj,ksrf :: loop indices
      INTEGER i,j,bi,bj
      INTEGER kSrf
      _RL     tempVar, tempMin
      _RL     dragCoeff
CEOP

      IF ( usingPCoords ) THEN
       kSrf = Nr
      ELSE
       kSrf = 1
      ENDIF
      tempMin = SEAICEdWatMin*SEAICEdWatMin

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy-1
         DO i=1-OLx,sNx+OLy-1
C     non-linear water drag coefficients CwatC (DWATN)
#ifdef OBCS_UVICE_OLD
          tempVar = 0.25 _d 0*(
     &            ( ( uIceLoc(i  ,j,bi,bj)-uVel(i  ,j,kSrf,bi,bj) )
     &             +( uIceLoc(i+1,j,bi,bj)-uVel(i+1,j,kSrf,bi,bj) )
     &            )**2
     &          + ( ( vIceLoc(i, j ,bi,bj)-vVel(i, j ,kSrf,bi,bj) )
     &             +( vIceLoc(i,j+1,bi,bj)-vVel(i,j+1,kSrf,bi,bj) )
     &            )**2 )
#else /* OBCS_UVICE_OLD */
          tempVar = 0.25 _d 0*(
     &            ( ( uIceLoc(i  ,j,bi,bj)-uVel(i  ,j,kSrf,bi,bj) )
     &             *maskInW( i ,j,bi,bj)
     &             +( uIceLoc(i+1,j,bi,bj)-uVel(i+1,j,kSrf,bi,bj) )
     &             *maskInW(i+1,j,bi,bj) )**2
     &          + ( ( vIceLoc(i,j  ,bi,bj)-vVel(i,j  ,kSrf,bi,bj) )
     &             *maskInS(i, j ,bi,bj)
     &             +( vIceLoc(i,j+1,bi,bj)-vVel(i,j+1,kSrf,bi,bj) )
     &             *maskInS(i,j+1,bi,bj) )**2 )
#endif /* OBCS_UVICE_OLD */
          IF ( YC(i,j,bi,bj) .LT. ZERO ) THEN
           dragCoeff = SEAICE_waterDrag_south*rhoConst
          ELSE
           dragCoeff = SEAICE_waterDrag      *rhoConst
          ENDIF
          CwatC(i,j,bi,bj) = SEAICEdWatMin
C     This is the AD-proof version of
C         IF ( dragCoeff * SQRT(tempVar) .GT. SEAICEdWatMin )
C     which was previously implemented as
C         IF ( tempVar .GT. (SEAICEdWatMin/dragCoeff)**2 )
C     I like this better:
          IF ( dragCoeff*dragCoeff * tempVar .GT. tempMin )
     &         CwatC(i,j,bi,bj) = dragCoeff*SQRT(tempVar)
          CwatC(i,j,bi,bj) = CwatC(i,j,bi,bj) * HEFFMLoc(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */

      RETURN
      END
