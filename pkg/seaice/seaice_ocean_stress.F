#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_OCEAN_STRESS
C     !INTERFACE:
      SUBROUTINE SEAICE_OCEAN_STRESS(
     I                  windTauX, windTauY,
     I                  myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_OCEAN_STRESS                           |
C     | o Calculate ocean surface stresses                       |
C     |   - C-grid version                                       |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     windTauX :: X-direction wind stress over seaice at U point
C     windTauY :: Y-direction wind stress over seaice at V point
C     myTime   :: Current time in simulation
C     myIter   :: Current iteration number
C     myThid   :: my Thread Id number
      _RL     windTauX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     windTauY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef SEAICE_CGRID
C     !LOCAL VARIABLES:
C     i,j,bi,bj :: Loop counters
C     kSrf      :: vertical index of surface layer
      INTEGER i, j, bi, bj
      INTEGER kSrf
      _RL  COSWAT
      _RS  SINWAT
      _RL  fuIceLoc, fvIceLoc
      _RL  areaW, areaS
CEOP

C     surrface level
      IF ( usingPCoords ) THEN
       kSrf = Nr
      ELSE
       kSrf = 1
      ENDIF
C     introduce turning angle (default is zero)
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

      IF ( useHB87StressCoupling ) THEN
C
C     use an intergral over ice and ocean surface layer to define
C     surface stresses on ocean following Hibler and Bryan (1987, JPO)
C
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO J=1,sNy
          DO I=1,sNx
C     average wind stress over ice and ocean and apply averaged wind
C     stress and internal ice stresses to surface layer of ocean
           areaW = 0.5 * (AREA(I,J,bi,bj) + AREA(I-1,J,bi,bj))
     &          * SEAICEstressFactor
           fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)
     &          + areaW*windTauX(I,J,bi,bj)
     &          + stressDivergenceX(I,J,bi,bj) * SEAICEstressFactor
          ENDDO
         ENDDO
C     This loop separation makes the adjoint code vectorize
         DO J=1,sNy
          DO I=1,sNx
           areaS = 0.5 * (AREA(I,J,bi,bj) + AREA(I,J-1,bi,bj))
     &          * SEAICEstressFactor
           fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)
     &          + areaS*windTauY(I,J,bi,bj)
     &          + stressDivergenceY(I,J,bi,bj) * SEAICEstressFactor
          ENDDO
         ENDDO
        ENDDO
       ENDDO

      ELSE
C     else: useHB87StressCoupling=F

C--   Compute ice-affected wind stress (interpolate to U/V-points)
C     by averaging wind stress and ice-ocean stress according to
C     ice cover
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fuIceLoc=HALF*( DWATN(I,J,bi,bj)+DWATN(I-1,J,bi,bj) )*
     &         COSWAT *
     &         ( uIce(I,J,bi,bj)-uVel(I,J,kSrf,bi,bj) )
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) *
     &         0.5 _d 0*(vIce(I  ,J  ,bi,bj)-vVel(I  ,J  ,kSrf,bi,bj)
     &                  +vIce(I  ,J+1,bi,bj)-vVel(I  ,J+1,kSrf,bi,bj))
     &         + DWATN(I-1,J,bi,bj) *
     &         0.5 _d 0*(vIce(I-1,J  ,bi,bj)-vVel(I-1,J  ,kSrf,bi,bj)
     &                  +vIce(I-1,J+1,bi,bj)-vVel(I-1,J+1,kSrf,bi,bj))
     &         )
          fvIceLoc=HALF*( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) )*
     &         COSWAT *
     &         ( vIce(I,J,bi,bj)-vVel(I,J,kSrf,bi,bj) )
     &         + SIGN(SINWAT,  _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) *
     &         0.5 _d 0*(uIce(I  ,J  ,bi,bj)-uVel(I  ,J  ,kSrf,bi,bj)
     &                  +uIce(I+1,J  ,bi,bj)-uVel(I+1,J  ,kSrf,bi,bj))
     &         + DWATN(I,J-1,bi,bj) *
     &         0.5 _d 0*(uIce(I  ,J-1,bi,bj)-uVel(I  ,J-1,kSrf,bi,bj)
     &                  +uIce(I+1,J-1,bi,bj)-uVel(I+1,J-1,kSrf,bi,bj))
     &         )
          areaW = 0.5 _d 0 * (AREA(I,J,bi,bj) + AREA(I-1,J,bi,bj))
     &         * SEAICEstressFactor
          areaS = 0.5 _d 0 * (AREA(I,J,bi,bj) + AREA(I,J-1,bi,bj))
     &         * SEAICEstressFactor
          fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)+areaW*fuIceLoc
          fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)+areaS*fvIceLoc
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)

#endif /* SEAICE_CGRID */

      RETURN
      END
