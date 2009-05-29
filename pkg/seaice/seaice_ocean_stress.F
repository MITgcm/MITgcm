C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ocean_stress.F,v 1.24 2009/05/29 14:51:21 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_OCEAN_STRESS(
     I     myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_OCEAN_STRESS                           |
C     | o Calculate ocean surface stresses                       |
C     |   - C-grid version                                       |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_CGRID
C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  SINWAT, COSWAT, SINWIN, COSWIN
      _RL  fuIceLoc, fvIceLoc
      _RL  areaW, areaS

c     introduce turning angle (default is zero)
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

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
           areaW = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I-1,J,1,bi,bj))
     &          * SEAICEstressFactor
           areaS = 0.5 * (AREA(I,J,1,bi,bj) + AREA(I,J-1,1,bi,bj))
     &          * SEAICEstressFactor
           fu(I,J,bi,bj)=(ONE-areaW)*fu(I,J,bi,bj)
     &          + areaW*taux(I,J,bi,bj)
     &          + stressDivergenceX(I,J,bi,bj) * SEAICEstressFactor
           fv(I,J,bi,bj)=(ONE-areaS)*fv(I,J,bi,bj)
     &          + areaS*tauy(I,J,bi,bj)
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
     &         ( UICE(I,J,1,bi,bj)-uVel(I,J,1,bi,bj) )
     &         - SIGN(SINWAT, _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I  ,J,bi,bj) *
     &         0.5 _d 0*(vIce(I  ,J  ,1,bi,bj)-vVel(I  ,J  ,1,bi,bj)
     &                  +vIce(I  ,J+1,1,bi,bj)-vVel(I  ,J+1,1,bi,bj))
     &         + DWATN(I-1,J,bi,bj) *
     &         0.5 _d 0*(vIce(I-1,J  ,1,bi,bj)-vVel(I-1,J  ,1,bi,bj)
     &                  +vIce(I-1,J+1,1,bi,bj)-vVel(I-1,J+1,1,bi,bj))
     &         )
          fvIceLoc=HALF*( DWATN(I,J,bi,bj)+DWATN(I,J-1,bi,bj) )*
     &         COSWAT *
     &         ( VICE(I,J,1,bi,bj)-vVel(I,J,1,bi,bj) )
     &         + SIGN(SINWAT,  _fCori(I,J,bi,bj)) * 0.5 _d 0 *
     &         ( DWATN(I,J  ,bi,bj) *
     &         0.5 _d 0*(uIce(I  ,J  ,1,bi,bj)-uVel(I  ,J  ,1,bi,bj)
     &                  +uIce(I+1,J  ,1,bi,bj)-uVel(I+1,J  ,1,bi,bj))
     &         + DWATN(I,J-1,bi,bj) *
     &         0.5 _d 0*(uIce(I  ,J-1,1,bi,bj)-uVel(I  ,J-1,1,bi,bj)
     &                  +uIce(I+1,J-1,1,bi,bj)-uVel(I+1,J-1,1,bi,bj))
     &         )
          areaW = 0.5 _d 0 * (AREA(I,J,1,bi,bj) + AREA(I-1,J,1,bi,bj))
     &         * SEAICEstressFactor
          areaS = 0.5 _d 0 * (AREA(I,J,1,bi,bj) + AREA(I,J-1,1,bi,bj))
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
