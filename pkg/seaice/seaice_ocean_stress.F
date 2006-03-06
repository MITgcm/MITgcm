C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ocean_stress.F,v 1.2 2006/03/06 13:27:40 mlosch Exp $
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
CML      _RL COR_ICE    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
CEndOfInterface

#ifdef SEAICE_CGRID 
C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  SINWIN, COSWIN, SINWAT, COSWAT
#ifdef SEAICE_TEST_ICE_STRESS_1
      _RL  fuIce, fvIce
#endif

c     introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

CML#ifdef SEAICE_ORIGINAL_BAD_ICE_STRESS
CMLC--   Following formulation is problematic and is no longer used.
CML#ifdef SEAICE_ALLOW_DYNAMICS
CML      IF ( SEAICEuseDYNAMICS ) THEN
CMLC--   Compute ice-affected wind stress
CML       DO bj=myByLo(myThid),myByHi(myThid)
CML        DO bi=myBxLo(myThid),myBxHi(myThid)
CML         DO j=1,sNy
CML          DO i=1,sNx
CML           WINDX(I,J,bi,bj)=DWATN(I,J,bi,bj)
CML     &          *(COSWAT*(GWATX(I,J,bi,bj)-UICE(I,J,1,bi,bj))
CML     &          -SINWAT*(GWATY(I,J,bi,bj)-VICEC(I,J,bi,bj)))
CML           WINDY(I,J,bi,bj)=DWATN(I,J,bi,bj)
CML     &          *(SINWAT*(GWATX(I,J,bi,bj)-UICEC(I,J,bi,bj))
CML     &          +COSWAT*(GWATY(I,J,bi,bj)-VICE(I,J,1,bi,bj)))
CML           WINDX(I,J,bi,bj)=WINDX(I,J,bi,bj)-( COR_ICE(I,J,bi,bj)
CML     &          *GWATY(I,J,bi,bj)-COR_ICE(I,J,bi,bj)*VICEC(I,J,bi,bj))
CML           WINDY(I,J,bi,bj)=WINDY(I,J,bi,bj)-(-COR_ICE(I,J,bi,bj)
CML     &          *GWATX(I,J,bi,bj)+COR_ICE(I,J,bi,bj)*UICEC(I,J,bi,bj))
CML           WINDX(I,J,bi,bj)=WINDX(I,J,bi,bj)-(UICE(I,J,1,bi,bj)
CML     &          -UICE(I,J,3,bi,bj))*AMASS(I,J,bi,bj)/SEAICE_DT*TWO
CML           WINDY(I,J,bi,bj)=WINDY(I,J,bi,bj)-(VICE(I,J,1,bi,bj)
CML     &          -VICE(I,J,3,bi,bj))*AMASS(I,J,bi,bj)/SEAICE_DT*TWO
CML          ENDDO
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML       DO bj=myByLo(myThid),myByHi(myThid)
CML        DO bi=myBxLo(myThid),myBxHi(myThid)
CML         DO j=1,sNy
CML          DO i=1,sNx
CML           WINDX(I,J,bi,bj)=-WINDX(I,J,bi,bj)
CML           WINDY(I,J,bi,bj)=-WINDY(I,J,bi,bj)
CML          ENDDO
CML         ENDDO
CML        ENDDO
CML       ENDDO
CML      ENDIF
CML#endif /*  SEAICE_ALLOW_DYNAMICS */
CML#endif /* SEAICE_ORIGINAL_BAD_ICE_STRESS */

C--   Update overlap regions
      CALL EXCH_UV_XY_RL(WINDX, WINDY, .TRUE., myThid)

#ifndef SEAICE_EXTERNAL_FLUXES
C--   Interpolate wind stress (N/m^2) from South-West B-grid
C     to South-West C-grid for forcing ocean model.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
            fu(I,J,bi,bj)=WINDX(I,J,bi,bj)
            fv(I,J,bi,bj)=WINDY(I,J,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#endif /* ifndef SEAICE_EXTERNAL_FLUXES */

#ifdef SEAICE_TEST_ICE_STRESS_1
C--   Compute ice-affected wind stress (interpolate to U/V-points)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fuIce=HALF*( DWATN(I,J,bi,bj)+DWATN(I,J+1,bi,bj) )*(
     &         COSWAT * 
     &         ( UICE(I,J,1,bi,bj)-GWATX(I,J,bi,bj) )
     &         - SINWAT* 0.5 _d 0 * (
     &          0.5 _d 0*(vIce(I  ,J  ,1,bi,bj)-GWATY(I  ,J  ,bi,bj)
     &                   +vIce(I-1,J  ,1,bi,bj)-GWATY(I-1,J  ,bi,bj))
     &         +0.5 _d 0*(vIce(I  ,J+1,1,bi,bj)-GWATY(I  ,J+1,bi,bj)
     &                   +vIce(I-1,J+1,1,bi,bj)-GWATY(I-1,J+1,bi,bj)) )
     &         )
          fvIce=HALF*( DWATN(I,J,bi,bj)+DWATN(I+1,J,bi,bj) )*(
     &         SINWAT * 
     &         ( UICE(I,J,1,bi,bj)-GWATX(I,J,bi,bj) )
     &         + COSWAT * 0.5 _d 0 * ( 
     &          0.5 _d 0*(uIce(I  ,J  ,1,bi,bj)-GWATX(I  ,J  ,bi,bj)
     &                   +uIce(I+1,J  ,1,bi,bj)-GWATX(I+1,J  ,bi,bj))
     &         +0.5 _d 0*(uIce(I  ,J-1,1,bi,bj)-GWATX(I  ,J-1,bi,bj) 
     &                   +uIce(I+1,J-1,1,bi,bj)-GWATX(I+1,J-1,bi,bj)) )
     &         )
          fu(I,J,bi,bj)=(ONE-AREA(I,J,1,bi,bj))*fu(I,J,bi,bj)+
     &         AREA(I,J,1,bi,bj)*fuIce
          fv(I,J,bi,bj)=(ONE-AREA(I,J,1,bi,bj))*fv(I,J,bi,bj)+
     &         AREA(I,J,1,bi,bj)*fvIce
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#endif /* SEAICE_TEST_ICE_STRESS_1 */
#endif /* not SEAICE_CGRID */

      RETURN
      END
