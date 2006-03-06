C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_ocean_stress.F,v 1.3 2006/03/06 20:53:25 mlosch Exp $
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
      _RL  SINWAT, COSWAT
      _RL  fuIce, fvIce

c     introduce turning angle (default is zero)
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C--   Update overlap regions
      CALL EXCH_UV_XY_RL(WINDX, WINDY, .TRUE., myThid)

#ifndef SEAICE_EXTERNAL_FLUXES
C--   Interpolate wind stress (N/m^2) from C-points of C-grid
C     to U and V points of C-grid for forcing the ocean model.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fu(I,J,bi,bj)=0.5*(WINDX(I,J,bi,bj) + WINDX(I-1,J,bi,bj))
          fv(I,J,bi,bj)=0.5*(WINDY(I,J,bi,bj) + WINDY(I,J-1,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ifndef SEAICE_EXTERNAL_FLUXES */

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

#endif /* not SEAICE_CGRID */

      RETURN
      END
