C $Header: /u/gcmpack/MITgcm/pkg/seaice/ostres.F,v 1.25 2012/03/06 16:45:20 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ostres( COR_ICE, myThid )
C     *==========================================================*
C     | SUBROUTINE ostres                                        |
C     | o Calculate ocean surface stress                         |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      _RL COR_ICE        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      INTEGER myThid
CEndOfInterface

#ifndef SEAICE_CGRID
C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  SINWIN, COSWIN, SINWAT, COSWAT
#ifdef SEAICE_BICE_STRESS
      _RL  fuIce, fvIce
#endif

c     introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)
      SINWAT=SIN(SEAICE_waterTurnAngle*deg2rad)
      COSWAT=COS(SEAICE_waterTurnAngle*deg2rad)

C--   Update overlap regions
      CALL EXCH_UV_XY_RL(WINDX, WINDY, .TRUE., myThid)

#ifndef SEAICE_EXTERNAL_FLUXES
C--   Interpolate wind stress (N/m^2) from South-West B-grid
C     to South-West C-grid for forcing ocean model.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
            fu(I,J,bi,bj)=HALF
     &           *(WINDX(I,J+1,bi,bj)+WINDX(I,J,bi,bj))
            fv(I,J,bi,bj)=HALF
     &           *(WINDY(I+1,J,bi,bj)+WINDY(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#endif /* ifndef SEAICE_EXTERNAL_FLUXES */

#ifdef SEAICE_BICE_STRESS
C--   Compute ice-affected wind stress
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fuIce=QUART*( DWATN(I,J,bi,bj)+DWATN(I,J+1,bi,bj) )*(
     &         COSWAT *
     &         ( UICE(I,J,  bi,bj)-GWATX(I,J,  bi,bj)
     &         + UICE(I,J+1,bi,bj)-GWATX(I,J+1,bi,bj) )
     &         -SIGN(SINWAT,COR_ICE(I,J,bi,bj)) *
     &         ( VICE(I,J,  bi,bj)-GWATY(I,J,  bi,bj)
     &         + VICE(I,J+1,bi,bj)-GWATY(I,J+1,bi,bj) )
     &         )
          fvIce=QUART*( DWATN(I,J,bi,bj)+DWATN(I+1,J,bi,bj) )*(
     &         SIGN(SINWAT,COR_ICE(I,J,bi,bj)) *
     &         ( UICE(I,  J,bi,bj)-GWATX(I,  J,bi,bj)
     &         + UICE(I+1,J,bi,bj)-GWATX(I+1,J,bi,bj) )
     &         + COSWAT *
     &         ( VICE(I,  J,bi,bj)-GWATY(I,  J,bi,bj)
     &         + VICE(I+1,J,bi,bj)-GWATY(I+1,J,bi,bj) )
     &         )
          fu(I,J,bi,bj)=(ONE-AREA(I,J,bi,bj))*fu(I,J,bi,bj)+
     &         AREA(I,J,bi,bj)*fuIce
          fv(I,J,bi,bj)=(ONE-AREA(I,J,bi,bj))*fv(I,J,bi,bj)+
     &         AREA(I,J,bi,bj)*fvIce
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)
#endif /* SEAICE_BICE_STRESS */
#endif /* not SEAICE_CGRID */

      RETURN
      END
