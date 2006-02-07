C $Header: /u/gcmpack/MITgcm/pkg/seaice/ostres.F,v 1.14 2006/02/07 08:08:28 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ostres( DWATN, COR_ICE, myThid )
C     /==========================================================\
C     | SUBROUTINE ostres                                        |
C     | o Calculate ocean surface stress                         |
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
C     myThid - Thread no. that called this routine.
      _RL DWATN      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL COR_ICE        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      _RL  SINWIN, COSWIN, SINWAT, COSWAT
#ifdef SEAICE_TEST_ICE_STRESS_1
      _RL  fuIce, fvIce
#endif

C   25 DEG GIVES SIN EQUAL TO 0.4226
      SINWIN=0.4226 _d 0
      COSWIN=0.9063 _d 0
      SINWAT=0.4226 _d 0
      COSWAT=0.9063 _d 0
c do not introduce turning angle
      SINWIN=ZERO
      COSWIN=ONE
      SINWAT=ZERO
      COSWAT=ONE

#ifdef SEAICE_ORIGINAL_BAD_ICE_STRESS
C--   Following formulation is problematic and is no longer used.
#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDYNAMICS ) THEN
C--   Compute ice-affected wind stress
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           WINDX(I,J,bi,bj)=DWATN(I,J,bi,bj)
     &          *(COSWAT*(GWATX(I,J,bi,bj)-UICE(I,J,1,bi,bj))
     &          -SINWAT*(GWATY(I,J,bi,bj)-VICEC(I,J,bi,bj)))
           WINDY(I,J,bi,bj)=DWATN(I,J,bi,bj)
     &          *(SINWAT*(GWATX(I,J,bi,bj)-UICEC(I,J,bi,bj))
     &          +COSWAT*(GWATY(I,J,bi,bj)-VICE(I,J,1,bi,bj)))
           WINDX(I,J,bi,bj)=WINDX(I,J,bi,bj)-( COR_ICE(I,J,bi,bj)
     &          *GWATY(I,J,bi,bj)-COR_ICE(I,J,bi,bj)*VICEC(I,J,bi,bj))
           WINDY(I,J,bi,bj)=WINDY(I,J,bi,bj)-(-COR_ICE(I,J,bi,bj)
     &          *GWATX(I,J,bi,bj)+COR_ICE(I,J,bi,bj)*UICEC(I,J,bi,bj))
           WINDX(I,J,bi,bj)=WINDX(I,J,bi,bj)-(UICE(I,J,1,bi,bj)
     &          -UICE(I,J,3,bi,bj))*AMASS(I,J,bi,bj)/SEAICE_DT*TWO
           WINDY(I,J,bi,bj)=WINDY(I,J,bi,bj)-(VICE(I,J,1,bi,bj)
     &          -VICE(I,J,3,bi,bj))*AMASS(I,J,bi,bj)/SEAICE_DT*TWO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           WINDX(I,J,bi,bj)=-WINDX(I,J,bi,bj)
           WINDY(I,J,bi,bj)=-WINDY(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
#endif /*  SEAICE_ALLOW_DYNAMICS */
#endif /* SEAICE_ORIGINAL_BAD_ICE_STRESS */

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

#ifdef SEAICE_TEST_ICE_STRESS_1
C--   Compute ice-affected wind stress
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          fuIce=QUART*( DWATN(I,J,bi,bj)+DWATN(I,J+1,bi,bj) )*
     &         ( UICE(I,J,  1,bi,bj)-GWATX(I,J,  bi,bj)
     &         + UICE(I,J+1,1,bi,bj)-GWATX(I,J+1,bi,bj) )
          fvIce=QUART*( DWATN(I,J,bi,bj)+DWATN(I+1,J,bi,bj) )*
     &         ( VICE(I,  J,1,bi,bj)-GWATY(I,  J,bi,bj)
     &         + VICE(I+1,J,1,bi,bj)-GWATY(I+1,J,bi,bj) )
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

      RETURN
      END
