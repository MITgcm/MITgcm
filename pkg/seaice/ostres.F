C $Header: /u/gcmpack/MITgcm/pkg/seaice/ostres.F,v 1.22 2010/10/29 15:26:29 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE ostres( COR_ICE, myThid )
C     *==========================================================*
C     | SUBROUTINE ostres                                        |
C     | o Calculate ocean surface stress                         |
C     *==========================================================*
C     *==========================================================*
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

#ifdef SEAICE_ORIGINAL_BAD_ICE_STRESS
      These lines are put here on purpose to cause the compilation
      to fail because the following code is retired and can no longer
      be used. It was probably never used anyway.
CMLC--   Following formulation is problematic and is no longer used.
CML#ifdef SEAICE_ALLOW_DYNAMICS
CML      IF ( SEAICEuseDYNAMICS ) THEN
CMLC--   Compute ice-affected wind stress
CML       DO bj=myByLo(myThid),myByHi(myThid)
CML        DO bi=myBxLo(myThid),myBxHi(myThid)
CML         DO j=1,sNy
CML          DO i=1,sNx
CML           WINDX(I,J,bi,bj)=DWATN(I,J,bi,bj)
CML     &          *(COSWAT*(GWATX(I,J,bi,bj)-UICE(I,J,bi,bj))
CML     &          -SIGN(SINWAT,COR_ICE(I,J,bi,bj))
CML     &          *(GWATY(I,J,bi,bj)-VICEC(I,J,bi,bj)))
CML           WINDY(I,J,bi,bj)=DWATN(I,J,bi,bj)
CML     &          *(SIGN(SINWAT,COR_ICE(I,J,bi,bj))
CML     &          *(GWATX(I,J,bi,bj)-UICEC(I,J,bi,bj))
CML     &          +COSWAT*(GWATY(I,J,bi,bj)-VICE(I,J,bi,bj)))
CML           WINDX(I,J,bi,bj)=WINDX(I,J,bi,bj)-( COR_ICE(I,J,bi,bj)
CML     &          *GWATY(I,J,bi,bj)-COR_ICE(I,J,bi,bj)*VICEC(I,J,bi,bj))
CML           WINDY(I,J,bi,bj)=WINDY(I,J,bi,bj)-(-COR_ICE(I,J,bi,bj)
CML     &          *GWATX(I,J,bi,bj)+COR_ICE(I,J,bi,bj)*UICEC(I,J,bi,bj))
CML           WINDX(I,J,bi,bj)=WINDX(I,J,bi,bj)-(UICE(I,J,bi,bj)
CML     &          -UICE(I,J,3,bi,bj))*AMASS(I,J,bi,bj)/SEAICE_DT*TWO
CML           WINDY(I,J,bi,bj)=WINDY(I,J,bi,bj)-(VICE(I,J,bi,bj)
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
     &         ( VICE(I,  J,bi,bj)-GWATY(I,  J,bi,bj)
     &         + VICE(I+1,J,bi,bj)-GWATY(I+1,J,bi,bj) )
     &         )
          fvIce=QUART*( DWATN(I,J,bi,bj)+DWATN(I+1,J,bi,bj) )*(
     &         SIGN(SINWAT,COR_ICE(I,J,bi,bj)) *
     &         ( UICE(I,J,  bi,bj)-GWATX(I,J,  bi,bj)
     &         + UICE(I,J+1,bi,bj)-GWATX(I,J+1,bi,bj) )
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
