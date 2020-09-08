#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

C--  File find_alpha.F: Calculate expansion Coeff
C--   Contents
C--   o FIND_ALPHA
C--   o FIND_BETA

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_ALPHA
C     !INTERFACE:
      SUBROUTINE FIND_ALPHA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     O     alphaLoc,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_ALPHA
C     |   Calculates [drho(S,T,z) / dT] of a horizontal slice
C     *==========================================================*
C     | k        - is the Theta/Salt level
C     | kRef     - determines pressure reference level
C     |            (not used in 'LINEAR' mode)
C     | alphaLoc - drho / dT (kg/m^3/C)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k      :: Level of Theta/Salt slice
C     kRef   :: Pressure reference level
c     myThid :: thread number for this instance of the routine
      INTEGER myThid
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      INTEGER kRef
      _RL alphaLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL refTemp, refSalt, tP, sP, dp0
      _RL t1, t2, t3, s1, s3o2, p1, p2, sp5, p1t1
      _RL ct, sa, sqrtsa, p
      _RL drhoP0dtheta, drhoP0dthetaFresh, drhoP0dthetaSalt
      _RL dKdtheta, dKdthetaFresh, dKdthetaSalt, dKdthetaPres
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoP0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL bulkMod(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dnum_dtheta, dden_dtheta
      _RL rhoDen (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoLoc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

#ifdef CHECK_SALINITY_FOR_NEGATIVE_VALUES
c     CALL LOOK_FOR_NEG_SALINITY(
c    I          iMin, iMax, jMin, jMax,
c    U          sFld,
c    I          k, bi, bj, myThid )
#endif

      IF (equationOfState.EQ.'LINEAR') THEN

         DO j=jMin,jMax
            DO i=iMin,iMax
               alphaLoc(i,j) = -rhonil * tAlpha
            ENDDO
         ENDDO

      ELSEIF (equationOfState.EQ.'POLY3') THEN

         refTemp=eosRefT(kRef)
         refSalt=eosRefS(kRef)

         DO j=jMin,jMax
            DO i=iMin,iMax
               tP=theta(i,j,k,bi,bj)-refTemp
               sP=salt(i,j,k,bi,bj)-refSalt
#ifdef USE_FACTORIZED_POLY
               alphaLoc(i,j) =
     &     ( eosC(6,kRef)
     &         *tP*3.
     &        +(eosC(7,kRef)*sP + eosC(3,kRef))*2.
     &       )*tP
     &      +(eosC(8,kRef)*sP + eosC(4,kRef) )*sP + eosC(1,kRef)
     &
#else
               alphaLoc(i,j) =
     &              eosC(1,kRef)                +
     &              eosC(3,kRef)*tP*2.          +
     &              eosC(4,kRef)         *sP    +
     &              eosC(6,kRef)*tP*tP*3.       +
     &              eosC(7,kRef)*tP*2.   *sP    +
     &              eosC(8,kRef)         *sP*sP
#endif
            ENDDO
         ENDDO

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &        .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I        bi, bj, iMin, iMax, jMin, jMax,  kRef, dp0,
     O        locPres,
     I        myThid )

         CALL FIND_RHOP0(
     I        iMin, iMax, jMin, jMax,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoP0,
     I        myThid )

         CALL FIND_BULKMOD(
     I        iMin, iMax, jMin, jMax, locPres,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        bulkMod,
     I        myThid )

         DO j=jMin,jMax
            DO i=iMin,iMax

C     abbreviations
               t1 = theta(i,j,k,bi,bj)
               t2 = t1*t1
               t3 = t2*t1

#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
               s3o2 = 0. _d 0
#endif
               s1  = salt(i,j,k,bi,bj)
              IF ( s1 .GT. 0. _d 0 ) THEN
               s3o2 = SQRT(s1*s1*s1)
              ELSE
               s1   = 0. _d 0
               s3o2 = 0. _d 0
              ENDIF

               p1  = locPres(i,j)*SItoBar
               p2 = p1*p1

C     d(rho)/d(theta)
C     of fresh water at p = 0
               drhoP0dthetaFresh =
     &                eosJMDCFw(2)
     &           + 2.*eosJMDCFw(3)*t1
     &           + 3.*eosJMDCFw(4)*t2
     &           + 4.*eosJMDCFw(5)*t3
     &           + 5.*eosJMDCFw(6)*t3*t1
C     of salt water at p = 0
               drhoP0dthetaSalt =
     &        s1*(
     &                eosJMDCSw(2)
     &           + 2.*eosJMDCSw(3)*t1
     &           + 3.*eosJMDCSw(4)*t2
     &           + 4.*eosJMDCSw(5)*t3
     &           )
     &       + s3o2*(
     &           +    eosJMDCSw(7)
     &           + 2.*eosJMDCSw(8)*t1
     &           )
C     d(bulk modulus)/d(theta)
C     of fresh water at p = 0
               dKdthetaFresh =
     &                eosJMDCKFw(2)
     &           + 2.*eosJMDCKFw(3)*t1
     &           + 3.*eosJMDCKFw(4)*t2
     &           + 4.*eosJMDCKFw(5)*t3
C     of sea water at p = 0
               dKdthetaSalt =
     &        s1*(    eosJMDCKSw(2)
     &           + 2.*eosJMDCKSw(3)*t1
     &           + 3.*eosJMDCKSw(4)*t2
     &           )
     &    + s3o2*(    eosJMDCKSw(6)
     &           + 2.*eosJMDCKSw(7)*t1
     &           )
C     of sea water at p
               dKdthetaPres =
     &        p1*(    eosJMDCKP(2)
     &           + 2.*eosJMDCKP(3)*t1
     &           + 3.*eosJMDCKP(4)*t2
     &           )
     &   + p1*s1*(    eosJMDCKP(6)
     &           + 2.*eosJMDCKP(7)*t1
     &           )
     &      + p2*(    eosJMDCKP(10)
     &           + 2.*eosJMDCKP(11)*t1
     &           )
     &   + p2*s1*(    eosJMDCKP(13)
     &           + 2.*eosJMDCKP(14)*t1
     &           )

               drhoP0dtheta  = drhoP0dthetaFresh
     &                       + drhoP0dthetaSalt
               dKdtheta      = dKdthetaFresh
     &                       + dKdthetaSalt
     &                       + dKdthetaPres
               alphaLoc(i,j) =
     &              ( bulkmod(i,j)**2*drhoP0dtheta
     &              - bulkmod(i,j)*p1*drhoP0dtheta
     &              - rhoP0(i,j)*p1*dKdtheta )
     &              /( bulkmod(i,j) - p1 )**2

            ENDDO
         ENDDO
      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I        bi, bj, iMin, iMax, jMin, jMax,  kRef, dp0,
     O        locPres,
     I        myThid )

         CALL FIND_RHONUM(
     I        iMin, iMax, jMin, jMax, locPres,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoLoc,
     I        myThid )

         CALL FIND_RHODEN(
     I        iMin, iMax, jMin, jMax, locPres,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoDen,
     I        myThid )

         DO j=jMin,jMax
            DO i=iMin,iMax
               t1  = theta(i,j,k,bi,bj)
               t2  = t1*t1
               s1  = salt(i,j,k,bi,bj)
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
               sp5 = 0. _d 0
#endif
              IF ( s1 .GT. 0. _d 0 ) THEN
               sp5 = SQRT(s1)
              ELSE
               s1  = 0. _d 0
               sp5 = 0. _d 0
              ENDIF

               p1   = locPres(i,j)*SItodBar
               p1t1 = p1*t1

               dnum_dtheta = eosMDJWFnum(1)
     &              + t1*(2.*eosMDJWFnum(2) + 3.*eosMDJWFnum(3)*t1)
     &              + eosMDJWFnum(5)*s1
     &              + p1t1*(2.*eosMDJWFnum(8) + 2.*eosMDJWFnum(11)*p1)

               dden_dtheta = eosMDJWFden(1)
     &              + t1*(2.*eosMDJWFden(2)
     &              +     t1*(3.*eosMDJWFden(3)
     &              +         4.*eosMDJWFden(4)*t1 ) )
     &              + s1*(eosMDJWFden(6)
     &              +     t1*(3.*eosMDJWFden(7)*t1
     &              +         2.*eosMDJWFden(9)*sp5 ) )
     &              + p1*p1*(3.*eosMDJWFden(11)*t2 + eosMDJWFden(12)*p1)

               alphaLoc(i,j)    = rhoDen(i,j)*(dnum_dtheta
     &              - (rhoLoc(i,j)*rhoDen(i,j))*dden_dtheta)

         ENDDO
      ENDDO

      ELSEIF ( equationOfState.EQ.'TEOS10' ) THEN
       dp0 = surf_pRef - eosRefP0

       CALL PRESSURE_FOR_EOS(
     I      bi, bj, iMin, iMax, jMin, jMax,  kRef, dp0,
     O      locPres,
     I      myThid )

       CALL FIND_RHOTEOS(
     I      iMin, iMax, jMin, jMax, locPres,
     I      theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O      rhoLoc, rhoDen,
     I      myThid )

       DO j=jMin,jMax
        DO i=iMin,iMax
         ct      = theta(i,j,k,bi,bj)
         sa      = salt(i,j,k,bi,bj)
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
         sqrtsa  = 0. _d 0
#endif
         IF ( sa .GT. 0. _d 0 ) THEN
          sqrtsa = SQRT(sa)
         ELSE
          sa     = 0. _d 0
          sqrtsa = 0. _d 0
         ENDIF
         p       = locPres(i,j)*SItodBar

         dnum_dtheta = teos(02)
     &   + ct*(2.*teos(03) + 3.*teos(04)*ct)
     &   + sa*(teos(06) + 2.*teos(07)*ct
     &   + sqrtsa*(teos(09) + ct*(2.*teos(10) + 3.*teos(11)*ct)))
     &   + p*(     teos(13) + 2.*teos(14)*ct  + sa*2.*teos(16)
     &        + p*(teos(18) + 2.*teos(19)*ct))

         dden_dtheta = teos(22)
     &   + ct*(2.*teos(23) + ct*(3.*teos(24) + 4.*teos(25)*ct))
     &   + sa*(teos(27)
     &   + ct*(2.*teos(28) + ct*(3.*teos(29) + 4.*teos(30)*ct))
     &   + sqrtsa*(teos(32)
     &   + ct*(2.*teos(33) + ct*(3.*teos(34) + 4.*teos(35)*ct))))
     &   + p*(teos(38) + ct*(2.*teos(39) + 3.*teos(40)*ct)
     &   + teos(42)
     &   + p*(teos(44) + 2.*teos(45)*ct + teos(46)*sa
     &   + p*teos(48) ))

         alphaLoc(i,j)    = rhoDen(i,j)*(dnum_dtheta
     &        - (rhoLoc(i,j)*rhoDen(i,j))*dden_dtheta)

        ENDDO
       ENDDO

      ELSE
         WRITE(*,*) 'FIND_ALPHA: equationOfState = ',equationOfState
         STOP 'FIND_ALPHA: "equationOfState" has illegal value'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_BETA
C     !INTERFACE:
      SUBROUTINE FIND_BETA (
     I     bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     O     betaLoc,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_BETA
C     |   Calculates [drho(S,T,z) / dS] of a horizontal slice
C     |==========================================================*
C     | k       - is the Theta/Salt level
C     | kRef    - determines pressure reference level
C     |           (not used in 'LINEAR' mode)
C     | betaLoc - drho / dS (kg/m^3/(g/kg))
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k      :: Level of Theta/Salt slice
C     kRef   :: Pressure reference level
c     myThid :: thread number for this instance of the routine
      INTEGER myThid
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      INTEGER kRef
      _RL betaLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL refTemp, refSalt, tP, sP, dp0
      _RL t1, t2, t3, s1, s3o2, p1, sp5, p1t1
      _RL ct, sa, sqrtsa, p
      _RL drhoP0dS
      _RL dKdS, dKdSSalt, dKdSPres
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoP0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL bulkMod(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dnum_dsalt, dden_dsalt
      _RL rhoDen (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoLoc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
CEOP

#ifdef CHECK_SALINITY_FOR_NEGATIVE_VALUES
c     CALL LOOK_FOR_NEG_SALINITY(
c    I          iMin, iMax, jMin, jMax,
c    U          sFld,
c    I          k, bi, bj, myThid )
#endif

      IF (equationOfState.EQ.'LINEAR') THEN

         DO j=jMin,jMax
            DO i=iMin,iMax
               betaLoc(i,j) = rhonil * sBeta
            ENDDO
         ENDDO

      ELSEIF (equationOfState.EQ.'POLY3') THEN

         refTemp=eosRefT(kRef)
         refSalt=eosRefS(kRef)

         DO j=jMin,jMax
            DO i=iMin,iMax
               tP=theta(i,j,k,bi,bj)-refTemp
               sP=salt(i,j,k,bi,bj)-refSalt
#ifdef USE_FACTORIZED_POLY
               betaLoc(i,j) =
     &    ( eosC(9,kRef)*sP*3. + eosC(5,kRef)*2. )*sP + eosC(2,kRef)
     &   + ( eosC(7,kRef)*tP
     &      +eosC(8,kRef)*sP*2. + eosC(4,kRef)
     &     )*tP
#else
               betaLoc(i,j) =
     &              eosC(2,kRef)                +
     &              eosC(4,kRef)*tP             +
     &              eosC(5,kRef)         *sP*2. +
     &              eosC(7,kRef)*tP*tP          +
     &              eosC(8,kRef)*tP      *sP*2. +
     &              eosC(9,kRef)         *sP*sP*3.
#endif
            ENDDO
         ENDDO

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &        .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I        bi, bj, iMin, iMax, jMin, jMax,  kRef, dp0,
     O        locPres,
     I        myThid )

         CALL FIND_RHOP0(
     I        iMin, iMax, jMin, jMax,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoP0,
     I        myThid )

         CALL FIND_BULKMOD(
     I        iMin, iMax, jMin, jMax, locPres,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        bulkMod,
     I        myThid )

         DO j=jMin,jMax
            DO i=iMin,iMax

C     abbreviations
               t1 = theta(i,j,k,bi,bj)
               t2 = t1*t1
               t3 = t2*t1

               s1  = salt(i,j,k,bi,bj)
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
               s3o2 = 0. _d 0
#endif
              IF ( s1 .GT. 0. _d 0 ) THEN
               s3o2 = 1.5*SQRT(s1)
              ELSE
               s1   = 0. _d 0
               s3o2 = 0. _d 0
              ENDIF

               p1  = locPres(i,j)*SItoBar

C     d(rho)/d(S)
C     of fresh water at p = 0
               drhoP0dS = 0. _d 0
C     of salt water at p = 0
               drhoP0dS = drhoP0dS
     &              + eosJMDCSw(1)
     &              + eosJMDCSw(2)*t1
     &              + eosJMDCSw(3)*t2
     &              + eosJMDCSw(4)*t3
     &              + eosJMDCSw(5)*t3*t1
     &       + s3o2*(
     &                eosJMDCSw(6)
     &              + eosJMDCSw(7)*t1
     &              + eosJMDCSw(8)*t2
     &              )
     &              + 2*eosJMDCSw(9)*s1
C     d(bulk modulus)/d(S)
C     of fresh water at p = 0
               dKdS = 0. _d 0
C     of sea water at p = 0
               dKdSSalt =
     &                eosJMDCKSw(1)
     &              + eosJMDCKSw(2)*t1
     &              + eosJMDCKSw(3)*t2
     &              + eosJMDCKSw(4)*t3
     &       + s3o2*( eosJMDCKSw(5)
     &              + eosJMDCKSw(6)*t1
     &              + eosJMDCKSw(7)*t2
     &              )

C     of sea water at p
               dKdSPres =
     &           p1*( eosJMDCKP(5)
     &              + eosJMDCKP(6)*t1
     &              + eosJMDCKP(7)*t2
     &              )
     &        + s3o2*p1*eosJMDCKP(8)
     &      + p1*p1*( eosJMDCKP(12)
     &              + eosJMDCKP(13)*t1
     &              + eosJMDCKP(14)*t2
     &              )

               dKdS = dKdSSalt + dKdSPres

               betaLoc(i,j) =
     &              ( bulkmod(i,j)**2*drhoP0dS
     &              - bulkmod(i,j)*p1*drhoP0dS
     &              - rhoP0(i,j)*p1*dKdS )
     &              /( bulkmod(i,j) - p1 )**2

            ENDDO
         ENDDO
      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I        bi, bj, iMin, iMax, jMin, jMax,  kRef, dp0,
     O        locPres,
     I        myThid )

         CALL FIND_RHONUM(
     I        iMin, iMax, jMin, jMax, locPres,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoLoc,
     I        myThid )

         CALL FIND_RHODEN(
     I        iMin, iMax, jMin, jMax, locPres,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoDen,
     I        myThid )

         DO j=jMin,jMax
            DO i=iMin,iMax
               t1  = theta(i,j,k,bi,bj)
               t2  = t1*t1
               s1  = salt(i,j,k,bi,bj)
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
               sp5 = 0. _d 0
#endif
              IF ( s1 .GT. 0. _d 0 ) THEN
               sp5 = SQRT(s1)
              ELSE
               s1  = 0. _d 0
               sp5 = 0. _d 0
              ENDIF

               p1   = locPres(i,j)*SItodBar
               p1t1 = p1*t1

               dnum_dsalt = eosMDJWFnum(4)
     &              + eosMDJWFnum(5)*t1
     &              + 2.*eosMDJWFnum(6)*s1 + eosMDJWFnum(9)*p1
               dden_dsalt = eosMDJWFden(5)
     &              + t1*( eosMDJWFden(6) + eosMDJWFden(7)*t2 )
     &              + 1.5*sp5*(eosMDJWFden(8) + eosMDJWFden(9)*t2)

               betaLoc(i,j) = rhoDen(i,j)*( dnum_dsalt
     &              - (rhoLoc(i,j)*rhoDen(i,j))*dden_dsalt )

            ENDDO
         ENDDO

      ELSEIF ( equationOfState.EQ.'TEOS10' ) THEN
       dp0 = surf_pRef - eosRefP0

       CALL PRESSURE_FOR_EOS(
     I      bi, bj, iMin, iMax, jMin, jMax,  kRef, dp0,
     O      locPres,
     I      myThid )

       CALL FIND_RHOTEOS(
     I      iMin, iMax, jMin, jMax, locPres,
     I      theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O      rhoLoc, rhoDen,
     I      myThid )

       DO j=jMin,jMax
        DO i=iMin,iMax
         ct      = theta(i,j,k,bi,bj)
         sa      = salt(i,j,k,bi,bj)
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
         sqrtsa  = 0. _d 0
#endif
         IF ( sa .GT. 0. _d 0 ) THEN
          sqrtsa = SQRT(sa)
         ELSE
          sa     = 0. _d 0
          sqrtsa = 0. _d 0
         ENDIF
         p       = locPres(i,j)*SItodBar

         dnum_dsalt =  teos(05) + ct*(teos(06) + teos(07)*ct)
     &   + 1.5*sqrtsa*(teos(08)
     &           + ct*(teos(09) + ct*(teos(10) + teos(11)*ct)))
     &   + p*(teos(15) + teos(16)*ct + p*teos(20))

         dden_dsalt = teos(26)
     &   + ct*(teos(27) + ct*(teos(28) + ct*(teos(29) + teos(30)*ct)))
     &   + 2.*teos(36)*sa
     &   + 1.5*sqrtsa*(teos(31) + ct*(teos(32) + ct*(teos(33)
     &                          + ct*(teos(34) + teos(35)*ct))))
     &   + p*(teos(41) + teos(42)*ct + p*teos(46))

         betaLoc(i,j) = rhoDen(i,j)*( dnum_dsalt
     &        - (rhoLoc(i,j)*rhoDen(i,j))*dden_dsalt )

        ENDDO
       ENDDO

      ELSE
         WRITE(*,*) 'FIND_BETA: equationOfState = ',equationOfState
         STOP 'FIND_BETA: "equationOfState" has illegal value'
      ENDIF

      RETURN
      END
