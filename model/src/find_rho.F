#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef TARGET_NEC_SX
C     make sure that the factorized EOS is used on NEC vector computers
# define USE_FACTORIZED_EOS
#endif
C     this was the default, so we keep it that way
#define USE_FACTORIZED_POLY

C--  File find_rho.F: Routines to compute density
C--   Contents
C--   o FIND_RHO_2D
C--   o FIND_RHOP0
C--   o FIND_BULKMOD
C--   o FIND_RHONUM
C--   o FIND_RHODEN
C--   o FIND_RHOTEOS
C--   o FIND_RHO_SCALAR: in-situ density for individual points
C--   o LOOK_FOR_NEG_SALINITY

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHO_2D
C     !INTERFACE:
      SUBROUTINE FIND_RHO_2D(
     I                iMin, iMax, jMin, jMax, kRef,
     I                tFld, sFld,
     O                rhoLoc,
     I                k, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHO_2D
C     |   Calculates [rho(S,T,z)-rhoConst] of a 2-D slice
C     *==========================================================*
C     | kRef - determines pressure reference level
C     |        (not used in 'LINEAR' mode)
C     | Note:  k is not used ; keep it for debugging.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k    :: Level of Theta/Salt slice
C     kRef :: Pressure reference level
      INTEGER iMin,iMax,jMin,jMax
      INTEGER kRef
      _RL tFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL refTemp, refSalt, sigRef, tP, sP, deltaSig, dRho
      _RL dp0, oneMinusKap, facPres, theta_v
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoP0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL bulkMod(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoNum (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoDen (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_AUTODIFF
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        rhoLoc(i,j)  = 0. _d 0
        rhoP0(i,j)   = 0. _d 0
        bulkMod(i,j) = 0. _d 0
       ENDDO
      ENDDO
#endif

#ifdef CHECK_SALINITY_FOR_NEGATIVE_VALUES
      CALL LOOK_FOR_NEG_SALINITY(
     I          iMin, iMax, jMin, jMax,
     U          sFld,
     I          k, bi, bj, myThid )
#endif

      IF (equationOfState.EQ.'LINEAR') THEN

C ***NOTE***
C In the linear EOS, to make the static stability calculation meaningful
C we use reference temp & salt from level kRef ;
C **********
       refTemp=tRef(kRef)
       refSalt=sRef(kRef)

       dRho = rhoNil-rhoConst

       DO j=jMin,jMax
        DO i=iMin,iMax
         rhoLoc(i,j)=rhoNil*(
     &     sBeta*(sFld(i,j)-refSalt)
     &   -tAlpha*(tFld(i,j)-refTemp) )
     &        + dRho
        ENDDO
       ENDDO

      ELSEIF (equationOfState.EQ.'POLY3') THEN

       refTemp=eosRefT(kRef)
       refSalt=eosRefS(kRef)
       sigRef=eosSig0(kRef) + (1000.-rhoConst)

       DO j=jMin,jMax
        DO i=iMin,iMax
         tP=tFld(i,j)-refTemp
         sP=sFld(i,j)-refSalt
#ifdef USE_FACTORIZED_POLY
         deltaSig=
     &    (( eosC(9,kRef)*sP + eosC(5,kRef) )*sP + eosC(2,kRef) )*sP
     &   + ( ( eosC(6,kRef)
     &         *tP
     &        +eosC(7,kRef)*sP + eosC(3,kRef)
     &       )*tP
     &      +(eosC(8,kRef)*sP + eosC(4,kRef) )*sP + eosC(1,kRef)
     &     )*tP
#else
         deltaSig=
     &     eosC(1,kRef)*tP
     &    +eosC(2,kRef)         *sP
     &    +eosC(3,kRef)*tP*tP
     &    +eosC(4,kRef)*tP      *sP
     &    +eosC(5,kRef)         *sP*sP
     &    +eosC(6,kRef)*tP*tP*tP
     &    +eosC(7,kRef)*tP*tP   *sP
     &    +eosC(8,kRef)*tP      *sP*sP
     &    +eosC(9,kRef)         *sP*sP*sP
#endif
         rhoLoc(i,j)=sigRef+deltaSig
        ENDDO
       ENDDO

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &      .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I             bi, bj, iMin, iMax, jMin, jMax, kRef, dp0,
     O             locPres,
     I             myThid )

         CALL FIND_RHOP0(
     I             iMin, iMax, jMin, jMax,
     I             tFld, sFld,
     O             rhoP0,
     I             myThid )

         CALL FIND_BULKMOD(
     I             iMin, iMax, jMin, jMax,
     I             locPres, tFld, sFld,
     O             bulkMod,
     I             myThid )

c#ifdef ALLOW_AUTODIFF_TAMC
cph can not DO storing here since find_rho is called multiple times;
cph additional recomp. should be acceptable
c#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax

C     density of sea water at pressure p
            rhoLoc(i,j) = rhoP0(i,j)
     &              /(1. _d 0 -
     &              locPres(i,j)*SItoBar/bulkMod(i,j) )
     &              - rhoConst

          ENDDO
         ENDDO

      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I             bi, bj, iMin, iMax, jMin, jMax, kRef, dp0,
     O             locPres,
     I             myThid )

         CALL FIND_RHONUM(
     I             iMin, iMax, jMin, jMax,
     I             locPres, tFld, sFld,
     O             rhoNum,
     I             myThid )

         CALL FIND_RHODEN(
     I             iMin, iMax, jMin, jMax,
     I             locPres, tFld, sFld,
     O             rhoDen,
     I             myThid )

c#ifdef ALLOW_AUTODIFF_TAMC
cph can not DO storing here since find_rho is called multiple times;
cph additional recomp. should be acceptable
c#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
            rhoLoc(i,j) = rhoNum(i,j)*rhoDen(i,j) - rhoConst
          ENDDO
         ENDDO

      ELSEIF ( equationOfState.EQ.'TEOS10' ) THEN
         dp0 = surf_pRef - eosRefP0

         CALL PRESSURE_FOR_EOS(
     I             bi, bj, iMin, iMax, jMin, jMax, kRef, dp0,
     O             locPres,
     I             myThid )

         CALL FIND_RHOTEOS(
     I             iMin, iMax, jMin, jMax,
     I             locPres, tFld, sFld,
     O             rhoNum, rhoDen,
     I             myThid )

c#ifdef ALLOW_AUTODIFF_TAMC
cph can not DO storing here since find_rho is called multiple times;
cph additional recomp. should be acceptable
c#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
            rhoLoc(i,j) = rhoNum(i,j)*rhoDen(i,j) - rhoConst
          ENDDO
         ENDDO

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN

         CALL PRESSURE_FOR_EOS(
     I             bi, bj, iMin, iMax, jMin, jMax, kRef, zeroRL,
     O             locPres,
     I             myThid )

         oneMinusKap = oneRL - atm_kappa
C        rho = P/(R*T_v) = Po/(R*theta_v)*(P/Po)^(1-kappa)
         DO j=jMin,jMax
          DO i=iMin,iMax
           IF ( locPres(i,j).GT.zeroRL .AND. tFld(i,j).GT.zeroRL ) THEN
            facPres = ( locPres(i,j)/atm_Po )**oneMinusKap
            theta_v = tFld(i,j)*( sFld(i,j)*atm_Rq + oneRL )
            rhoLoc(i,j) = atm_Po*facPres
     &                 /( atm_Rd*theta_v ) - rhoConst
           ELSE
            rhoLoc(i,j) = zeroRL
           ENDIF
          ENDDO
         ENDDO

      ELSE
       WRITE(msgBuf,'(3a)')
     &      ' FIND_RHO_2D: equationOfState = "',equationOfState,'"'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R FIND_RHO_2D'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHOP0
C     !INTERFACE:
      SUBROUTINE FIND_RHOP0(
     I                iMin, iMax, jMin, jMax,
     I                tFld, sFld,
     O                rhoP0,
     I                myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHOP0
C     |   Calculates rho(S,T,0) of a slice
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER iMin,iMax,jMin,jMax
      _RL tFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoP0(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL rfresh, rsalt
      _RL t, t2, t3, t4, s, s3o2
#ifdef USE_FACTORIZED_EOS
      _RL eosF1, eosF2, eosF3, eosF4, eosF5, eosF6
      _RL eosS1, eosS2, eosS3, eosS4, eosS5, eosS6
      _RL eosS7, eosS8, eosS9
#endif /* USE_FACTORIZED_EOS */
CEOP
#ifdef USE_FACTORIZED_EOS
      eosF1 = eosJMDCFw(1)
      eosF2 = eosJMDCFw(2)
      eosF3 = eosJMDCFw(3)
      eosF4 = eosJMDCFw(4)
      eosF5 = eosJMDCFw(5)
      eosF6 = eosJMDCFw(6)
      eosS1 = eosJMDCSw(1)
      eosS2 = eosJMDCSw(2)
      eosS3 = eosJMDCSw(3)
      eosS4 = eosJMDCSw(4)
      eosS5 = eosJMDCSw(5)
      eosS6 = eosJMDCSw(6)
      eosS7 = eosJMDCSw(7)
      eosS8 = eosJMDCSw(8)
      eosS9 = eosJMDCSw(9)
#endif /* USE_FACTORIZED_EOS */

      DO j=jMin,jMax
         DO i=iMin,iMax
C     abbreviations
            t  = tFld(i,j)
            t2 = t*t
            t3 = t2*t
            t4 = t3*t

#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
            s3o2 = 0. _d 0
#endif
            s  = sFld(i,j)
           IF ( s .GT. 0. _d 0 ) THEN
            s3o2 = s*SQRT(s)
           ELSE
            s    = 0. _d 0
            s3o2 = 0. _d 0
           ENDIF

C     density of freshwater at the surface
#ifdef USE_FACTORIZED_EOS
            rfresh =
     &                    eosF1 +
     &               t* ( eosF2 +
     &               t* ( eosF3 +
     &               t* ( eosF4 +
     &               t* ( eosF5 +
     &               t*   eosF6  ))))
#else
            rfresh =
     &             eosJMDCFw(1)
     &           + eosJMDCFw(2)*t
     &           + eosJMDCFw(3)*t2
     &           + eosJMDCFw(4)*t3
     &           + eosJMDCFw(5)*t4
     &           + eosJMDCFw(6)*t4*t
#endif /* USE_FACTORIZED_EOS */
C     density of sea water at the surface
#ifdef USE_FACTORIZED_EOS
            rsalt =  s * ( eosS1 +
     &               t * ( eosS2 +
     &               t * ( eosS3 +
     &               t * ( eosS4 +
     &               t *   eosS5  ))))
     &          + s3o2 * ( eosS6 +
     &               t * ( eosS7 +
     &               t *   eosS8  ))
     &          + s*s  *   eosS9
#else
            rsalt =
     &         s*(
     &             eosJMDCSw(1)
     &           + eosJMDCSw(2)*t
     &           + eosJMDCSw(3)*t2
     &           + eosJMDCSw(4)*t3
     &           + eosJMDCSw(5)*t4
     &           )
     &       + s3o2*(
     &             eosJMDCSw(6)
     &           + eosJMDCSw(7)*t
     &           + eosJMDCSw(8)*t2
     &           )
     &           + eosJMDCSw(9)*s*s
#endif /* USE_FACTORIZED_EOS */

            rhoP0(i,j) = rfresh + rsalt

         ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_BULKMOD
C     !INTERFACE:
      SUBROUTINE FIND_BULKMOD(
     I                iMin, iMax, jMin, jMax,
     I                locPres, tFld, sFld,
     O                bulkMod,
     I                myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_BULKMOD
C     |   Calculates the secant bulk modulus K(S,T,p) of a slice
C     *==========================================================*
C     | k    - is the level of Theta/Salt slice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER iMin,iMax,jMin,jMax
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL bulkMod(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL bMfresh, bMsalt, bMpres
      _RL t, t2, t3, t4, s, s3o2, p, p2
#ifdef USE_FACTORIZED_EOS
      _RL eosF1, eosF2, eosF3, eosF4, eosF5
      _RL eosS1, eosS2, eosS3, eosS4, eosS5, eosS6, eosS7
      _RL eosP1, eosP2, eosP3, eosP4, eosP5, eosP6, eosP7
      _RL eosP8, eosP9, eosP10, eosP11, eosP12, eosP13, eosP14
#endif /* USE_FACTORIZED_EOS */
CEOP
#ifdef USE_FACTORIZED_EOS
      eosF1 = eosJMDCKFw(1)
      eosF2 = eosJMDCKFw(2)
      eosF3 = eosJMDCKFw(3)
      eosF4 = eosJMDCKFw(4)
      eosF5 = eosJMDCKFw(5)

      eosS1 = eosJMDCKSw(1)
      eosS2 = eosJMDCKSw(2)
      eosS3 = eosJMDCKSw(3)
      eosS4 = eosJMDCKSw(4)
      eosS5 = eosJMDCKSw(5)
      eosS6 = eosJMDCKSw(6)
      eosS7 = eosJMDCKSw(7)

      eosP1  =eosJMDCKP(1)
      eosP2  =eosJMDCKP(2)
      eosP3  =eosJMDCKP(3)
      eosP4  =eosJMDCKP(4)
      eosP5  =eosJMDCKP(5)
      eosP6  =eosJMDCKP(6)
      eosP7  =eosJMDCKP(7)
      eosP8  =eosJMDCKP(8)
      eosP9  =eosJMDCKP(9)
      eosP10 =eosJMDCKP(10)
      eosP11 =eosJMDCKP(11)
      eosP12 =eosJMDCKP(12)
      eosP13 =eosJMDCKP(13)
      eosP14 =eosJMDCKP(14)
#endif /* USE_FACTORIZED_EOS */

      DO j=jMin,jMax
         DO i=iMin,iMax
C     abbreviations
            t  = tFld(i,j)
            t2 = t*t
            t3 = t2*t
            t4 = t3*t

#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
            s3o2 = 0. _d 0
#endif
            s  = sFld(i,j)
          IF ( s .GT. 0. _d 0 ) THEN
            s3o2 = s*SQRT(s)
          ELSE
            s    = 0. _d 0
            s3o2 = 0. _d 0
          ENDIF
C
            p = locPres(i,j)*SItoBar
            p2 = p*p
C     secant bulk modulus of fresh water at the surface
#ifdef USE_FACTORIZED_EOS
            bMfresh =       eosF1 +
     &                t * ( eosF2 +
     &                t * ( eosF3 +
     &                t * ( eosF4 +
     &                t *   eosF5  )))
#else
            bMfresh =
     &             eosJMDCKFw(1)
     &           + eosJMDCKFw(2)*t
     &           + eosJMDCKFw(3)*t2
     &           + eosJMDCKFw(4)*t3
     &           + eosJMDCKFw(5)*t4
#endif /* USE_FACTORIZED_EOS */
C     secant bulk modulus of sea water at the surface
#ifdef USE_FACTORIZED_EOS
            bMsalt =  s * ( eosS1 +
     &                t * ( eosS2 +
     &                t * ( eosS3 +
     &                t *   eosS4  )))
     &            + s3o2* ( eosS5 +
     &                t * ( eosS6 +
     &                t *   eosS7  ))
C     secant bulk modulus of sea water at pressure p
            bMpres =  p * ( eosP1 +
     &                t * ( eosP2 +
     &                t * ( eosP3 +
     &                t *   eosP4  )))
     &             + p*s* ( eosP5 +
     &                t * ( eosP6 +
     &                t *   eosP7  ))
     &             + p*s3o2*eosP8
     &             + p2 * ( eosP9 +
     &                t * ( eosP10 +
     &                t *   eosP11  ))
     &            + p2*s* ( eosP12 +
     &                t * ( eosP13 +
     &                t *   eosP14 ))
#else
C     secant bulk modulus of sea water at the surface
            bMsalt =
     &         s*( eosJMDCKSw(1)
     &           + eosJMDCKSw(2)*t
     &           + eosJMDCKSw(3)*t2
     &           + eosJMDCKSw(4)*t3
     &           )
     &    + s3o2*( eosJMDCKSw(5)
     &           + eosJMDCKSw(6)*t
     &           + eosJMDCKSw(7)*t2
     &           )
C     secant bulk modulus of sea water at pressure p
            bMpres =
     &         p*( eosJMDCKP(1)
     &           + eosJMDCKP(2)*t
     &           + eosJMDCKP(3)*t2
     &           + eosJMDCKP(4)*t3
     &           )
     &     + p*s*( eosJMDCKP(5)
     &           + eosJMDCKP(6)*t
     &           + eosJMDCKP(7)*t2
     &           )
     &      + p*s3o2*eosJMDCKP(8)
     &      + p2*( eosJMDCKP(9)
     &           + eosJMDCKP(10)*t
     &           + eosJMDCKP(11)*t2
     &           )
     &    + p2*s*( eosJMDCKP(12)
     &           + eosJMDCKP(13)*t
     &           + eosJMDCKP(14)*t2
     &           )
#endif /* USE_FACTORIZED_EOS */

            bulkMod(i,j) = bMfresh + bMsalt + bMpres

         ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHONUM
C     !INTERFACE:
      SUBROUTINE FIND_RHONUM(
     I                iMin, iMax, jMin, jMax,
     I                locPres, tFld, sFld,
     O                rhoNum,
     I                myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHONUM
C     |   Calculates the numerator of the McDougall et al.
C     |   equation of state
C     |   - the code is more or less a copy of MOM4
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER iMin,iMax,jMin,jMax
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoNum (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL t1, t2, s1, p1
CEOP
      DO j=jMin,jMax
         DO i=iMin,iMax
C     abbreviations
            t1  = tFld(i,j)
            t2 = t1*t1
            s1  = sFld(i,j)

            p1   = locPres(i,j)*SItodBar

            rhoNum(i,j) = eosMDJWFnum(0)
     &           + t1*(eosMDJWFnum(1)
     &           +     t1*(eosMDJWFnum(2) + eosMDJWFnum(3)*t1) )
     &           + s1*(eosMDJWFnum(4)
     &           +     eosMDJWFnum(5)*t1  + eosMDJWFnum(6)*s1)
     &           + p1*(eosMDJWFnum(7) + eosMDJWFnum(8)*t2
     &           +     eosMDJWFnum(9)*s1
     &           +     p1*(eosMDJWFnum(10) + eosMDJWFnum(11)*t2) )

         ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHODEN
C     !INTERFACE:
      SUBROUTINE FIND_RHODEN(
     I                iMin, iMax, jMin, jMax,
     I                locPres, tFld, sFld,
     O                rhoDen,
     I                myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHODEN
C     |   Calculates the denominator of the McDougall et al.
C     |   equation of state
C     |   - the code is more or less a copy of MOM4
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER iMin,iMax,jMin,jMax
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoDen (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL t1, t2, s1, sp5, p1, p1t1
      _RL den, epsln
      parameter ( epsln = 0. _d 0 )
CEOP
      DO j=jMin,jMax
         DO i=iMin,iMax
C     abbreviations
            t1  = tFld(i,j)
            t2 = t1*t1
            s1  = sFld(i,j)
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

            den = eosMDJWFden(0)
     &           + t1*(eosMDJWFden(1)
     &           +     t1*(eosMDJWFden(2)
     &           +         t1*(eosMDJWFden(3) + t1*eosMDJWFden(4) ) ) )
     &           + s1*(eosMDJWFden(5)
     &           +     t1*(eosMDJWFden(6)
     &           +         eosMDJWFden(7)*t2)
     &           +     sp5*(eosMDJWFden(8) + eosMDJWFden(9)*t2) )
     &           + p1*(eosMDJWFden(10)
     &           +     p1t1*(eosMDJWFden(11)*t2 + eosMDJWFden(12)*p1) )

            rhoDen(i,j) = 1.0/(epsln+den)

         ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHOTEOS
C     !INTERFACE:
      SUBROUTINE FIND_RHOTEOS(
     I                iMin, iMax, jMin, jMax,
     I                locPres, tFld, sFld,
     O                rhoNum, rhoDen,
     I                myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHOTEOS
C     |   Calculates numerator and inverse of denominator of the
C     |   TEOS-10 (McDougall et al. 2011) equation of state
C     |   - the code is more or less a copy the teos-10 toolbox
C     |     available at http://www.teos-10.org
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER iMin,iMax,jMin,jMax
      _RL locPres(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoNum (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoDen (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL ct, sa, sqrtsa, p
      _RL den, epsln
      parameter ( epsln = 0. _d 0 )
CEOP
      DO j=jMin,jMax
       DO i=iMin,iMax
C     abbreviations
        ct      = tFld(i,j)
        sa      = sFld(i,j)
#if (defined ALLOW_AUTODIFF && defined TARGET_NEC_SX)
C     this line makes TAF create vectorizable code
        sqrtsa = 0. _d 0
#endif
        IF ( sa .GT. 0. _d 0 ) THEN
         sqrtsa = SQRT(sa)
        ELSE
         sa     = 0. _d 0
         sqrtsa = 0. _d 0
        ENDIF
        p       = locPres(i,j)*SItodBar

        rhoNum(i,j) = teos(01)
     &   + ct*(teos(02) + ct*(teos(03) + teos(04)*ct))
     &   + sa*(teos(05) + ct*(teos(06) + teos(07)*ct)
     &   + sqrtsa*(teos(08) + ct*(teos(09)
     &                      + ct*(teos(10) + teos(11)*ct))))
     &   + p*(teos(12) + ct*(teos(13) + teos(14)*ct)
     &                 + sa*(teos(15) + teos(16)*ct)
     &   + p*(teos(17) + ct*(teos(18) + teos(19)*ct) + teos(20)*sa))

        den = teos(21)
     &   + ct*(teos(22) + ct*(teos(23) + ct*(teos(24) + teos(25)*ct)))
     &   + sa*(teos(26) + ct*(teos(27) + ct*(teos(28)
     &   + ct*(teos(29) + teos(30)*ct)))
     &   + teos(36)*sa
     &   + sqrtsa*(teos(31) + ct*(teos(32) + ct*(teos(33)
     &            + ct*(teos(34) + teos(35)*ct)))))
     &   + p*(teos(37) + ct*(teos(38) + ct*(teos(39) + teos(40)*ct))
     &   + sa*(teos(41) + teos(42)*ct)
     &   + p*(teos(43) + ct*(teos(44) + teos(45)*ct + teos(46)*sa)
     &   + p*(teos(47) + teos(48)*ct)))

        rhoDen(i,j) = 1.0/(epsln+den)

       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHO_SCALAR
C     !INTERFACE:
      SUBROUTINE FIND_RHO_SCALAR(
     I     tLoc, sLoc, pLoc,
     O     rhoLoc,
     I     myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHO_SCALAR
C     |   Calculates rho(S,T,p)
C     | Note: for seawater EOS, p is pressure (Pa) relative to
C     |   reference surface pressure "surf_pRef"
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      _RL tLoc, sLoc, pLoc
      _RL rhoLoc
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      _RL t1, t2, t3, t4, s1, s3o2, p1, p2, sp5, p1t1
      _RL dp0, ct, sa, sqrtsa, p
      _RL rfresh, rsalt, rhoP0
      _RL bMfresh, bMsalt, bMpres, BulkMod
      _RL rhoNum, rhoDen, den, epsln
      _RL oneMinusKap, facPres, theta_v
      PARAMETER ( epsln = 0. _d 0 )
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef USE_FACTORIZED_EOS
      _RL eosF1, eosF2, eosF3, eosF4, eosF5, eosF6
      _RL eosS1, eosS2, eosS3, eosS4, eosS5, eosS6
      _RL eosS7, eosS8, eosS9
      _RL eosP1, eosP2, eosP3, eosP4, eosP5, eosP6, eosP7
      _RL eosP8, eosP9, eosP10, eosP11, eosP12, eosP13, eosP14
#endif /* USE_FACTORIZED_EOS */
CEOP
#ifdef USE_FACTORIZED_EOS
      eosF1 = eosJMDCFw(1)
      eosF2 = eosJMDCFw(2)
      eosF3 = eosJMDCFw(3)
      eosF4 = eosJMDCFw(4)
      eosF5 = eosJMDCFw(5)
      eosF6 = eosJMDCFw(6)
      eosS1 = eosJMDCSw(1)
      eosS2 = eosJMDCSw(2)
      eosS3 = eosJMDCSw(3)
      eosS4 = eosJMDCSw(4)
      eosS5 = eosJMDCSw(5)
      eosS6 = eosJMDCSw(6)
      eosS7 = eosJMDCSw(7)
      eosS8 = eosJMDCSw(8)
      eosS9 = eosJMDCSw(9)

      eosP1  =eosJMDCKP(1)
      eosP2  =eosJMDCKP(2)
      eosP3  =eosJMDCKP(3)
      eosP4  =eosJMDCKP(4)
      eosP5  =eosJMDCKP(5)
      eosP6  =eosJMDCKP(6)
      eosP7  =eosJMDCKP(7)
      eosP8  =eosJMDCKP(8)
      eosP9  =eosJMDCKP(9)
      eosP10 =eosJMDCKP(10)
      eosP11 =eosJMDCKP(11)
      eosP12 =eosJMDCKP(12)
      eosP13 =eosJMDCKP(13)
      eosP14 =eosJMDCKP(14)
#endif /* USE_FACTORIZED_EOS */

      rhoLoc  = 0. _d 0
      rhoP0   = 0. _d 0
      bulkMod = 0. _d 0
      rfresh  = 0. _d 0
      rsalt   = 0. _d 0
      bMfresh = 0. _d 0
      bMsalt  = 0. _d 0
      bMpres  = 0. _d 0
      rhoNum  = 0. _d 0
      rhoDen  = 0. _d 0
      den     = 0. _d 0
      dp0     = surf_pRef - eosRefP0

      t1 = tLoc
      t2 = t1*t1
      t3 = t2*t1
      t4 = t3*t1

      s1  = sLoc
      IF ( s1 .LT. 0. _d 0 ) THEN
C     issue a warning
         WRITE(msgBuf,'(A,E13.5)')
     &        ' FIND_RHO_SCALAR:   WARNING, salinity = ', s1
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         s1 = 0. _d 0
      ENDIF

      IF (equationOfState.EQ.'LINEAR') THEN

         rholoc = rhoNil*(
     &                      sBeta *(sLoc-sRef(1))
     &                     -tAlpha*(tLoc-tRef(1))
     &                   ) + rhoNil
c        rhoLoc = 0. _d  0

      ELSEIF (equationOfState.EQ.'POLY3') THEN

C     this is not correct, there is a field eosSig0 which should be use here
C     but I DO not intent to include the reference level in this routine
         WRITE(msgBuf,'(A)')
     &        ' FIND_RHO_SCALAR: for POLY3, the density is not'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         WRITE(msgBuf,'(A)')
     &         '                 computed correctly in this routine'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT , myThid )
         rhoLoc = 0. _d 0

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &      .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates

         s3o2 = s1*SQRT(s1)

         p1 = ( pLoc + dp0 )*SItoBar
         p2 = p1*p1

C     density of freshwater at the surface
#ifdef USE_FACTORIZED_EOS
         rfresh =
     &              eosF1 +
     &        t1* ( eosF2 +
     &        t1* ( eosF3 +
     &        t1* ( eosF4 +
     &        t1* ( eosF5 +
     &        t1*   eosF6  ))))
#else
         rfresh =
     &          eosJMDCFw(1)
     &        + eosJMDCFw(2)*t1
     &        + eosJMDCFw(3)*t2
     &        + eosJMDCFw(4)*t3
     &        + eosJMDCFw(5)*t4
     &        + eosJMDCFw(6)*t4*t1
#endif /* USE_FACTORIZED_EOS */
C     density of sea water at the surface
#ifdef USE_FACTORIZED_EOS
         rsalt =   s1 * ( eosS1 +
     &             t1 * ( eosS2 +
     &             t1 * ( eosS3 +
     &             t1 * ( eosS4 +
     &             t1 *   eosS5  ))))
     &         + s3o2 * ( eosS6 +
     &             t1 * ( eosS7 +
     &             t1 *   eosS8  ))
     &         + s1*s1*   eosS9
#else
         rsalt =
     &        s1*(
     &             eosJMDCSw(1)
     &           + eosJMDCSw(2)*t1
     &           + eosJMDCSw(3)*t2
     &           + eosJMDCSw(4)*t3
     &           + eosJMDCSw(5)*t4
     &           )
     &        + s3o2*(
     &             eosJMDCSw(6)
     &           + eosJMDCSw(7)*t1
     &           + eosJMDCSw(8)*t2
     &           )
     &           + eosJMDCSw(9)*s1*s1
#endif /* USE_FACTORIZED_EOS */

         rhoP0 = rfresh + rsalt

C     secant bulk modulus of fresh water at the surface
#ifdef USE_FACTORIZED_EOS
         bMfresh =   eosF1 +
     &        t1 * ( eosF2 +
     &        t1 * ( eosF3 +
     &        t1 * ( eosF4 +
     &        t1 *   eosF5  )))
#else
         bMfresh =
     &             eosJMDCKFw(1)
     &           + eosJMDCKFw(2)*t1
     &           + eosJMDCKFw(3)*t2
     &           + eosJMDCKFw(4)*t3
     &           + eosJMDCKFw(5)*t4
#endif /* USE_FACTORIZED_EOS */
C     secant bulk modulus of sea water at the surface
#ifdef USE_FACTORIZED_EOS
         bMsalt = s1 * ( eosS1 +
     &            t1 * ( eosS2 +
     &            t1 * ( eosS3 +
     &            t1 *   eosS4  )))
     &        + s3o2 * ( eosS5 +
     &            t1 * ( eosS6 +
     &            t1 *   eosS7  ))
C     secant bulk modulus of sea water at pressure p
         bMpres =  p1 * ( eosP1 +
     &             t1 * ( eosP2 +
     &             t1 * ( eosP3 +
     &             t1 *   eosP4  )))
     &          + p1*s1*( eosP5 +
     &             t1 * ( eosP6 +
     &             t1 *   eosP7  ))
     &          + p1*s3o2*eosP8
     &          + p2  * ( eosP9 +
     &             t1 * ( eosP10 +
     &             t1 *   eosP11  ))
     &         + p2*s1* ( eosP12 +
     &             t1 * ( eosP13 +
     &             t1 *   eosP14 ))
#else
C     secant bulk modulus of sea water at the surface
         bMsalt =
     &        s1*( eosJMDCKSw(1)
     &           + eosJMDCKSw(2)*t1
     &           + eosJMDCKSw(3)*t2
     &           + eosJMDCKSw(4)*t3
     &           )
     &    + s3o2*( eosJMDCKSw(5)
     &           + eosJMDCKSw(6)*t1
     &           + eosJMDCKSw(7)*t2
     &           )
C     secant bulk modulus of sea water at pressure p
         bMpres =
     &        p1*( eosJMDCKP(1)
     &           + eosJMDCKP(2)*t1
     &           + eosJMDCKP(3)*t2
     &           + eosJMDCKP(4)*t3
     &           )
     &   + p1*s1*( eosJMDCKP(5)
     &           + eosJMDCKP(6)*t1
     &           + eosJMDCKP(7)*t2
     &           )
     &      + p1*s3o2*eosJMDCKP(8)
     &      + p2*( eosJMDCKP(9)
     &           + eosJMDCKP(10)*t1
     &           + eosJMDCKP(11)*t2
     &           )
     &    + p2*s1*( eosJMDCKP(12)
     &           + eosJMDCKP(13)*t1
     &           + eosJMDCKP(14)*t2
     &           )
#endif /* USE_FACTORIZED_EOS */

         bulkMod = bMfresh + bMsalt + bMpres

C     density of sea water at pressure p
         rhoLoc = rhoP0/(1. _d 0 - p1/bulkMod)

      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN

       sp5 = SQRT(s1)

       p1 = ( pLoc + dp0 )*SItodBar
       p1t1 = p1*t1

       rhoNum = eosMDJWFnum(0)
     &      + t1*(eosMDJWFnum(1)
     &      +     t1*(eosMDJWFnum(2) + eosMDJWFnum(3)*t1) )
     &      + s1*(eosMDJWFnum(4)
     &      +     eosMDJWFnum(5)*t1  + eosMDJWFnum(6)*s1)
     &      + p1*(eosMDJWFnum(7) + eosMDJWFnum(8)*t2
     &      +     eosMDJWFnum(9)*s1
     &      +     p1*(eosMDJWFnum(10) + eosMDJWFnum(11)*t2) )

       den = eosMDJWFden(0)
     &      + t1*(eosMDJWFden(1)
     &      +     t1*(eosMDJWFden(2)
     &      +         t1*(eosMDJWFden(3) + t1*eosMDJWFden(4) ) ) )
     &      + s1*(eosMDJWFden(5)
     &      +     t1*(eosMDJWFden(6)
     &      +         eosMDJWFden(7)*t2)
     &      +     sp5*(eosMDJWFden(8) + eosMDJWFden(9)*t2) )
     &      + p1*(eosMDJWFden(10)
     &      +     p1t1*(eosMDJWFden(11)*t2 + eosMDJWFden(12)*p1) )

       rhoDen = 1.0/(epsln+den)

       rhoLoc = rhoNum*rhoDen

      ELSEIF( equationOfState .EQ. 'TEOS10' ) THEN

       ct      = tLoc
       sa      = sLoc
       IF ( sa .GT. 0. _d 0 ) THEN
        sqrtsa = SQRT(sa)
       ELSE
        sa     = 0. _d 0
        sqrtsa = 0. _d 0
       ENDIF
       p = ( pLoc + dp0 )*SItodBar

       rhoNum = teos(01)
     &   + ct*(teos(02) + ct*(teos(03) + teos(04)*ct))
     &   + sa*(teos(05) + ct*(teos(06) + teos(07)*ct)
     &   + sqrtsa*(teos(08) + ct*(teos(09)
     &            + ct*(teos(10) + teos(11)*ct))))
     &   + p*(teos(12) + ct*(teos(13) + teos(14)*ct)
     &                      + sa*(teos(15) + teos(16)*ct)
     &   + p*(teos(17) + ct*(teos(18) + teos(19)*ct) + teos(20)*sa))

       den = teos(21)
     &   + ct*(teos(22) + ct*(teos(23) + ct*(teos(24) + teos(25)*ct)))
     &   + sa*(teos(26) + ct*(teos(27) + ct*(teos(28)
     &   + ct*(teos(29) + teos(30)*ct)))
     &   + teos(36)*sa
     %   + sqrtsa*(teos(31) + ct*(teos(32) + ct*(teos(33)
     &            + ct*(teos(34) + teos(35)*ct)))))
     %   + p*(teos(37) + ct*(teos(38) + ct*(teos(39) + teos(40)*ct))
     %   + sa*(teos(41) + teos(42)*ct)
     %   + p*(teos(43) + ct*(teos(44) + teos(45)*ct + teos(46)*sa)
     %   + p*(teos(47) + teos(48)*ct)))

       rhoDen = 1.0/(epsln+den)

       rhoLoc = rhoNum*rhoDen

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN

         oneMinusKap = oneRL - atm_kappa
C        rho = P/(R*T_v) = Po/(R*theta_v)*(P/Po)^(1-kappa)
         facPres = ( pLoc/atm_Po )**oneMinusKap
         theta_v = tLoc*( sLoc*atm_Rq + oneRL )
         rhoLoc = atm_Po*facPres /( atm_Rd*theta_v )

      ELSE
       WRITE(msgBuf,'(3A)')
     &        ' FIND_RHO_SCALAR : equationOfState = "',
     &        equationOfState,'"'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R FIND_RHO_SCALAR'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: LOOK_FOR_NEG_SALINITY
C     !INTERFACE:
      SUBROUTINE LOOK_FOR_NEG_SALINITY(
     I                iMin, iMax, jMin, jMax,
     U                sFld,
     I                k, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE LOOK_FOR_NEG_SALINITY
C     |   looks for and fixes negative salinity values
C     |   this is necessary IF the equation of state uses
C     |   the square root of salinity
C     *==========================================================*
C     | k - is the Salt level
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k    :: Level of Salt slice
      INTEGER iMin,iMax,jMin,jMax
      _RL     sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k, bi, bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j, localWarning
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      localWarning = 0
      DO j=jMin,jMax
       DO i=iMin,iMax
C     abbreviations
        IF ( sFld(i,j) .LT. 0. _d 0 ) THEN
         localWarning = localWarning + 1
         sFld(i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO
C     issue a warning
      IF ( localWarning .GT. 0 ) THEN
        WRITE(msgBuf,'(2A,I5,A,2I4)') 'S/R LOOK_FOR_NEG_SALINITY:',
     &      ' from level k =', k, ' ; bi,bj =', bi, bj
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
        WRITE(msgBuf,'(2A,I6,A)') 'S/R LOOK_FOR_NEG_SALINITY:',
     &      ' reset to zero', localWarning, ' negative salinity.'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT , myThid )
      ENDIF

      RETURN
      END
