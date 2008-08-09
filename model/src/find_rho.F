C $Header: /u/gcmpack/MITgcm/model/src/find_rho.F,v 1.35 2008/08/09 01:02:28 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#define USE_FACTORIZED_POLY

C--  File find_rho.F: Routines to compute density
C--   Contents
C--   o FIND_RHO
C--   o FIND_RHO_2D
C--   o FIND_RHOP0
C--   o FIND_BULKMOD
C--   o FIND_RHONUM
C--   o FIND_RHODEN
C--   o LOOK_FOR_NEG_SALINITY

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHO
C     !INTERFACE:
      SUBROUTINE FIND_RHO(
     I      bi, bj, iMin, iMax, jMin, jMax,  k, kRef,
     I      tFld, sFld,
     O      rholoc,
     I      myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHO
C     |   Calculates [rho(S,T,z)-rhoConst] of a slice
C     *==========================================================*
C     |
C     | k - is the Theta/Salt level
C     | kRef - determines pressure reference level
C     |        (not used in 'LINEAR' mode)
C     |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k    :: Level of Theta/Salt slice
C     kRef :: Pressure reference level
      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      INTEGER kRef
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL rholoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
CEOP

      CALL FIND_RHO_2D(
     I      iMin, iMax, jMin, jMax, kRef,
     I      tFld(1-OLx,1-OLy,k,bi,bj), sFld(1-OLx,1-OLy,k,bi,bj),
     O      rholoc,
     I      k, bi, bj, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FIND_RHO_2D
C     !INTERFACE:
      SUBROUTINE FIND_RHO_2D(
     I                iMin, iMax, jMin, jMax, kRef,
     I                tFld, sFld,
     O                rholoc,
     I                k, bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | o SUBROUTINE FIND_RHO_2D
C     |   Calculates [rho(S,T,z)-rhoConst] of a 2-D slice
C     *==========================================================*
C     |
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
c#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k    :: Level of Theta/Salt slice
C     kRef :: Pressure reference level
      INTEGER iMin,iMax,jMin,jMax
      INTEGER kRef
      _RL tFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rholoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER k, bi, bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL refTemp,refSalt,sigRef,tP,sP,deltaSig,dRho
      _RL locPres(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoP0  (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoNum (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL rhoDen (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

#ifdef ALLOW_AUTODIFF_TAMC
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        rholoc(i,j)  = 0. _d 0
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
         rholoc(i,j)=rhoNil*(
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
         rholoc(i,j)=sigRef+deltaSig
        ENDDO
       ENDDO

      ELSEIF ( equationOfState(1:5).EQ.'JMD95'
     &      .OR. equationOfState.EQ.'UNESCO' ) THEN
C     nonlinear equation of state in pressure coordinates

         CALL PRESSURE_FOR_EOS(
     I             bi, bj, iMin, iMax, jMin, jMax, kRef,
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

#ifdef ALLOW_AUTODIFF_TAMC
cph can not DO storing here since find_rho is called multiple times;
cph additional recomp. should be acceptable
cphCADJ STORE rhoP0(:,:)   = comlev1_bibj_k ,  key=kkey , byte=isbyte
cphCADJ STORE bulkMod(:,:) = comlev1_bibj_k ,  key=kkey , byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax

C     density of sea water at pressure p
            rholoc(i,j) = rhoP0(i,j)
     &              /(1. _d 0 -
     &              locPres(i,j)*SItoBar/bulkMod(i,j) )
     &              - rhoConst

          ENDDO
         ENDDO

      ELSEIF ( equationOfState.EQ.'MDJWF' ) THEN

         CALL PRESSURE_FOR_EOS(
     I             bi, bj, iMin, iMax, jMin, jMax, kRef,
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
#ifdef ALLOW_AUTODIFF_TAMC
cph can not DO storing here since find_rho is called multiple times;
cph additional recomp. should be acceptable
cphCADJ STORE rhoNum(:,:) = comlev1_bibj_k ,  key=kkey , byte=isbyte
cphCADJ STORE rhoDen(:,:) = comlev1_bibj_k ,  key=kkey , byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */
         DO j=jMin,jMax
          DO i=iMin,iMax
            rholoc(i,j) = rhoNum(i,j)*rhoDen(i,j) - rhoConst
          ENDDO
         ENDDO

      ELSEIF( equationOfState .EQ. 'IDEALG' ) THEN
C
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
      _RL rhoP0(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL rfresh, rsalt
      _RL t, t2, t3, t4, s, s3o2
CEOP

      DO j=jMin,jMax
         DO i=iMin,iMax
C     abbreviations
            t  = tFld(i,j)
            t2 = t*t
            t3 = t2*t
            t4 = t3*t

            s  = sFld(i,j)
           IF ( s .GT. 0. _d 0 ) THEN
            s3o2 = s*SQRT(s)
           ELSE
            s    = 0. _d 0
            s3o2 = 0. _d 0
           ENDIF

C     density of freshwater at the surface
            rfresh =
     &             eosJMDCFw(1)
     &           + eosJMDCFw(2)*t
     &           + eosJMDCFw(3)*t2
     &           + eosJMDCFw(4)*t3
     &           + eosJMDCFw(5)*t4
     &           + eosJMDCFw(6)*t4*t
C     density of sea water at the surface
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

            rhoP0(i,j) = rfresh + rsalt

         ENDDO
      ENDDO

      RETURN
      END

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
C     |
C     | k    - is the level of Theta/Salt slice
C     |
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
      _RL locPres(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL bulkMod(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL bMfresh, bMsalt, bMpres
      _RL t, t2, t3, t4, s, s3o2, p, p2
CEOP

      DO j=jMin,jMax
         DO i=iMin,iMax
C     abbreviations
            t  = tFld(i,j)
            t2 = t*t
            t3 = t2*t
            t4 = t3*t

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
            bMfresh =
     &             eosJMDCKFw(1)
     &           + eosJMDCKFw(2)*t
     &           + eosJMDCKFw(3)*t2
     &           + eosJMDCKFw(4)*t3
     &           + eosJMDCKFw(5)*t4
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

            bulkMod(i,j) = bMfresh + bMsalt + bMpres

         ENDDO
      ENDDO

      RETURN
      END

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
      _RL locPres(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoNum (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
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
      _RL locPres(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL tFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sFld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rhoDen (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
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
C     |
C     | k - is the Salt level
C     |
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     k    :: Level of Salt slice
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      _RL     sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j, localWarning
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
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
       WRITE(standardMessageUnit,'(A,I6,A)')
     &      'S/R LOOK_FOR_NEG_SALINITY: found',localWarning,
     &      ' negative salinity values and reset them to zero.'
       WRITE(standardMessageUnit,'(A,I3)')
     &      'S/R LOOK_FOR_NEG_SALINITY: current level is k = ', k
      ENDIF

      RETURN
      END
