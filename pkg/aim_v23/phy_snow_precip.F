C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/phy_snow_precip.F,v 1.1 2004/03/11 14:33:19 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: SNOW_PRECIP
C     !INTERFACE:
      SUBROUTINE SNOW_PRECIP (
     I                   PSA, dpFac, ThA,
     I                   IDEPTH,PRECNV,DQCNV,PRECLS,DQLSC,
     U                   DTCNV,DTLSC,
     O                   EnPrec,
     I                   kGrd,bi,bj,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SNOW_PRECIP
C     | o Diagnose snow precipitation according to near surface 
C     |   air temperature. 
C     *==========================================================*
C     | Correct condensation heating for energy (rain Temp, snow) 
C     |  of precipication (L.S. cond & convect. precip)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===

C     Resolution parameters
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 
#include "EEPARAMS.h"

C     Physical constants + functions of sigma and latitude
#include "com_physcon.h"

C     == Routine Arguments ==
C     !INPUT PARAMETERS:
C       kGrd   :: Ground level index                       (2-dim)
C       bi,bj  :: tile index
C       myThid :: Thread number for this instance of the routine
C       PSA    :: norm. surface pressure [p/p0]            (2-dim)
C       dpFac  :: cell delta_P fraction                    (3-dim)
C       ThA    :: Pot.temperature    [K]                   (3-dim)
C       IDEPTH :: convection depth in layers               (2-dim)
C       PRECNV :: convective precipitation [g/(m^2 s)]     (2-dim)
C       DQCNV  :: hum. tendency [g/(kg s)] from convection (3-dim)
C       PRECLS :: large-scale precipitation [g/(m^2 s)]    (2-dim)
C       DQLSC  :: hum. tendency [g/(kg s)] from l.s. cond  (3-dim)
C     !MODIFIED PARAMETERS:
C       DTCNV  :: temperature tendency from convection     (3-dim)
C       DTLSC  :: temperature tendency from l.s. cond      (3-dim)
C     !OUTPUT PARAMETERS:
C       EnPrec :: energy of precipitation (snow, rain temp) [J/g]
C--
      _RL PSA(NGP), dpFac(NGP,NLEV), ThA(NGP,NLEV)
      _RL PRECLS(NGP), DTLSC(NGP,NLEV), DQLSC(NGP,NLEV)
      _RL PRECNV(NGP), DTCNV(NGP,NLEV), DQCNV(NGP,NLEV)
      _RL EnPrec(NGP)
      INTEGER  IDEPTH(NGP)
      INTEGER  kGrd(NGP)
      INTEGER  bi,bj,myThid
CEOP

#ifdef ALLOW_AIM

C-- Local variables:  
      INTEGER J, K, Ktop
      _RL kappa
      _RL T1(NGP)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   1. Initialization
      kappa = RD/CP

C--   2. Compute energy of precip, based on near-surface Air temperature:
C     note: EnPrec units are J/g since precip is in g/m2 and spec.hum. in g/Kg
      DO J=1,NGP
       IF ( kGrd(J).NE.0 ) THEN
        T1(J) = ThA(J,kGrd(J))*(PSA(J)**kappa)
        IF ( T1(J) .GE. tFreeze ) THEN
         EnPrec(J) = rainCP*(T1(J)-tFreeze)
        ELSE
         EnPrec(J) = -ALHF
        ENDIF
       ELSE
        EnPrec(J) = 0.
       ENDIF
      ENDDO

C--   3. Large-scale precipitation: correct condensation Heating

      DO J=1,NGP
        DO K=2,kGrd(J)
          DTLSC(J,K) = DTLSC(J,K) + EnPrec(J)*DQLSC(J,K)/CP
        ENDDO
      ENDDO

C--   4. Convective precipitation: correct condensation Heating

      DO J=1,NGP
        IF (kGrd(J).NE.0 .AND. IDEPTH(J).NE.0 ) THEN
          Ktop = kGrd(J) - IDEPTH(J)
          DTCNV(J,Ktop) = DTCNV(J,Ktop) 
     &                  - EnPrec(J)*PRECNV(J)*GRDSCP(Ktop)
        ENDIF
      ENDDO

#endif /* ALLOW_AIM */ 

      RETURN
      END
