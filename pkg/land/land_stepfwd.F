C $Header: /u/gcmpack/MITgcm/pkg/land/land_stepfwd.F,v 1.7 2007/10/01 13:31:15 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_STEPFWD
C     !INTERFACE:
      SUBROUTINE LAND_STEPFWD(
     I                land_frc, bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_STEPFWD
C     | o Land model main S/R: step forward land variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_LAND
C     == Local variables ==
C     i,j,k        :: loop counters
C     kp1          :: k+1
C     grd_HeatCp   :: Heat capacity of the ground [J/m3/K]
C     enthalpGrdW  :: enthalpy of ground water [J/m3]
C     fieldCapac   :: field capacity (of water) [m]
C     mWater       :: water content of the ground [kg/m3]
C     groundWnp1   :: hold temporary future soil moisture []
C     grdWexcess   :: ground water in excess [m/s]
C     fractRunOff  :: fraction of water in excess which leaves as runoff
C     flxkup       :: downward flux of water, upper interface (k-1,k)
C     flxdwn       :: downward flux of water, lower interface (k,k+1)
C     flxEngU      :: downward energy flux associated with water flux (W/m2)
C                     upper interface (k-1,k)
C     flxEngL      :: downward energy flux associated with water flux (W/m2)
C                     lower interface (k,k+1)
C     temp_af      :: ground temperature if above freezing
C     temp_bf      :: ground temperature if below freezing
C     mPmE         :: hold temporary (liquid) Precip minus Evap [kg/m2/s]
C     enWfx        :: hold temporary energy flux of Precip [W/m2]
C     enGr1        :: ground enthalpy of level 1  [J/m2]
C     mSnow        :: mass of snow         [kg/m2]
C     dMsn         :: mass of melting snow [kg/m2]
C     snowPrec     :: snow precipitation [kg/m2/s]
C     hNewSnow     :: fresh snow accumulation [m]
C     dhSnowMx     :: potential snow increase [m]
C     dhSnow       :: effective snow increase [m]
C     mIceDt       :: ground-ice growth rate (<- excess of snow) [kg/m2/s]
C     ageFac       :: snow aging factor [1]
      _RL grd_HeatCp, enthalpGrdW
      _RL fieldCapac, mWater
      _RL groundWnp1, grdWexcess, fractRunOff
      _RL flxkup(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxkdw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxEngU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxEngL, temp_af, temp_bf, mPmE, enWfx, enGr1
      _RL mSnow, dMsn, snowPrec
      _RL hNewSnow, dhSnowMx, dhSnow, mIceDt, ageFac
      INTEGER i,j,k,kp1

#ifdef LAND_DEBUG
      LOGICAL dBug
      INTEGER iprt,jprt,lprt
      DATA iprt, jprt , lprt / 19 , 20 , 6 /
 1010 FORMAT(A,I3,1P4E11.3)
#endif

      IF (land_calc_grT .AND. .NOT.land_impl_grT ) THEN
C--   Step forward ground temperature:

      DO k=1,land_nLev
       kp1 = MIN(k+1,land_nLev)

       IF (k.EQ.1) THEN
        DO j=1,sNy
         DO i=1,sNx
           flxkup(i,j) = land_HeatFlx(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
           flxkup(i,j) = flxkdw(i,j)
         ENDDO
        ENDDO
       ENDIF

       DO j=1,sNy
        DO i=1,sNx
         IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
C-     Thermal conductivity flux, lower interface (k,k+1):
          flxkdw(i,j) = land_grdLambda*
     &             ( land_groundT(i,j,k,bi,bj)
     &              -land_groundT(i,j,kp1,bi,bj) )
     &            *land_rec_dzC(kp1)

C-     Step forward ground enthalpy, level k :
          land_enthalp(i,j,k,bi,bj) = land_enthalp(i,j,k,bi,bj)
     &       + land_deltaT * (flxkup(i,j)-flxkdw(i,j))/land_dzF(k)

         ENDIF
        ENDDO
       ENDDO

      ENDDO
C--   step forward ground temperature: end
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( land_calc_grW .OR. land_calc_snow ) THEN
C--   Initialize run-off arrays.
        DO j=1,sNy
         DO i=1,sNx
           land_runOff(i,j,bi,bj) = 0. _d 0
           land_enRnOf(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
      ENDIF

#ifdef LAND_OLD_VERSION
      IF ( .TRUE. ) THEN
#else
      IF ( land_calc_grW ) THEN
#endif
C--   need (later on) ground temp. to be consistent with updated enthalpy:
        DO k=1,land_nLev
         DO j=1,sNy
          DO i=1,sNx
           IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
            mWater = land_rhoLiqW*land_waterCap
     &              *land_groundW(i,j,k,bi,bj)
            mWater = MAX( mWater, 0. _d 0 )
            grd_HeatCp = land_heatCs + land_CpWater*mWater
            temp_bf = (land_enthalp(i,j,k,bi,bj)+land_Lfreez*mWater)
     &                                           / grd_HeatCp
            temp_af =  land_enthalp(i,j,k,bi,bj) / grd_HeatCp
            land_groundT(i,j,k,bi,bj) =
     &              MIN( temp_bf, MAX(temp_af, 0. _d 0) )
#ifdef LAND_DEBUG
            dBug = bi.eq.lprt .AND. i.EQ.iprt .AND. j.EQ.jprt
            IF (dBug) write(6,1010)
     &        'LAND_STEPFWD: k,temp,af,bf=',
     &       k,land_groundT(i,j,k,bi,bj),temp_af,temp_bf
#endif
           ENDIF
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      IF ( land_calc_snow ) THEN
C--   Step forward Snow thickness (also account for rain temperature)
        ageFac = 1. _d 0 - land_deltaT/timeSnowAge
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
           mPmE  = land_Pr_m_Ev(i,j,bi,bj)
           enWfx = land_EnWFlux(i,j,bi,bj)
           enGr1 = land_enthalp(i,j,1,bi,bj)*land_dzF(1)
#ifdef LAND_DEBUG
           dBug = bi.eq.lprt .AND. i.EQ.iprt .AND. j.EQ.jprt
           IF (dBug) write(6,1010)
     &       'LAND_STEPFWD:mPmE,enWfx,enGr1/dt,hSnow=',0,
     &       mPmE,enWfx,enGr1/land_deltaT,land_hSnow(i,j,bi,bj)
#endif
C-    snow aging:
           land_snowAge(i,j,bi,bj) =
     &         ( land_deltaT + land_snowAge(i,j,bi,bj)*ageFac )
           IF ( enWfx.LT.0. ) THEN
C-    snow precip in excess ( > Evap of snow) or snow prec & Evap of Liq.Water:
C     => start to melt (until ground at freezing point) and then accumulate
            snowPrec = -enWfx -MAX( enGr1/land_deltaT, 0. _d 0 )
C-    snow accumulation cannot be larger that net precip
            snowPrec = MAX( 0. _d 0 ,
     &                      MIN( snowPrec*recip_Lfreez, mPmE ) )
            mPmE = mPmE - snowPrec
            flxEngU(i,j) = enWfx + land_Lfreez*snowPrec
            hNewSnow = land_deltaT * snowPrec / land_rhoSnow
C-    refresh snow age:
            land_snowAge(i,j,bi,bj) = land_snowAge(i,j,bi,bj)
     &                          *EXP( -hNewSnow/hNewSnowAge )
C-    update snow thickness:
c           land_hSnow(i,j,bi,bj) = land_hSnow(i,j,bi,bj) + hNewSnow
C     glacier & ice-sheet missing: excess of snow put directly into run-off
            dhSnowMx = MAX( 0. _d 0,
     &                      land_hMaxSnow - land_hSnow(i,j,bi,bj) )
            dhSnow = MIN( hNewSnow, dhSnowMx )
            land_hSnow(i,j,bi,bj) = land_hSnow(i,j,bi,bj) + dhSnow
            mIceDt = land_rhoSnow * (hNewSnow-dhSnow) / land_deltaT
            land_runOff(i,j,bi,bj) =  mIceDt
            land_enRnOf(i,j,bi,bj) = -mIceDt*land_Lfreez
#ifdef LAND_DEBUG
            IF (dBug) write(6,1010)
     &        'LAND_STEPFWD: 3,snP,mPmE,hNsnw,hSnw=',
     &         3,snowPrec,mPmE,hNewSnow,land_hSnow(i,j,bi,bj)
#endif
           ELSE
C-    rain precip (whatever Evap is) or Evap of snow exceeds snow precip:
C     => snow melts or sublimates
c           snowMelt = MIN( enWfx*recip_Lfreez ,
c    &                 land_hSnow(i,j,bi,bj)*land_rhoSnow/land_deltaT )
            mSnow = land_hSnow(i,j,bi,bj)*land_rhoSnow
            dMsn = enWfx*recip_Lfreez*land_deltaT
            IF ( dMsn .GE. mSnow ) THEN
              dMsn = mSnow
              land_hSnow(i,j,bi,bj) = 0. _d 0
              flxEngU(i,j) = enWfx - land_Lfreez*mSnow/land_deltaT
            ELSE
              flxEngU(i,j) = 0. _d 0
              land_hSnow(i,j,bi,bj) = land_hSnow(i,j,bi,bj)
     &                              - dMsn / land_rhoSnow
            ENDIF
c           IF (mPmE.GT.0.) land_snowAge(i,j,bi,bj) = timeSnowAge
            mPmE = mPmE + dMsn/land_deltaT
#ifdef LAND_DEBUG
            IF (dBug) write(6,1010)
     &        'LAND_STEPFWD: 4,dMsn,mPmE,hSnw,enWfx=',
     &         4,dMsn,mPmE,land_hSnow(i,j,bi,bj),flxEngU(i,j)
#endif
           ENDIF
           flxkup(i,j) = mPmE/land_rhoLiqW
c          land_Pr_m_Ev(i,j,bi,bj) = mPmE
           IF ( land_hSnow(i,j,bi,bj).LE. 0. _d 0 )
     &          land_snowAge(i,j,bi,bj) = 0. _d 0
C-    avoid negative (but very small, < 1.e-34) hSnow that occurs because
C      of truncation error. Might need to rewrite this part.
c          IF ( land_hSnow(i,j,bi,bj).LE. 0. _d 0 ) THEN
c             land_hSnow(i,j,bi,bj)   = 0. _d 0
c             land_snowAge(i,j,bi,bj) = 0. _d 0
c          ENDIF
          ENDIF
         ENDDO
        ENDDO
      ELSE
        DO j=1,sNy
         DO i=1,sNx
           flxkup(i,j) = land_Pr_m_Ev(i,j,bi,bj)/land_rhoLiqW
           flxEngU(i,j) = 0. _d 0
         ENDDO
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (land_calc_grW) THEN
C--   Step forward ground Water:

      DO k=1,land_nLev
       IF (k.EQ.land_nLev) THEN
        kp1 = k
        fractRunOff = 1. _d 0
       ELSE
        kp1 = k+1
        fractRunOff = land_fractRunOff
       ENDIF
       fieldCapac = land_waterCap*land_dzF(k)

       DO j=1,sNy
        DO i=1,sNx
         IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
#ifdef LAND_DEBUG
          dBug = bi.eq.lprt .AND. i.EQ.iprt .AND. j.EQ.jprt
#endif

#ifdef LAND_OLD_VERSION
          IF ( .TRUE. ) THEN
           IF ( k.EQ.land_nLev ) THEN
#else
          IF ( land_groundT(i,j,k,bi,bj).LT.0. _d 0 ) THEN
C-     Frozen level: only account for upper level fluxes
           IF ( flxkup(i,j) .LT. 0. _d 0 ) THEN
C-     Step forward soil moisture (& enthapy), level k :
            land_groundW(i,j,k,bi,bj) = land_groundW(i,j,k,bi,bj)
     &       + land_deltaT * flxkup(i,j) / fieldCapac
            IF ( land_calc_snow )
     &      land_enthalp(i,j,k,bi,bj) = land_enthalp(i,j,k,bi,bj)
     &       + land_deltaT * flxEngU(i,j) / land_dzF(k)
           ELSE
C-     Frozen level: incoming water flux goes directly into run-off
            land_runOff(i,j,bi,bj) = land_runOff(i,j,bi,bj)
     &                             + flxkup(i,j)*land_rhoLiqW
            land_enRnOf(i,j,bi,bj) = land_enRnOf(i,j,bi,bj)
     &                             + flxEngU(i,j)
           ENDIF
C-     prepare fluxes for next level:
           flxkup(i,j)  = 0. _d 0
           flxEngU(i,j) = 0. _d 0

          ELSE

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-     Diffusion flux of water, lower interface (k,k+1):
           IF ( k.EQ.land_nLev .OR.
     &         land_groundT(i,j,kp1,bi,bj).LT.0. _d 0 ) THEN
#endif /* LAND_OLD_VERSION */
C-     no Diffusion of water if one level is frozen :
             flxkdw(i,j) = 0. _d 0
             flxEngL =     0. _d 0
           ELSE
             flxkdw(i,j) = fieldCapac*
     &                   ( land_groundW(i,j,k,bi,bj)
     &                    -land_groundW(i,j,kp1,bi,bj) )
     &                   / land_wTauDiff
C-     energy flux associated with water flux: take upwind Temp
             IF ( flxkdw(i,j).GE.0. ) THEN
              flxEngL = flxkdw(i,j)*land_rhoLiqW*land_CpWater
     &                 *land_groundT(i,j,k,bi,bj)
             ELSE
              flxEngL = flxkdw(i,j)*land_rhoLiqW*land_CpWater
     &                 *land_groundT(i,j,kp1,bi,bj)
             ENDIF
           ENDIF

C-     Step forward soil moisture, level k :
           groundWnp1 = land_groundW(i,j,k,bi,bj)
     &       + land_deltaT * (flxkup(i,j)-flxkdw(i,j)) / fieldCapac

#ifdef LAND_DEBUG
           IF(dBug)write(6,1010)'LAND_STEPFWD: grdW-1,fx_ku,kd,grdW-1='
     &      ,5,land_groundW(i,j,k,bi,bj)-1.,
     &         flxkup(i,j),flxkdw(i,j),groundWnp1-1.
#endif

C-     Water in excess will leave as run-off or go to level below
           land_groundW(i,j,k,bi,bj) = MIN(1. _d 0, groundWnp1)
           grdWexcess = ( groundWnp1 - MIN(1. _d 0, groundWnp1) )
     &                 *fieldCapac/land_deltaT

C-     Run off: fraction 1-fractRunOff enters level below
           land_runOff(i,j,bi,bj) = land_runOff(i,j,bi,bj)
     &                        + fractRunOff*grdWexcess*land_rhoLiqW
C-     prepare fluxes for next level:
           flxkup(i,j) = flxkdw(i,j)
     &              + (1. _d 0-fractRunOff)*grdWexcess

           IF ( land_calc_snow ) THEN
            enthalpGrdW = land_rhoLiqW*land_CpWater
     &                   *land_groundT(i,j,k,bi,bj)
C--    Account for water fluxes in energy budget: update ground Enthalpy
            land_enthalp(i,j,k,bi,bj) = land_enthalp(i,j,k,bi,bj)
     &         + ( flxEngU(i,j) - flxEngL - grdWexcess*enthalpGrdW
     &           )*land_deltaT/land_dzF(k)

            land_enRnOf(i,j,bi,bj) = land_enRnOf(i,j,bi,bj)
     &                        + fractRunOff*grdWexcess*enthalpGrdW
C-     prepare fluxes for next level:
            flxEngU(i,j) = flxEngL
     &              + (1. _d 0-fractRunOff)*grdWexcess*enthalpGrdW
           ENDIF
#ifdef LAND_DEBUG
           IF (dBug) write(6,1010) 'LAND_STEPFWD: Temp,FlxE,FlxW=',
     &      7, land_groundT(i,j,k,bi,bj), flxEngU(i,j), flxkup(i,j)
#endif
          ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef LAND_DEBUG
           IF (dBug) write(6,1010) 'LAND_STEPFWD: RO,enRO=',
     &      8, land_runOff(i,j,bi,bj),land_enRnOf(i,j,bi,bj)
#endif

         ENDIF
        ENDDO
       ENDDO

      ENDDO
C--   step forward ground Water: end
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( land_calc_grT ) THEN
C--   Compute ground temperature from enthalpy (if not already done):

       DO k=1,land_nLev
        DO j=1,sNy
         DO i=1,sNx
C-     Ground Heat capacity, layer k:
          mWater = land_rhoLiqW*land_waterCap
     &            *land_groundW(i,j,k,bi,bj)
          mWater = MAX( mWater, 0. _d 0 )
          grd_HeatCp = land_heatCs + land_CpWater*mWater
C         temperature below freezing:
          temp_bf = (land_enthalp(i,j,k,bi,bj)+land_Lfreez*mWater)
     &                                         / grd_HeatCp
C         temperature above freezing:
          temp_af =  land_enthalp(i,j,k,bi,bj) / grd_HeatCp
#ifdef LAND_OLD_VERSION
          land_enthalp(i,j,k,bi,bj) =
     &          grd_HeatCp*land_groundT(i,j,k,bi,bj)
#else
          land_groundT(i,j,k,bi,bj) =
     &            MIN( temp_bf, MAX(temp_af, 0. _d 0) )
#endif
         ENDDO
        ENDDO
       ENDDO

       IF ( land_impl_grT ) THEN
        DO j=1,sNy
         DO i=1,sNx
          IF ( land_hSnow(i,j,bi,bj).GT.0. _d 0 ) THEN
           land_skinT(i,j,bi,bj) = MIN(land_skinT(i,j,bi,bj), 0. _d 0)
          ELSE
           land_skinT(i,j,bi,bj) = land_groundT(i,j,1,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ELSE
        DO j=1,sNy
         DO i=1,sNx
           land_skinT(i,j,bi,bj) = land_groundT(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDIF

C--   Compute ground temperature: end
      ENDIF

#endif /* ALLOW_LAND */

      RETURN
      END
