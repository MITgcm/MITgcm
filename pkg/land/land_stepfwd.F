C $Header: /u/gcmpack/MITgcm/pkg/land/land_stepfwd.F,v 1.2 2004/03/11 14:42:00 jmc Exp $
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
C     fieldCapac   :: field capacity (of water) [m]
C     mWater       :: water content of the ground [kg/m3]
C     fractRunOff  :: fraction of water in excess which leaves as runoff
C     grdWexcess   :: ground water in excess [m/s]
C     groundWnp1   :: hold temporary future soil moisture
C     enthalpGrdW  :: enthalpy of ground water [J/m3]
C     flxkup       :: downward flux of water, upper interface (k-1,k)
C     flxdwn       :: downward flux of water, lower interface (k,k+1)
C     flxEng       :: downward energy flux associated with water flux
C     temp_af      :: ground temperature if above freezing
C     temp_bf      :: ground temperature if below freezing
C     mPmE         :: hold temporary (liquid) Precip minus Evap [kg/m2/s]
C     enWfx        :: hold temporary energy flux of Precip [W/m2]
C     enGr1        :: ground enthalpy of level 1  [J/m2]
C     mSnow        :: mass of snow         [kg/m2]
C     dMsn         :: mass of melting snow [kg/m2]
C     snowPrec     :: snow precipitation [kg/m2/s]
C     hNewSnow     :: fresh snow accumulation [m]
C     ageFac       :: snow aging factor [1]
      _RL grd_HeatCp, fieldCapac, mWater
      _RL fractRunOff, grdWexcess, groundWnp1, enthalpGrdW
      _RL flxkup(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxkdw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxEng(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL temp_af, temp_bf, mPmE, enWfx, enGr1
      _RL mSnow, dMsn, snowPrec, hNewSnow, ageFac
      INTEGER i,j,k,kp1

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

#ifdef LAND_OLD_VERSION
      IF ( .TRUE. ) THEN
#else
      IF ( land_calc_snow ) THEN
#endif
C--   need (later on) ground temp. to be consistent with updated enthalpy:
        DO k=1,land_nLev
         DO j=1,sNy
          DO i=1,sNx
           IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
            mWater = land_rhoLiqW*land_waterCap
     &              *land_groundW(i,j,k,bi,bj)
            grd_HeatCp = land_heatCs + land_CpWater*mWater
            temp_bf = (land_enthalp(i,j,k,bi,bj)+land_Lfreez*mWater)
     &                                           / grd_HeatCp
            temp_af =  land_enthalp(i,j,k,bi,bj) / grd_HeatCp
            land_groundT(i,j,k,bi,bj) = 
     &              MIN( temp_bf, MAX(temp_af, 0. _d 0) )
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
C-    snow aging:
           land_snowAge(i,j,bi,bj) = 
     &         ( land_deltaT + land_snowAge(i,j,bi,bj)*ageFac )
           IF ( enWfx.LT.0. ) THEN
C-    snow precip in excess (Snow > Evap) :
C     => start to melt (until ground at freezing point) and then accumulate 
            snowPrec = -enWfx -MAX( enGr1/land_deltaT, 0. _d 0 )
            snowPrec = MAX( snowPrec*recip_Lfreez , 0. _d 0 )
            mPmE = mPmE - snowPrec
            flxEng(i,j) = enWfx + land_Lfreez*snowPrec
            hNewSnow = land_deltaT * snowPrec / land_rhoSnow
            land_hSnow(i,j,bi,bj) = land_hSnow(i,j,bi,bj) + hNewSnow
C-    refresh snow age:
            land_snowAge(i,j,bi,bj) = land_snowAge(i,j,bi,bj)
     &                          *EXP( -hNewSnow/hNewSnowAge )
           ELSE
C-    rain precip (whatever Evap is) or Evap exceeds snow precip :
C     => snow melts or sublimates
c           snowMelt = MIN( enWfx*recip_Lfreez ,
c    &                 land_hSnow(i,j,bi,bj)*land_rhoSnow/land_deltaT )
            mSnow = land_hSnow(i,j,bi,bj)*land_rhoSnow
            dMsn = enWfx*recip_Lfreez*land_deltaT
            IF ( dMsn .GE. mSnow ) THEN
              dMsn = mSnow
              land_hSnow(i,j,bi,bj) = 0. _d 0
              flxEng(i,j) = enWfx - land_Lfreez*mSnow/land_deltaT
            ELSE
              flxEng(i,j) = 0. _d 0
              land_hSnow(i,j,bi,bj) = land_hSnow(i,j,bi,bj)
     &                              - dMsn / land_rhoSnow 
            ENDIF
c           IF (mPmE.GT.0.) land_snowAge(i,j,bi,bj) = timeSnowAge
            mPmE = mPmE + dMsn/land_deltaT
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
           flxEng(i,j) = 0. _d 0
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

       IF (k.EQ.1) THEN
        DO j=1,sNy
         DO i=1,sNx
           land_runOff(i,j,bi,bj) = 0. _d 0
           land_enRnOf(i,j,bi,bj) = 0. _d 0
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
C-     Diffusion flux of water, lower interface (k,k+1):
          flxkdw(i,j) = fieldCapac*
     &                ( land_groundW(i,j,k,bi,bj)
     &                 -land_groundW(i,j,kp1,bi,bj) )
     &                / land_wTauDiff
 
C-     Step forward soil moisture, level k :
          groundWnp1 = land_groundW(i,j,k,bi,bj)
     &       + land_deltaT * (flxkup(i,j)-flxkdw(i,j)) / fieldCapac
          land_groundW(i,j,k,bi,bj) = MIN(1. _d 0, groundWnp1)

C-     Run off: fraction 1-fractRunOff enters level below
          grdWexcess = ( groundWnp1 - MIN(1. _d 0, groundWnp1) )
     &                *fieldCapac/land_deltaT
          land_runOff(i,j,bi,bj) = land_runOff(i,j,bi,bj)
     &                           + fractRunOff*grdWexcess

          IF ( land_calc_snow ) THEN
C-     account for water fluxes in energy budget:
           enthalpGrdW = land_enthalp(i,j,k,bi,bj) 
     &                 - land_heatCs*land_groundT(i,j,k,bi,bj)
           land_enRnOf(i,j,bi,bj) = land_enRnOf(i,j,bi,bj)
     &                            + fractRunOff*grdWexcess*enthalpGrdW
           land_enthalp(i,j,k,bi,bj) = land_enthalp(i,j,k,bi,bj)
     &          + ( flxEng(i,j) - (flxkdw(i,j)+grdWexcess)*enthalpGrdW
     &            )*land_deltaT/land_dzF(k)
          ELSE
           enthalpGrdW = 0. _d 0
          ENDIF
C-     prepare fluxes for next level:
          flxkdw(i,j) = flxkdw(i,j)
     &                + (1. _d 0-fractRunOff)*grdWexcess
          flxEng(i,j) = flxkdw(i,j)*enthalpGrdW

         ENDIF
        ENDDO
       ENDDO

      ENDDO
C--   step forward ground Water: end
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (land_calc_grT ) THEN
C--   Compute ground temperature from enthalpy :

       DO k=1,land_nLev
        DO j=1,sNy
         DO i=1,sNx
C-     Ground Heat capacity, layer k:
          mWater = land_rhoLiqW*land_waterCap
     &            *land_groundW(i,j,k,bi,bj)
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
