C $Header: /u/gcmpack/MITgcm/pkg/land/land_stepfwd.F,v 1.1 2003/06/12 17:54:22 jmc Exp $
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

c #include "PARAMS.h"
c #include "GRID.h"
c #include "DYNVARS.h"

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
C     grd_HeatCp   :: Heat capacity of the ground
C     fieldCapac   :: field capacity (of water) [m]
C     ground_dTdt  :: ground temperature tendency
C     ground_dWdt  :: soil moisture tendency
C     flxkup       :: downward flux, upper interface (k-1,k)
C     flxdwn       :: downward flux, lower interface (k,k+1)
C     fractRunOff  :: fraction of water in excess which leaves as runoff
C     grdWexcess   :: ground water in excess [m/s]
      _RL grd_HeatCp, fieldCapac, ground_dTdt, ground_dWdt
      _RL fractRunOff, grdWexcess, groundWnp1
      _RL flxkup(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxkdw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j,k,kp1

      IF (land_calc_grT) THEN
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
 
C-     Ground Heat capacity, layer k:
          grd_HeatCp = land_heatCs
     &               + land_heatCw*land_groundW(i,j,k,bi,bj)
     &                            *land_waterCap

C-     Net temperature tendency
          ground_dTdt = (flxkup(i,j)-flxkdw(i,j))
     &                / (grd_HeatCp*land_dzF(k))
         
C-     Step forward ground temperature, level k :
          land_groundT(i,j,k,bi,bj) = land_groundT(i,j,k,bi,bj)
     &                              + land_deltaT*ground_dTdt

         ENDIF
        ENDDO
       ENDDO

      ENDDO
C--   step forward ground temperature: end
      ENDIF

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
           flxkup(i,j) = land_Pr_m_Ev(i,j,bi,bj)
           land_runOff(i,j,bi,bj) = 0. _d 0
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
C-     Diffusion flux of soil moisture, lower interface (k,k+1):
          flxkdw(i,j) = fieldCapac*
     &                ( land_groundW(i,j,k,bi,bj)
     &                 -land_groundW(i,j,kp1,bi,bj) )
     &                / land_wTauDiff
 
C-     Net soil moisture tendency
          ground_dWdt = (flxkup(i,j)-flxkdw(i,j)) / fieldCapac
         
C-     Step forward soil moisture, level k :
          groundWnp1 = land_groundW(i,j,k,bi,bj)
     &               + land_deltaT*ground_dWdt
          land_groundW(i,j,k,bi,bj) = MIN(1. _d 0, groundWnp1)

C-     Run off: fraction 1-fractRunOff enters level below
          grdWexcess = ( groundWnp1 - MIN(1. _d 0, groundWnp1) )
     &                *fieldCapac/land_deltaT
          flxkdw(i,j) = flxkdw(i,j)
     &                + (1. _d 0-fractRunOff)*grdWexcess
          land_runOff(i,j,bi,bj) = land_runOff(i,j,bi,bj)
     &                           + fractRunOff*grdWexcess
         ENDIF
        ENDDO
       ENDDO

      ENDDO
C--   step forward ground Water: end
      ENDIF

#endif /* ALLOW_LAND */

      RETURN
      END
