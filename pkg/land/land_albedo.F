C $Header: /u/gcmpack/MITgcm/pkg/land/land_albedo.F,v 1.2 2007/01/31 23:02:06 dfer Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_DIAGNOSTICS
C     !INTERFACE:
      SUBROUTINE LAND_ALBEDO( 
     I                land_frc, grnd_alb,
     O                alb_land,
     I                bi,bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_ALBEDO
C     | o Calculate snow albedo over land
C     *==========================================================*
C     \ev
 
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Land package :
#include "LAND_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     grnd_alb :: ground albedo [0-1]
C     alb_land :: albedo of land (including snow effect) [0-1]
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS grnd_alb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL alb_land(sNx,sNy)
      INTEGER bi, bj, myIter, myThid
      _RL     myTime
CEOP

#ifdef ALLOW_LAND

C     == Local Variables == 
C     i,j      :: Loop counters
C     albSnow  :: albedo of snow
C     ageSnow  :: age of snow [days]
      INTEGER i,j
      _RL albSnow, ageSnow, hSnow, Tsf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-     Compute albedo of snow and then albedo of land.

        DO j=1,sNy
         DO i=1,sNx
          alb_land(i,j) = grnd_alb(i,j,bi,bj)
          IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
           Tsf = land_skinT(i,j,bi,bj)
C       snow age  (units = days):
           ageSnow = land_snowAge(i,j,bi,bj)/86400. _d 0
           hSnow = land_hSnow(i,j,bi,bj)

C--------------------------------------------
C--- taken from thsice/thsice_albedo.F :
C-    New snow: (linear) transition between tempSnowAlbL (oC) and 0.oC
C      from cold/dry snow albedo to warm/wet snow albedo
           IF ( tempSnowAlbL.LT.0. _d 0 ) THEN
               albSnow = albColdSnow
     &         + (albWarmSnow - albColdSnow)
     &         *MAX( 0. _d 0, MIN(1. _d 0 - Tsf/tempSnowAlbL, 1. _d 0) )
           ELSE
               albSnow = albColdSnow
           ENDIF
C-    albedo of snow is function of snow-age
           albSnow = albOldSnow
     &       +(albSnow-albOldSnow)*exp(-0.2 _d 0*ageSnow)

C-    layer of snow over the ground:
           alb_land(i,j) = albSnow 
     &       +(alb_land(i,j)-albSnow)*exp(-hSnow/hAlbSnow)
C--------------------------------------------
          ENDIF
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_LAND */

      RETURN
      END
