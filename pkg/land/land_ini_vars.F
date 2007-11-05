C $Header: /u/gcmpack/MITgcm/pkg/land/land_ini_vars.F,v 1.7 2007/11/05 15:40:53 jmc Exp $
C $Name:  $

#include "LAND_OPTIONS.h"

CBOP
C     !ROUTINE: LAND_INI_VARS
C     !INTERFACE:
      SUBROUTINE LAND_INI_VARS( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R LAND_INI_VARS
C     | o Initialize Land package variables
C     *==========================================================*
C     | for now, used only for a restart
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
C     == Routine Arguments ==
C     myThid -  Number of this instance
      INTEGER myThid
CEOP

#ifdef ALLOW_LAND

C     == Local Variables ==
C     msgBuf      - Informational/error meesage buffer
C     i,j,k,bi,bj :: loop indices
C     grd_HeatCp   :: Heat capacity of the ground (J/m3/K)
C     mWater       :: water content of the ground (kg/m3)
C     temp_af      :: ground temperature if above freezing
C     temp_bf      :: ground temperature if below freezing
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,k,bi,bj
      _RL grd_HeatCp, mWater
      _RL temp_af, temp_bf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Initialize Land package variables

C-    Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO k=1,land_nLev
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx
           land_groundT(i,j,k,bi,bj) = 0. _d 0
           land_enthalp(i,j,k,bi,bj) = 0. _d 0
           land_groundW(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
           land_skinT  (i,j,bi,bj) = 0. _d 0
           land_hSnow  (i,j,bi,bj) = 0. _d 0
           land_snowAge(i,j,bi,bj) = 0. _d 0
           land_runOff (i,j,bi,bj) = 0. _d 0
           land_enRnOf (i,j,bi,bj) = 0. _d 0
           land_HeatFLx(i,j,bi,bj) = 0. _d 0
           land_Pr_m_Ev(i,j,bi,bj) = 0. _d 0
           land_EnWFlux(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C-     end bi,bj loops
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Need to synchronize here before doing master-thread IO
      _BARRIER

      IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0 ) THEN

C--   Define the initial state : read from file
       IF ( land_grT_iniFile .NE. ' ' ) THEN
        CALL READ_REC_3D_RL( land_grT_iniFile, readBinaryPrec,
     &            land_nLev, land_groundT, 1, nIter0, myThid )
       ENDIF
       IF ( land_grW_iniFile .NE. ' ' ) THEN
        CALL READ_REC_3D_RL( land_grW_iniFile, readBinaryPrec,
     &            land_nLev, land_groundW, 1, nIter0, myThid )
       ENDIF
       IF ( land_snow_iniFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( land_snow_iniFile, ' ',
     &                       land_hSnow, nIter0, myThid )
       ENDIF

      ELSEIF ( land_calc_grT .OR. land_calc_grW ) THEN

C--   Read Land package state variables from pickup file
        CALL LAND_READ_PICKUP( nIter0, myThid )

c     ELSE
C-    a trick to allow to start without a land pickup:
C        load grT & grW from AIM surf. BC in S/R aim_land2aim
      ENDIF

C-    Every one else must wait until loading is done.
      _BARRIER

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C-     to have a consistent initial state: set surface Temp & enthalpy
C       assuming all the water in 1 phase only (solid or liquid):
        IF ( ( startTime.EQ.baseTime .AND. nIter0.EQ.0 ) .OR.
     &   .NOT.( land_calc_grT .OR. land_calc_grW ) .OR.
     &       land_oldPickup ) THEN
         DO j=1,sNy
          DO i=1,sNx
c          IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
            DO k=1,land_nLev
             mWater = land_rhoLiqW*land_waterCap
     &               *land_groundW(i,j,k,bi,bj)
             grd_HeatCp = land_heatCs + land_CpWater*mWater
             land_enthalp(i,j,k,bi,bj) =
     &                     grd_HeatCp*land_groundT(i,j,k,bi,bj)
             IF (land_groundT(i,j,k,bi,bj).LT. 0. _d 0)
     &       land_enthalp(i,j,k,bi,bj) = land_enthalp(i,j,k,bi,bj)
     &                                 - land_Lfreez*mWater
            ENDDO
             land_skinT(i,j,bi,bj) = land_groundT(i,j,1,bi,bj)
c          ENDIF
          ENDDO
         ENDDO
        ELSE
         DO j=1,sNy
          DO i=1,sNx
            DO k=1,land_nLev
             mWater = land_rhoLiqW*land_waterCap
     &               *land_groundW(i,j,k,bi,bj)
             grd_HeatCp = land_heatCs + land_CpWater*mWater
C         temperature if below freezing:
             temp_bf = (land_enthalp(i,j,k,bi,bj)+land_Lfreez*mWater)
     &                                            / grd_HeatCp
C         temperature if above freezing:
             temp_af =  land_enthalp(i,j,k,bi,bj) / grd_HeatCp
             land_groundT(i,j,k,bi,bj) =
     &               MIN( temp_bf, MAX(temp_af, 0. _d 0) )
            ENDDO
          ENDDO
         ENDDO
        ENDIF

C-     end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_LAND */

      RETURN
      END
