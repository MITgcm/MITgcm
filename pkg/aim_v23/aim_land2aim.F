C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_land2aim.F,v 1.6 2004/05/21 17:27:22 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_LAND
#include "LAND_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_LAND2AIM
C     !INTERFACE:
      SUBROUTINE AIM_LAND2AIM(
     I               land_frc, land_veg, grnd_alb, snowFld,
     U               aimTld, aimSWA, aimAlb,
     I               myTime, myIter, bi, bj, myThid ) 

C     !DESCRIPTION: \bv
C     *================================================================*
C     | S/R AIM_LAND2AIM
C     | provide surface Boundary Conditions over land (from land pkg)
C     |  to atmospheric physics package AIM
C     *================================================================*
C     | note: by default, use AIM surf.B.C. fields as initial state
C     |   (myIter=0) for the land model 
C     *================================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

C-- MITgcm
#include "EEPARAMS.h"
#include "PARAMS.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "com_forcon.h"

#ifdef ALLOW_LAND
C-- Land package
#include "LAND_SIZE.h"
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#endif

C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     land_veg :: vegetation fraction [0-1]
C     grnd_alb :: ground albedo [0-1]
C     aimTld   :: land surface temp (K), used in AIM
C     snowFld  :: prescribed snow thickness (from AIM input data) [m]
C     aimSWA   :: soil wetness availability [0-1], used in AIM
C     aimAlb   :: land albedo [0-1], used in AIM
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     bi,bj    :: Tile index 
C     myThid   :: Number of this instance of the routine
      _RS  land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  land_veg(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS  grnd_alb(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  snowFld(sNx,sNy)
      _RL  aimTld(sNx,sNy)
      _RL  aimSWA(sNx,sNy)
      _RL  aimAlb(sNx,sNy)
      INTEGER myIter, bi, bj, myThid
      _RL myTime
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AIM
#ifdef ALLOW_LAND

C     == Local variables ==
C     i,j,k        :: Loop counters
C     grd_HeatCp   :: Heat capacity of the ground [J/m3/K]
C     mWater       :: water content of the ground [kg/m3]
C     recip_hSnWet :: reciprol effective snow depth for wetness (m^-1)
C     recip_mSnWet :: reciprol effective snow cover for wetness (mm^-1)
      INTEGER i,j,k
      INTEGER kFillW
      _RL grd_HeatCp, mWater
      _RL RSD
      _RL recip_hSnWet, recip_mSnWet

c     IF (useLand) THEN

       IF ( land_calc_grT ) THEN
C-    Use land-pkg output instead of prescribed ground Temp
        DO j=1,sNy
         DO i=1,sNx
           aimTld(i,j) = land_skinT(i,j,bi,bj)+celsius2K
         ENDDO
        ENDDO
       ELSE
C-    Fill in land-pkg ground Temp. using AIM surf. fields
        DO k=1,land_nLev
         DO j=1,sNy
          DO i=1,sNx
           land_groundT(i,j,k,bi,bj) = aimTld(i,j)-celsius2K
          ENDDO
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx
           land_skinT(i,j,bi,bj) = land_groundT(i,j,1,bi,bj)
         ENDDO
        ENDDO
       ENDIF

       IF (land_calc_grW) THEN
C-    Use land-pkg output instead of prescribed soil moisture
        IF ( land_calc_snow ) THEN
C        Units: hSnow & hSnowWetness are in meter (of snow)
         recip_hSnWet = 0. _d 0
         IF ( hSnowWetness .NE. 0. _d 0 )
     &   recip_hSnWet = 1. _d 0 / hSnowWetness
         DO j=1,sNy
          DO i=1,sNx
           aimSWA(i,j) = land_groundW(i,j,1,bi,bj)
     &                 + land_hSnow(i,j,bi,bj)*recip_hSnWet
           aimSWA(i,j) = MIN( MAX(0. _d 0, aimSWA(i,j)), 1. _d 0 )
          ENDDO
         ENDDO
        ELSE
C        Units: snowFld is in milimeter of equivalent liquid water
         recip_mSnWet = 0. _d 0
         IF ( hSnowWetness .NE. 0. _d 0 )
     &   recip_mSnWet = land_rhoLiqW 
     &                / (land_rhoSnow*hSnowWetness*1000. _d 0)
         DO j=1,sNy
          DO i=1,sNx
           aimSWA(i,j) = land_groundW(i,j,1,bi,bj)
     &                 + snowFld(i,j)*recip_mSnWet
           aimSWA(i,j) = MIN( MAX(0. _d 0, aimSWA(i,j)), 1. _d 0 )
          ENDDO
         ENDDO
        ENDIF
       ELSE
C-    Fill in land-pkg soil moisture using AIM surf. fields
        kFillW = land_nLev
        IF (land_calc_grT) kFillW = 1
        DO k=1,kFillW
         DO j=1,sNy
          DO i=1,sNx
           land_groundW(i,j,k,bi,bj) = aimSWA(i,j)
          ENDDO
         ENDDO
        ENDDO
c       IF ( land_calc_grT ) THEN
C-      needs to recompute enthalpy since grdW has changed:
C       not a good idea: frozen water in the ground will be lost !
        IF ( .FALSE. ) THEN
         DO j=1,sNy
          DO i=1,sNx
c          IF ( land_frc(i,j,bi,bj).GT.0. ) THEN
            DO k=1,land_nLev
             mWater = land_rhoLiqW*land_waterCap
     &               *land_groundW(i,j,k,bi,bj)
             grd_HeatCp = land_heatCs + land_CpWater*mWater

             land_enthalp(i,j,k,bi,bj) =
     &                     grd_HeatCp*land_groundT(i,j,k,bi,bj)
            ENDDO
c          ENDIF
          ENDDO
         ENDDO
        ENDIF

C- end: if land_calc_grW
       ENDIF

       IF (land_calc_snow) THEN

        IF (land_calc_alb) THEN
C-     Compute albedo of snow ; and replace albedo of land.
         CALL LAND_ALBEDO(
     I                land_frc, grnd_alb,
     O                aimAlb,
     I                bi,bj, myTime, myIter, myThid )

        ELSE
C-    Surface Albedo : (from F.M. FORDATE S/R)
         RSD = 1. _d 0/SDALB
         DO j=1,sNy
          DO i=1,sNx
           aimAlb(i,j) = grnd_alb(i,j,bi,bj)
     &       + MAX( 0. _d 0, ALBSN-grnd_alb(i,j,bi,bj) )
     &        *MIN( 1. _d 0, RSD*snowFld(i,j) )
          ENDDO
         ENDDO
        ENDIF

       ENDIF

c     ENDIF

#endif /* ALLOW_LAND */
#endif /* ALLOW_AIM */

      RETURN
      END
