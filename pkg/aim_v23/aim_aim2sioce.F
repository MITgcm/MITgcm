C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_aim2sioce.F,v 1.9 2013/05/02 20:10:14 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_THSICE
#include "THSICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_AIM2SIOCE
C     !INTERFACE:
      SUBROUTINE AIM_AIM2SIOCE(
     I               land_frc, siceFrac,
     O               prcAtm, snowPrc,
     I               bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_AIM2SIOCE
C     | o Interface between AIM and thSIce pkg or (coupled) ocean
C     *==========================================================*
C     | o compute surface fluxes over ocean (ice-free + ice covered)
C     |   for diagnostics, thsice package and (slab, coupled) ocean
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h"

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "com_physcon.h"
#include "com_physvar.h"

#ifdef ALLOW_THSICE
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#endif

C updated fields (in commom blocks):
C  if using thSIce:
C      Qsw(inp)   :: SW radiation through the sea-ice down to the ocean (+=up)
C      Qsw(out)   :: SW radiation down to the ocean (ice-free + ice-covered)(+=up)
C      Qnet(out)  :: Net heat flux out of the ocean (ice-free ocean only)(+=up)
C             and the Ice-Covered contribution will be added in S/R THSICE_STEP_FWD
C      EmPmR(out) :: Net fresh water flux out off the ocean (ice-free ocean only)
C             and the Ice-Covered contribution will be added in S/R THSICE_STEP_FWD
C      sHeating(in/out) :: air - seaice surface heat flux left to melt the ice
C      icFrwAtm   :: Evaporation over sea-ice [kg/m2/s] (>0 if evaporate)
C      icFlxSW    :: net SW heat flux through the ice to the ocean [W/m2] (+=dw)
C  if not using thSIce:
C      Qsw(out)   :: SW radiation down to the ocean (ice-free + ice-covered)(+=up)
C      Qnet(out)  :: Net heat flux out of the ocean (ice-free + ice-covered)(+=up)
C      EmPmR(out) :: Net fresh water flux out off the ocean (ice-free + ice-covered)

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     siceFrac :: sea-ice fraction (relative to full grid-cell) [0-1]
C     prcAtm   :: total precip from the atmosphere [kg/m2/s]
C     snowPrc  :: snow precip over sea-ice [kg/m2/s]
C     bi,bj    :: Tile indices
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: My Thread Id number
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL siceFrac(sNx,sNy)
      _RL prcAtm (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL snowPrc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j,I2      :: loop counters
C     convPrcEvp  :: units conversion factor for Precip & Evap:
C                 :: from AIM units (g/m2/s) to model EmPmR units ( kg/m2/s )
      _RL convPrcEvp
      _RL icFrac, opFrac
      INTEGER i,j,I2

C--   Initialisation :

C--   Atmospheric Physics Fluxes

C     from g/m2/s to kg/m2/s :
      convPrcEvp = 1. _d -3

      DO j=1,sNy
       DO i=1,sNx
        IF ( land_frc(i,j,bi,bj).GE.1. _d 0 ) THEN
C-    Full Land grid-cell: set all fluxes to zero (this has no effect on the
C        model integration and just put this to get meaningfull diagnostics)
         prcAtm(i,j)     = 0. _d 0
         Qnet(i,j,bi,bj) = 0. _d 0
         EmPmR(i,j,bi,bj)= 0. _d 0
         Qsw(i,j,bi,bj)  = 0. _d 0
        ELSE
         I2 = i+(j-1)*sNx

C-    Total Precip (no distinction between ice-covered / ice-free fraction):
         prcAtm(i,j) = ( PRECNV(I2,myThid)
     &                 + PRECLS(I2,myThid) )

C-    Net surface heat flux over ice-free ocean (+=down)
C     note: with aim_splitSIOsFx=F, ice-free & ice covered contribution are
C     already merged together and Qnet is the mean heat flux over the grid box.
         Qnet(i,j,bi,bj) =
     &                         SSR(I2,2,myThid)
     &                       - SLR(I2,2,myThid)
     &                       - SHF(I2,2,myThid)
     &                       - EVAP(I2,2,myThid)*ALHC

C-    E-P over ice-free ocean [m/s]: (same as above is aim_splitSIOsFx=F)
         EmPmR(i,j,bi,bj) = ( EVAP(I2,2,myThid)
     &                      - prcAtm(i,j) ) * convPrcEvp

C-    Net short wave (ice-free ocean) into the ocean (+=down)
         Qsw(i,j,bi,bj) = SSR(I2,2,myThid)

        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_THSICE
      IF ( useThSIce ) THEN
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
C-    Mixed-Layer Ocean: (for thsice slab_ocean and coupler)
C     NOTE: masking is now applied much earlier, during initialisation
c        IF (land_frc(i,j,bi,bj).EQ.1. _d 0) hOceMxL(i,j,bi,bj) = 0.

C-    Evaporation over sea-ice: (for thsice)
         icFrwAtm(i,j,bi,bj) = EVAP(I2,3,myThid)*convPrcEvp

C-    short-wave downward heat flux (ice-free ocean + ice-covered):
C     note: at this point we already called THSICE_IMPL_TEMP to solve for
C     seaice temp and SW flux through the ice. SW is not modified after, and
C     can therefore combine the open-ocean & ice-covered ocean SW fluxes.
         icFrac = iceMask(i,j,bi,bj)
         opFrac = 1. _d 0 - icFrac
         Qsw(i,j,bi,bj) = icFrac*icFlxSW(i,j,bi,bj)
     &                  + opFrac*Qsw(i,j,bi,bj)

        ENDDO
       ENDDO

       IF ( aim_energPrecip ) THEN
C--   Add energy flux related to Precip. (snow, T_rain) over sea-ice
         DO j=1,sNy
          DO i=1,sNx
           IF ( iceMask(i,j,bi,bj).GT.0. _d 0 ) THEN
            I2 = i+(j-1)*sNx
            IF ( EnPrec(I2,myThid).GE.0. _d 0 ) THEN
C-    positive => add to surface heating
              sHeating(i,j,bi,bj) = sHeating(i,j,bi,bj)
     &                            + EnPrec(I2,myThid)*prcAtm(i,j)
              snowPrc(i,j) = 0. _d 0
            ELSE
C-    negative => make snow
              snowPrc(i,j) = prcAtm(i,j)*convPrcEvp
            ENDIF
           ELSE
              snowPrc(i,j) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
       ENDIF

      ELSEIF ( aim_splitSIOsFx ) THEN
#else /* ALLOW_THSICE */
      IF ( aim_splitSIOsFx ) THEN
#endif /* ALLOW_THSICE */
C-    aim_splitSIOsFx=T: fluxes over sea-ice (3) & ice-free ocean (2) were
C     computed separately and here we merge the 2 fractions
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
         IF ( siceFrac(i,j) .GT. 0. ) THEN
          icFrac = siceFrac(i,j)/(1. _d 0 - land_frc(i,j,bi,bj))
          opFrac = 1. _d 0 - icFrac

C-    Net surface heat flux over sea-ice + ice-free ocean (+=down)
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)*opFrac
     &                    + (  SSR(I2,3,myThid)
     &                       - SLR(I2,3,myThid)
     &                       - SHF(I2,3,myThid)
     &                       - EVAP(I2,3,myThid)*ALHC
     &                      )*icFrac
C-    E-P over sea-ice + ice-free ocean [m/s]:
          EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*opFrac
     &                     + ( EVAP(I2,3,myThid)
     &                       - prcAtm(i,j) ) * convPrcEvp * icFrac

C-    Net short wave (ice-free ocean) into the ocean (+=down)
          Qsw(i,j,bi,bj) = opFrac*Qsw(i,j,bi,bj)
     &                   + icFrac*SSR(I2,3,myThid)

         ENDIF
        ENDDO
       ENDDO

C--   end of If useThSIce / elseif aim_splitSIOsFx blocks
      ENDIF

      IF ( aim_energPrecip ) THEN
C--   Ice free fraction: Add energy flux related to Precip. (snow, T_rain):
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &                    + EnPrec(I2,myThid)*prcAtm(i,j)
         ENDDO
        ENDDO
      ENDIF

      DO j=1,sNy
        DO i=1,sNx
C-    Total Precip : convert units
          prcAtm(i,j) = prcAtm(i,j) * convPrcEvp
C-    Oceanic convention: Heat flux are > 0 upward ; reverse sign.
          Qsw(i,j,bi,bj) = -Qsw(i,j,bi,bj)
          Qnet(i,j,bi,bj)= -Qnet(i,j,bi,bj)
        ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
