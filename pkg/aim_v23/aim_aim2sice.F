C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/Attic/aim_aim2sice.F,v 1.1 2004/04/08 00:14:09 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_THSICE
#include "THSICE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_AIM2SICE
C     !INTERFACE:
      SUBROUTINE AIM_AIM2SICE(
     I               land_frc, 
     O               prcAtm, evpAtm, flxSW,
     I               bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_AIM2SICE
C     | o Export sea-ice surface fluxes to thSIce package
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

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     land_frc :: land fraction [0-1]
C     prcAtm   :: total precip from the atmosphere [kg/m2/s]
C     evpAtm   :: evaporation to the atmosphere [kg/m2/s] (>0 if evaporate)
C     flxSW    :: net heat flux from the ice to the ocean
C     bi,bj    :: Tile index
C     myTime   :: Current time of simulation ( s )
C     myIter   :: Current iteration number in simulation
C     myThid   :: Number of this instance of the routine
      _RS land_frc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL prcAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL evpAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxSW (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEOP

#ifdef ALLOW_AIM
#ifdef ALLOW_THSICE
C     == Local variables ==
C     i,j,I2       :: loop counters
C     conv_precip  :: conversion factor for precip: from g/m2/s to kg/m2/s
      _RL conv_precip
      _RL icFrac, opFrac
      INTEGER i,j,I2

C--   Initialisation :

C--   Atmospheric Physics Fluxes

c     IF ( useLand ) THEN

C      from g/m2/s to kg/m2/s :
       conv_Precip = 1. _d -3
C      from g/m2/s to m/s :
c      conv_Precip = 1. _d -3 / rhofw 

      DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
C-    Mixed-Layer Ocean:
         IF (land_frc(i,j,bi,bj).EQ.1. _d 0) hOceMxL(i,j,bi,bj) = 0.

C-    Total Precip :
         prcAtm(i,j) = ( PRECNV(I2,myThid)
     &                 + PRECLS(I2,myThid) )

C-    Net surface heat flux over ice-free ocean (+=down)
         Qnet(i,j,bi,bj) = 
     &                         SSR(I2,2,myThid)
     &                       - SLR(I2,2,myThid)
     &                       - SHF(I2,2,myThid)
     &                       - EVAP(I2,2,myThid)*ALHC

C-    E-P over ice-free ocean [m/s]:
         EmPmR(i,j,bi,bj) = ( EVAP(I2,2,myThid)
     &                      - prcAtm(i,j) ) * conv_precip/rhofw

C-    Net short wave (ice-free ocean) into the ocean (+=down)
         flxSW(i,j) = Qsw(i,j,bi,bj)
         Qsw(i,j,bi,bj) = SSR(I2,2,myThid)

        ENDDO
      ENDDO

      IF ( aim_energPrecip ) THEN
C--   Add energy flux related to Precip. (snow, T_rain) over ice-free ocean
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj)
     &                    + EnPrec(I2,myThid)*prcAtm(i,j)
         ENDDO
        ENDDO
      ENDIF

      IF ( useThSIce ) THEN
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx

C-    Evaporation over sea-ice:
         evpAtm(i,j) = EVAP(I2,3,myThid)*conv_precip

C-    short-wave downward heat flux (open ocean + ice-covered):
         icFrac = iceMask(i,j,bi,bj)
         opFrac = 1. _d 0 - icFrac
         Qsw(i,j,bi,bj) = icFrac*flxSW(i,j) + opFrac*Qsw(i,j,bi,bj)

        ENDDO
       ENDDO
      ENDIF

      IF ( useThSIce .AND. aim_energPrecip ) THEN
C--   Add energy flux related to Precip. (snow, T_rain) over sea-ice
       DO j=1,sNy
        DO i=1,sNx
         IF ( iceMask(i,j,bi,bj).GT.0. _d 0 ) THEN
          I2 = i+(j-1)*sNx
          IF ( EnPrec(I2,myThid).GE.0. _d 0 ) THEN
C-    positive => add to surface heating
            sHeating(i,j,bi,bj) = sHeating(i,j,bi,bj)
     &                          + EnPrec(I2,myThid)*prcAtm(i,j)
          ELSE
C-    negative => make snow
            snowPrc(i,j,bi,bj) = prcAtm(i,j)*conv_precip
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDIF

      DO j=1,sNy
        DO i=1,sNx
C-    Total Precip : convert units
          prcAtm(i,j) = prcAtm(i,j) * conv_precip
C-    Oceanic convention: Heat flux are > 0 upward ; reverse sign.
          Qsw(i,j,bi,bj) = -Qsw(i,j,bi,bj)
          Qnet(i,j,bi,bj)= -Qnet(i,j,bi,bj)
        ENDDO
      ENDDO

#endif /* ALLOW_THSICE */
#endif /* ALLOW_AIM */

      RETURN
      END
