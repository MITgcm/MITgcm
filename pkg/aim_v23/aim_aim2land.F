C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_aim2land.F,v 1.3 2004/05/21 17:43:04 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"
#ifdef ALLOW_LAND
#include "LAND_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: AIM_AIM2LAND
C     !INTERFACE:
      SUBROUTINE AIM_AIM2LAND(
     I               land_frc, bi, bj, myTime, myIter, myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_AIM2LAND
C     | o Export land surface fluxes to Land package
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"
#include "PARAMS.h"

C-- Physics package
#include "AIM_PARAMS.h"
#include "com_physcon.h"
#include "com_physvar.h"

#ifdef ALLOW_LAND
#include "LAND_SIZE.h" 
#include "LAND_PARAMS.h"
#include "LAND_VARS.h"
#endif

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

#ifdef ALLOW_AIM
#ifdef ALLOW_LAND
C     == Local variables ==
C     i,j,k,I2     :: loop counters
C     conv_precip  :: conversion factor for precip: from g/m2/s to kg/m2/s
      _RL conv_precip
      INTEGER i,j,k,I2

C--   Initialisation :
      IF ( .NOT.land_impl_grT ) THEN
       DO j=1,sNy
        DO i=1,sNx
         land_Pr_m_Ev(i,j,bi,bj) = 0. _d 0
         land_EnWFlux(i,j,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

C--   Atmospheric Physics Fluxes

c     IF ( useLand ) THEN

C      from g/m2/s to kg/m2/s :
       conv_Precip = 1. _d -3

      IF ( land_calc_grT .AND. .NOT.land_impl_grT ) THEN
C--   Surface heat flux to compute ground temperature explicitely:
       k = 0
       IF (aim_splitSIOsFx) k = 1 
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx

C-    total surface downward heat flux :
         land_HeatFlx(i,j,bi,bj) = 
     &                         SSR(I2,k,myThid)
     &                       - SLR(I2,k,myThid)
     &                       - SHF(I2,1,myThid)
     &                       - EVAP(I2,1,myThid)*ALHC
        ENDDO
       ENDDO

       IF ( land_calc_snow ) THEN
C-     Evap of snow: substract Latent Heat of freezing from heatFlux
        DO j=1,sNy
         DO i=1,sNx
          I2 = i+(j-1)*sNx
          IF ( land_skinT(i,j,bi,bj).LT. 0. _d 0 .OR.
     &         land_hSnow(i,j,bi,bj).GT. 0. _d 0 ) THEN
           land_HeatFlx(i,j,bi,bj) = land_HeatFlx(i,j,bi,bj)
     &                       - EVAP(I2,1,myThid)*ALHF
           land_EnWFlux(i,j,bi,bj) = 
     &                         EVAP(I2,1,myThid)*ALHF
          ENDIF
         ENDDO
        ENDDO
       ENDIF

C--   to compute ground temperature explicitely: end
      ENDIF
 
C--   Fresh water fluxes
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx

C-    total Precip minus Evap surface flux :
C        convert from g.m-2.s-1 to kg/m2/s
         land_Pr_m_Ev(i,j,bi,bj) = land_Pr_m_Ev(i,j,bi,bj)
     &       + conv_precip*(   PRECNV(I2,myThid)
     &                       + PRECLS(I2,myThid)
     &                       - EVAP(I2,1,myThid)
     &                     )

        ENDDO
       ENDDO

      IF ( aim_energPrecip ) THEN
C-     Compute energy flux related to Precip. (snow, T_rain)
C      Evap of snow: add Latent Heat of freezing
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
         land_EnWFlux(i,j,bi,bj) = land_EnWFlux(i,j,bi,bj)
     &       + EnPrec(I2,myThid)*( PRECNV(I2,myThid)
     &                            +PRECLS(I2,myThid) )
        ENDDO
       ENDDO
      ENDIF

C- end (if useLand)
c     ENDIF

#endif /* ALLOW_LAND */
#endif /* ALLOW_AIM */

      RETURN
      END
