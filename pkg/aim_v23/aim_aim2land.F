C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_aim2land.F,v 1.1 2003/06/12 17:59:05 jmc Exp $
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
C     i,j,k        :: loop counters
C     I2,Katm      :: loop counters
C     conv_precip  :: conversion factor for precip: from g.m-2.s-1 to m/s
      _RL conv_precip
      INTEGER i,j,k
      INTEGER I2

C--   Physics tendency term

c     IF ( useLand ) THEN

C      from g/(m^2.s) to m/s :
       conv_Precip = 1. _d -3 / rhoConstFresh

       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx

C-    total surface downward heat flux :
         land_HeatFLx(i,j,bi,bj) = 
     &                         SSR(I2,myThid)
     &                       - SLR(I2,myThid)
     &                       - SHF(I2,1,myThid)
     &                       - EVAP(I2,1,myThid)*ALHC
 
C-    total Precip minus Evap surface flux :
C        convert from g.m-2.s-1 to m/s 
         land_Pr_m_Ev(i,j,bi,bj) =
     &         conv_precip*(   PRECNV(I2,myThid)
     &                       + PRECLS(I2,myThid)
     &                       - EVAP(I2,1,myThid)
     &                     )

        ENDDO
       ENDDO

C- end (if useLand)
c     ENDIF

#endif /* ALLOW_LAND */
#endif /* ALLOW_AIM */

      RETURN
      END
