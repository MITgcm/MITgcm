C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_aim2dyn.F,v 1.2 2003/10/31 20:35:32 edhill Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_AIM2DYN(
     I           bi, bj, myTime, myIter, myThid)
C     *==========================================================*
C     | S/R AIM_AIM2DYN                                          |
C     | o Remap AIM outputs to dynamics conforming arrays.       |
C     |==========================================================*
C     | Currently AIM exports to the dynmaics                    |
C     |  - PBL drag coefficient                                  |
C     |  - Net tendency for temperature                          |
C     |  - Net tendency for water vapor                          |
C     | Exporting drag has the nice property that it is a scalar.|
C     | This means that the exchanges on the AIM exported fields |
C     | do not need special piaring on the cube. It may not be   |
C     | a good idea in the long term as it makes assumptions     |
C     | about the momentum schemes within AIM.                   |
C     *==========================================================*
C-------
C  Note: Except LSC tendency, all others need to be /dpFac.
C-------
      IMPLICIT NONE

C     == Global data ==
C-- size for MITgcm & Physics package :
#include "AIM_SIZE.h" 

#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"
#include "com_physvar.h"

C     == Routine arguments ==
C     bi,bj  - Tile index
C     myTime - Current time of simulation ( s )
C     myIter - Current iteration number in simulation
C     myThid - Number of this instance of the routine
      INTEGER bi, bj, myIter, myThid
      _RL myTime
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j,k        :: loop counters
C     I2,Katm      :: loop counters
C     conv_T2theta :: conversion factor from (absolute) Temp. to Pot.Temp.
      _RL conv_T2theta
      INTEGER i,j,k
      INTEGER I2, Katm

C--   Physics tendency term

      DO k=1,Nr
       conv_T2theta = (atm_po/rC(k))**atm_kappa
       DO j=1,sNy
        DO i=1,sNx
         I2 = i+(j-1)*sNx
         Katm = _KD2KA( k )
C        Planetary boundary layer drag coeff.
         aim_drag(i,j,bi,bj) = DRAG(I2,3,myThid)

C        Net temperature tendency
         aim_dTdt(i,j,k,bi,bj) =   TT_LSC(I2,Katm,myThid)
     &                         + ( TT_CNV(I2,Katm,myThid)
     &                            +TT_PBL(I2,Katm,myThid)
     &                            +TT_RSW(I2,Katm,myThid)
     &                            +TT_RLW(I2,Katm,myThid)
     &                           )*recip_hFacC(i,j,k,bi,bj)
         aim_dTdt(i,j,k,bi,bj) = aim_dTdt(i,j,k,bi,bj)*conv_T2theta

C        Net water vapor tendency
         aim_dSdt(i,j,k,bi,bj) =   QT_LSC(I2,Katm,myThid)
     &                         + ( QT_CNV(I2,Katm,myThid)
     &                            +QT_PBL(I2,Katm,myThid)
     &                           )*recip_hFacC(i,j,k,bi,bj)

        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
