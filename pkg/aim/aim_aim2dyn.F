C     $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_aim2dyn.F,v 1.1 2001/05/29 19:28:53 cnh Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_AIM2DYN(
     I           bi,bj, myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_AIM2DYN                                          |
C     | o Remap AIM outputs to dynamics conforming arrays.       |
C     |==========================================================|
C     | Currently AIM exports to the dynmaics                    |
C     |  - PBL drag coefficient                                  |
C     |  - Net tendency for temperature                          |
C     |  - Net tendency for water vapor                          |
C     | Exporting drag has the nice property that it is a scalar.|
C     | This means that the exchanges on the AIM exported fields |
C     | do not need special piaring on the cube. It may not be   |
C     | a good idea in the long term as it makes assumptions     |
C     | about the momentum schemes within AIM.                   |
C     \==========================================================/
      IMPLICIT rEAL*8 (A-H,O-Z) 

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#ifdef ALLOW_AIM
#include "atparam0.h"
#include "atparam1.h"
      INTEGER NGP
      INTEGER NLON
      INTEGER NLAT
      INTEGER NLEV
      PARAMETER ( NLON=IX, NLAT=IL, NLEV=KX, NGP=NLON*NLAT )
#include "com_physvar.h"
#include "AIM2DYN.h"
#endif /* ALLOW_AIM */

C     == Routine arguments ==
      INTEGER bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J
      INTEGER I2, I3, kAtm, kLev

C--   Forcing term
      _RL pGround,CPAIR,RD

      pGround = 1. _d 5
      RD      = 287. _d 0
      CPAIR   = 1004. _d 0
      DO kLev=1,Nr
       DO J=1,sNy
        DO I=1,sNx
         I2 = sNx*(J-1)+I
         Katm = _KD2KA( Klev )
C        Planetary boundary layer drag coeff.
         aim_drag(i,j,bi,bj) = DRAG(I2,myThid)

C        Net temperature tendency
         aim_dTdt(i,j,kLev,bi,bj) = ( TT_PBL(I2,katm,myThid)
     &                               +TT_CNV(I2,katm,myThid)
     &                               +TT_LSC(I2,katm,myThid)
     &                               +TT_RSW(I2,katm,myThid)
     &                               +TT_RLW(I2,katm,myThid)
     &        )*((pGround/rC(kLev))**(RD/CPAIR))

C        Net water vapor tendency
         aim_dSdt(i,j,kLev,bi,bj) = QT_PBL(I2,katm,myThid)
     &                             +QT_CNV(I2,katm,myThid)
     &                             +QT_LSC(I2,katm,myThid)

        ENDDO
       ENDDO
      ENDDO

C     DO kLev=1,Nr
C      DO J=1-OLy,sNy+OLy
C       DO I=1-OLx,sNx+OLx
C        aim_drag(i,j,bi,bj) = 0.000000
C       ENDDO
C      ENDDO
C     ENDDO

C     This can't stay here!!
C     _EXCH_XY_R8( aim_drag, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
