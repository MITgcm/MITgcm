C     $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_dyn2aim.F,v 1.3 2001/08/15 13:50:43 jmc Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_DYN2AIM(
     I           bi,bj, myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_DYN2AIM                                          |
C     | o Map dynamics conforming arrays to AIM internal arrays. |
C     |==========================================================|
C     | Currently this routine transfers grid information to     |
C     | AIM. Other information needs to be included from         |
C     | aim_do_atmos_physics().                                  |
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
#include "Lev_def.h"
#include "com_physvar.h"
#include "com_physcon.h"
#endif /* ALLOW_AIM */

C     == Routine arguments ==
      INTEGER bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J, I2, K

C     Legendre polynomials 
      DO J=1,NLAT
       DO I=1,NLON
        I2=J
        I2=NLON*(J-1)+I
        FMU(I2,1,myThid)=SIN(yC(I,J,bi,bj)*deg2rad)
        FMU(I2,2,myThid)=1.5 _d 0*FMU(I2,1,myThid)**2-0.5 _d 0
       ENDDO
      ENDDO
C     DO J=1,NLAT
C      FMU(J,1,myThid)=SIN(yC(1,J,bi,bj)*deg2rad)
C      FMU(J,2,myThid)=1.5 _d 0*FMU(J,1,myThid)**2-0.5 _d 0
C     ENDDO

C     Depth in levels
      I2=0
      DO J=1,NLAT
        DO I=1,NLON
          I2=I2+1
          NLEVxy(I2,myThid) =0
          NLEVxyU(I2,myThid)=0
          NLEVxyV(I2,myThid)=0
          DO k=1,NLEV
            NLEVxy(I2,myThid) = NLEVxy(I2,myThid)
     &                        + INT( maskC(I,J,K,bi,bj) )
            NLEVxyU(I2,myThid)= NLEVxyU(I2,myThid)
     &                        + INT( maskW(I,J,K,bi,bj) )
            NLEVxyV(I2,myThid)= NLEVxyV(I2,myThid)
     &                        + INT( maskS(I,J,K,bi,bj) )
          ENDDO
        ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
