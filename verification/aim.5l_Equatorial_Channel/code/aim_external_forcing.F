C     $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/aim_external_forcing.F,v 1.1 2001/02/08 04:20:39 cnh Exp $
C     $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_EXTERNAL_FORCING_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_EXTERNAL_FORCING_U                               |
C     | o Add AIM tendency terms to U tendency.                  |
C     \==========================================================/

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#ifdef ALLOW_AIM
#include "AIM_DDT.h"
#endif /* ALLOW_AIM */

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J

      DO J=1,sNy
       DO I=1,sNx
        IF ( hFacW(i,j,kLev,bi,bj) .NE. 0. ) THEN
         gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj) +
     &   0.5*( AIM_UT(I,J,kLev,bi,bj)+AIM_UT(I-1,J,kLev,bi,bj) )
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE AIM_EXTERNAL_FORCING_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_V                                   |
C     | o Add AIM tendency to meridional velocity.               |
C     \==========================================================/
      IMPLICIT rEAL*8 (A-H,O-Z)

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

#ifdef ALLOW_AIM
#include "AIM_DDT.h"
#endif /* ALLOW_AIM */

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J

      DO J=1,sNy
       DO I=1,sNx
        IF ( hFacS(i,j,kLev,bi,bj) .NE. 0. ) THEN
         gV(i,j,kLev,bi,bj) =
     &   gV(i,j,kLev,bi,bj) + 
     &   0.5*( AIM_VT(I,J,kLev,bi,bj) + AIM_VT(I,J-1,kLev,bi,bj) )
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE AIM_EXTERNAL_FORCING_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_EXTERNAL_FORCING_T                               |
C     | o Add AIM tendency to T                                  | 
C     \==========================================================/

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
#endif

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J
      _RL phiTemp( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nSx,nSy)

C--   Forcing term
      _RL thetaLim,kT,ka,ks,term1,term2,thetaEq,termP,rSurf
      _RL pGround

C--   Forcing term
      pGround = 1. _d 5
      RD      = 287. _d 0
      CPAIR   = 1004. _d 0
      DO J=1,sNy
       DO I=1,sNx
        I2 = sNx*(J-1)+I
        katm = _KD2KA( Klev )
        gT(i,j,kLev,bi,bj) = gT(i,j,kLev,bi,bj) +(
     &                           TT_PBL(I2,katm)
     &                          +TT_CNV(I2,katm)
     &                          +TT_LSC(I2,katm)
     &                          +TT_RSW(I2,katm)
     &                          +TT_RLW(I2,katm)
     &        )*((pGround/rC(kLev))**(RD/CPAIR))
C       ig = myxgloballo+i-1
C       jg = myygloballo+j-1
C       if ( ig .eq.3 .and. jg .eq. 3 ) then
C         gT(i,j,kLev,bi,bj) = gT(i,j,kLev,bi,bj) +1.e-9
C       endif
        phiTemp(I,J,bi,bj) = gT(i,j,kLev,bi,bj)
       ENDDO
      ENDDO

      _EXCH_XY_R8( phiTemp , myThid) 

      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        gT(i,j,kLev,bi,bj) = phiTemp(I,J,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE AIM_EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_EXTERNAL_FORCING_S                               |
C     | o Add AIM tendency to S.                                 |
C     \==========================================================/

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
#endif


C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J
      _RL phiTemp( 1-OLx:sNx+OLx, 1-OLy:sNy+OLy,nSx,nSy)

      DO J=1,sNy
       DO I=1,sNx
        katm = _KD2KA( kLev )
        I2 = sNx*(J-1)+I
            gS(i,j,kLev,bi,bj) = gS(i,j,kLev,bi,bj)
     &                       +QT_PBL(I2,katm)
     &                       +QT_CNV(I2,katm)
     &                       +QT_LSC(I2,katm)
        phiTemp(I,J,bi,bj) = gS(i,j,kLev,bi,bj)
       ENDDO
      ENDDO

      _EXCH_XY_R8( phiTemp , myThid) 
C     _EXCH_XYZ_R8( gS , myThid) 

      DO J=1-OLy,sNy+OLy
       DO I=1-OLx,sNx+OLx
        gS(i,j,kLev,bi,bj) = phiTemp(I,J,bi,bj)
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
