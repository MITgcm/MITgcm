C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/Attic/aim_aim2dyn_exchanges.F,v 1.1 2002/11/22 17:17:03 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_AIM2DYN_EXCHANGES(
     I           myTime,myThid)
C     /==========================================================\
C     | S/R AIM_AIM2DYN_EXCHANGES                                |
C     | o Do overlap exchanges for AIM related quantities.       |
C     |   AIM does not compute into overlaps so exchanges        |
C     |   are needed for AIM quantities.                         |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h" 
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"

#include "AIM2DYN.h"

C     == Routine arguments ==
      _RL myTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM

C     == Local variables ==

      _EXCH_XY_R8( aim_drag, myThid )
c     _EXCH_XYZ_R8( gT, myThid )
c     _EXCH_XYZ_R8( gS, myThid )
c     _EXCH_XYZ_R8( gTNM1, myThid )
c     _EXCH_XYZ_R8( gSNM1, myThid )

#endif /* ALLOW_AIM */

      RETURN
      END
