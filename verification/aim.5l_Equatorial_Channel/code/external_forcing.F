C     $Header: /u/gcmpack/MITgcm/verification/aim.5l_Equatorial_Channel/code/Attic/external_forcing.F,v 1.2 2001/02/02 21:36:31 adcroft Exp $
C     $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_U                                   |
C     | o Contains problem specific forcing for zonal velocity.  |
C     |==========================================================|
C     | Adds terms to gU for forcing by external sources         |
C     | e.g. wind stress, bottom friction etc..................  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
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

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_EXTERNAL_FORCING_U( iMin, iMax, jMin, jMax,
     &                                           bi,bj,kLev,
     &                                           myCurrentTime, myThid )
#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_V                                   |
C     | o Contains problem specific forcing for merid velocity.  |
C     |==========================================================|
C     | Adds terms to gV for forcing by external sources         |
C     | e.g. wind stress, bottom friction etc..................  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"


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
C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_EXTERNAL_FORCING_V( iMin, iMax, jMin, jMax,
     &                                           bi,bj,kLev,
     &                                           myCurrentTime, myThid )
#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           maskC,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_T                                   |
C     | o Contains problem specific forcing for temperature.     |
C     |==========================================================|
C     | Adds terms to gT for forcing by external sources         |
C     | e.g. heat flux, climatalogical relaxation..............  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_EXTERNAL_FORCING_T( iMin, iMax,
     &              jMin, jMax,bi,bj,kLev,
     &              myCurrentTime, myThid )
#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           maskC,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_S                                   |
C     | o Contains problem specific forcing for merid velocity.  |
C     |==========================================================|
C     | Adds terms to gS for forcing by external sources         |
C     | e.g. fresh-water flux, climatalogical relaxation.......  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_EXTERNAL_FORCING_S( iMin, iMax, 
     &                   jMin, jMax,bi,bj,kLev,
     &                   myCurrentTime, myThid )
#endif /* ALLOW_AIM */

      RETURN
      END
