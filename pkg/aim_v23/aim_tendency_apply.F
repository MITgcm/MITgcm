C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_tendency_apply.F,v 1.3 2003/07/31 18:40:57 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterface
      SUBROUTINE AIM_TENDENCY_APPLY_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myTime,myThid)
C     *==========================================================*
C     | S/R AIM_TENDENCY_APPLY_U
C     | o Add AIM tendency terms to U tendency.
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM_PARAMS.h"
#include "AIM2DYN.h"
#include "AIM_DIAGS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j  - Loop counters
      INTEGER i, j
      _RL DDTT, uStr_tmp

      DDTT = deltaTclock

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( kLev.EQ.Nr .AND. aim_dragStrato.GT.0. ) THEN
C- Note: exclusive IF / ELSE is legitimate here since surface drag 
C        is not supposed to be applied in stratosphere
       DO j=jMin,jMax
        DO i=iMin,iMax
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &     -maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)/aim_dragStrato
        ENDDO
       ENDDO
      ELSEIF (kLev.eq.1) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskW(i,j,kLev,bi,bj) .NE. 0. ) THEN
          uStr_tmp = 
     &     -( aim_drag(i-1,j,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * uVel(i,j,kLev,bi,bj)
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(kLev)
     &                       * recip_hFacW(i,j,kLev,bi,bj)
#ifdef ALLOW_AIM_TAVE
          USTRtave(i,j,bi,bj) = USTRtave(i,j,bi,bj) + uStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskW(i,j,kLev,bi,bj) .NE. 0. ) THEN
          uStr_tmp = 
     &      -( (1.-maskC(i-1,j,kLev-1,bi,bj))*aim_drag(i-1,j,bi,bj)
     &        +(1.-maskC( i ,j,kLev-1,bi,bj))*aim_drag( i ,j,bi,bj)
     &       )* 0.5 _d 0 * uVel(i,j,kLev,bi,bj)
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(kLev)
     &                       * recip_hFacW(i,j,kLev,bi,bj)
#ifdef ALLOW_AIM_TAVE
          USTRtave(i,j,bi,bj) = USTRtave(i,j,bi,bj) + uStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ENDIF
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE AIM_TENDENCY_APPLY_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myTime,myThid)
C     *==========================================================*
C     | S/R TENDENCY_APPLY_V
C     | o Add AIM tendency terms to V tendency.
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM_PARAMS.h"
#include "AIM2DYN.h"
#include "AIM_DIAGS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER i, j
      _RL DDTT, vStr_tmp

      DDTT = deltaTclock

c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( kLev.EQ.Nr .AND. aim_dragStrato.GT.0. ) THEN
C- Note: exclusive IF / ELSE is legitimate here since surface drag 
C        is not supposed to be applied in the stratosphere
       DO j=jMin,jMax
        DO i=iMin,iMax
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &     -maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)/aim_dragStrato
        ENDDO
       ENDDO
      ELSEIF (kLev.eq.1) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskS(i,j,kLev,bi,bj) .NE. 0. ) THEN
          vStr_tmp = 
     &     -( aim_drag(i,j-1,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * vVel(i,j,kLev,bi,bj)
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(kLev)
     &                       * recip_hFacS(i,j,kLev,bi,bj)
#ifdef ALLOW_AIM_TAVE
          VSTRtave(i,j,bi,bj) = VSTRtave(i,j,bi,bj) + vStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskS(i,j,kLev,bi,bj) .NE. 0. ) THEN
          vStr_tmp = 
     &     -( (1.-maskC(i,j-1,kLev-1,bi,bj))*aim_drag(i,j-1,bi,bj)
     &       +(1.-maskC(i, j ,kLev-1,bi,bj))*aim_drag(i, j ,bi,bj)
     &      )* 0.5 _d 0 * vVel(i,j,kLev,bi,bj)
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(kLev)
     &                       * recip_hFacS(i,j,kLev,bi,bj)
#ifdef ALLOW_AIM_TAVE
          VSTRtave(i,j,bi,bj) = VSTRtave(i,j,bi,bj) + vStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ENDIF
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE AIM_TENDENCY_APPLY_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myTime,myThid)
C     *==========================================================*
C     | S/R AIM_TENDENCY_APPLY_T
C     | o Add AIM tendency to gT
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing: add AIM heating/cooling tendency to gT:
      DO J=1,sNy
       DO I=1,sNx
        gT(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     &       *( gT(i,j,kLev,bi,bj) + aim_dTdt(i,j,kLev,bi,bj) )
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
CStartOfInterface
      SUBROUTINE AIM_TENDENCY_APPLY_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myTime,myThid)
C     *==========================================================*
C     | S/R AIM_TENDENCY_APPLY_S
C     | o Add AIM tendency to gS.
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid
CEndOfInterface


#ifdef ALLOW_AIM
C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing: add AIM dq/dt tendency to gS:
      DO J=1,sNy
       DO I=1,sNx
        gS(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     &       *( gS(i,j,kLev,bi,bj) + aim_dSdt(i,j,kLev,bi,bj) )
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
