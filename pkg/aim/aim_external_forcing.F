C     $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/aim_external_forcing.F,v 1.6 2002/09/27 20:05:11 jmc Exp $
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
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"
#include "AIM_DIAGS.h"

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
C     i,j  - Loop counters
      INTEGER i, j
      _RL DDTT, uStr_tmp

      DDTT = deltaTclock

#ifdef OLD_AIM_INTERFACE
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C - to reproduce old results (coupled run, summer 2000) :
      IF (kLev.eq.1) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          uStr_tmp = -aim_drag(i,j,bi,bj)*uVel(i,j,kLev,bi,bj)
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(kLev)
#ifdef ALLOW_TIMEAVE
          USTRtave(i,j,bi,bj) = USTRtave(i,j,bi,bj) + uStr_tmp*DDTT
#endif
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskW(i,j,kLev-1,bi,bj) .EQ. 0. ) THEN
          uStr_tmp = -aim_drag(i,j,bi,bj)*uVel(i,j,kLev,bi,bj)
#ifdef ALLOW_TIMEAVE
          USTRtave(i,j,bi,bj) = USTRtave(i,j,bi,bj) + uStr_tmp*DDTT
#endif
C - put the same bug as in the old setup :
          IF ( maskC(i,j,kLev-1,bi,bj) .EQ. 1. ) uStr_tmp = 0.
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(kLev)
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#else  /* OLD_AIM_INTERFACE */
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (kLev.eq.1) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( hFacW(i,j,kLev,bi,bj) .NE. 0. ) THEN
          uStr_tmp = 
     &     -( aim_drag(i-1,j,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * uVel(i,j,kLev,bi,bj)
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(kLev)
c    &                       * recip_hFacW(i,j,kLev,bi,bj)
#ifdef ALLOW_TIMEAVE
          USTRtave(i,j,bi,bj) = USTRtave(i,j,bi,bj) + uStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( hFacW(i,j,kLev,bi,bj) .NE. 0. ) THEN
          uStr_tmp = 
     &      -( (1.-maskC(i-1,j,kLev-1,bi,bj))*aim_drag(i-1,j,bi,bj)
     &        +(1.-maskC( i ,j,kLev-1,bi,bj))*aim_drag( i ,j,bi,bj)
     &       )* 0.5 _d 0 * uVel(i,j,kLev,bi,bj)
          gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(kLev)
c    &                       * recip_hFacW(i,j,kLev,bi,bj)
#ifdef ALLOW_TIMEAVE
          USTRtave(i,j,bi,bj) = USTRtave(i,j,bi,bj) + uStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ENDIF
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* OLD_AIM_INTERFACE */

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
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"
#include "AIM_DIAGS.h"

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
      INTEGER i, j
      _RL DDTT, vStr_tmp

      DDTT = deltaTclock

C     call CHKSUM_TILED( ' drag AAA ',aim_drag,1,bi,bj,myThid )

#ifdef OLD_AIM_INTERFACE
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C - to reproduce old results (coupled run, summer 2000) :
      IF (kLev.eq.1) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
          vStr_tmp = -aim_drag(i,j,bi,bj)*vVel(i,j,kLev,bi,bj)
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(kLev)
#ifdef ALLOW_TIMEAVE
          VSTRtave(i,j,bi,bj) = VSTRtave(i,j,bi,bj) + vStr_tmp*DDTT
#endif
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskS(i,j,kLev-1,bi,bj) .EQ. 0. ) THEN
          vStr_tmp = -aim_drag(i,j,bi,bj)*vVel(i,j,kLev,bi,bj)
#ifdef ALLOW_TIMEAVE
          VSTRtave(i,j,bi,bj) = VSTRtave(i,j,bi,bj) + vStr_tmp*DDTT
#endif
C - put the same bug as in the old setup :
          IF ( maskC(i,j,kLev-1,bi,bj) .EQ. 1. ) vStr_tmp = 0.
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(kLev)
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#else  /* OLD_AIM_INTERFACE */
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (kLev.eq.1) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( hFacS(i,j,kLev,bi,bj) .NE. 0. ) THEN
          vStr_tmp = 
     &     -( aim_drag(i,j-1,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * vVel(i,j,kLev,bi,bj)
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(kLev)
c    &                       * recip_hFacS(i,j,kLev,bi,bj)
#ifdef ALLOW_TIMEAVE
          VSTRtave(i,j,bi,bj) = VSTRtave(i,j,bi,bj) + vStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( hFacS(i,j,kLev,bi,bj) .NE. 0. ) THEN
          vStr_tmp = 
     &     -( (1.-maskC(i,j-1,kLev-1,bi,bj))*aim_drag(i,j-1,bi,bj)
     &       +(1.-maskC(i, j ,kLev-1,bi,bj))*aim_drag(i, j ,bi,bj)
     &      )* 0.5 _d 0 * vVel(i,j,kLev,bi,bj)
          gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(kLev)
c    &                       * recip_hFacS(i,j,kLev,bi,bj)
#ifdef ALLOW_TIMEAVE
          VSTRtave(i,j,bi,bj) = VSTRtave(i,j,bi,bj) + vStr_tmp*DDTT
#endif
         ENDIF
        ENDDO
       ENDDO
      ENDIF
c---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* OLD_AIM_INTERFACE */

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
      _RL myCurrentTime
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
      SUBROUTINE AIM_EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R AIM_EXTERNAL_FORCING_S                               |
C     | o Add AIM tendency to S.                                 |
C     \==========================================================/
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
      _RL myCurrentTime
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
