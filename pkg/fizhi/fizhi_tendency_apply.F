C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_tendency_apply.F,v 1.14 2014/07/16 20:12:21 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      SUBROUTINE fizhi_tendency_apply_u(
     U                        gU_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
C=======================================================================
C Routine: fizhi_tendency_apply_u
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to U tendency.
C
C INPUT:
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C        Assumes that U and V tendencies are already on C-Grid
C=======================================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      _RL     gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      _RL rayleighdrag
      _RL tmpdiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i, j
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

      IF ( klev.EQ.Nr .OR. rC(klev).LT.1000. ) THEN
       rayleighdrag = 1./(31.*86400.*2.)
      ELSE
       rayleighdrag = 0.
      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
        gU_arr(i,j) = gU_arr(i,j)
     &              + maskW(i,j,kLev,bi,bj)
     &               *( guphy(i,j,kLev,bi,bj)
     &                - rayleighdrag*uVel(i,j,kLev,bi,bj) )
       ENDDO
      ENDDO

      IF ( DIAGNOSTICS_IS_ON('DIABUDYN',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) = maskW(i,j,kLev,bi,bj)
     &               *( guphy(i,j,kLev,bi,bj)
     &                - rayleighdrag*uVel(i,j,kLev,bi,bj) )
     &               * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'DIABUDYN',kLev,1,2,bi,bj,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('RFU     ',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) = -1. _d 0 * rayleighdrag *
     &       maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj) * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'RFU     ',kLev,1,2,bi,bj,myThid)
      ENDIF

      RETURN
      END
      SUBROUTINE fizhi_tendency_apply_v(
     U                        gV_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
C=======================================================================
C Routine: fizhi_tendency_apply_v
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to V tendency.
C
C INPUT:
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C        Assumes that U and V tendencies are already on C-Grid
C=======================================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      _RL     gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      _RL rayleighdrag
      _RL tmpdiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i, j
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

      IF ( klev.EQ.Nr .OR. rC(klev).LT.1000. ) THEN
       rayleighdrag = 1./(31.*86400.*2.)
      ELSE
       rayleighdrag = 0.
      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
        gV_arr(i,j) = gV_arr(i,j)
     &              + maskS(i,j,kLev,bi,bj)
     &               *( gvphy(i,j,kLev,bi,bj)
     &                - rayleighdrag*vVel(i,j,kLev,bi,bj) )
       ENDDO
      ENDDO

      IF ( DIAGNOSTICS_IS_ON('DIABVDYN',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) = maskS(i,j,kLev,bi,bj)
     &               *( gvphy(i,j,kLev,bi,bj)
     &                - rayleighdrag*vVel(i,j,kLev,bi,bj) )
     &               * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'DIABVDYN',kLev,1,2,bi,bj,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('RFV     ',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) = -1. _d 0 * rayleighdrag *
     &       maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj) * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'RFV     ',kLev,1,2,bi,bj,myThid)
      ENDIF

      RETURN
      END
      SUBROUTINE fizhi_tendency_apply_t(
     U                        gT_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
C=======================================================================
C Routine: fizhi_tendency_apply_t
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to T (theta) tendency.
C
C INPUT:
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C=======================================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      _RL rayleighdrag,getcon,cp,kappa,pNrkappa
      _RL tmpdiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i, j
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

      IF ( klev.EQ.Nr .OR. rC(klev).LT.1000. ) THEN
       cp = getcon('CP')
       kappa = getcon('KAPPA')
       pNrkappa = (rC(klev)/100000.)**kappa
       rayleighdrag = 1./((31.*86400.*2.)*(pNrkappa*cp))
      ELSE
       rayleighdrag = 0.
      ENDIF

      DO j=jMin,jMax
       DO i=iMin,iMax
        gT_arr(i,j) = gT_arr(i,j)
     &    + ( maskC(i,j,kLev,bi,bj)*gthphy(i,j,kLev,bi,bj)
     &      + rayleighdrag * 0.5
     &        *( maskW(i,j,kLev,bi,bj)
     &              *uVel(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)
     &         + maskW(i+1,j,kLev,bi,bj)
     &              *uVel(i+1,j,kLev,bi,bj)*uVel(i+1,j,kLev,bi,bj)
     &         + maskS(i,j,kLev,bi,bj)
     &              *vVel(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)
     &         + maskS(i,j+1,kLev,bi,bj)
     &              *vVel(i,j+1,kLev,bi,bj)*vVel(i,j+1,kLev,bi,bj)
     &         ) )
       ENDDO
      ENDDO

      IF ( DIAGNOSTICS_IS_ON('DIABTDYN',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) =
     &      ( maskC(i,j,kLev,bi,bj)*gthphy(i,j,kLev,bi,bj)
     &      + rayleighdrag * 0.5
     &        *( maskW(i,j,kLev,bi,bj)
     &              *uVel(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)
     &         + maskW(i+1,j,kLev,bi,bj)
     &              *uVel(i+1,j,kLev,bi,bj)*uVel(i+1,j,kLev,bi,bj)
     &         + maskS(i,j,kLev,bi,bj)
     &              *vVel(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)
     &         + maskS(i,j+1,kLev,bi,bj)
     &              *vVel(i,j+1,kLev,bi,bj)*vVel(i,j+1,kLev,bi,bj)
     &         ) ) * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'DIABTDYN',kLev,1,2,bi,bj,myThid)
      ENDIF

      IF ( DIAGNOSTICS_IS_ON('RFT     ',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) = ( rayleighdrag * 0.5
     &        *( maskW(i,j,kLev,bi,bj)
     &              *uVel(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)
     &         + maskW(i+1,j,kLev,bi,bj)
     &              *uVel(i+1,j,kLev,bi,bj)*uVel(i+1,j,kLev,bi,bj)
     &         + maskS(i,j,kLev,bi,bj)
     &              *vVel(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)
     &         + maskS(i,j+1,kLev,bi,bj)
     &              *vVel(i,j+1,kLev,bi,bj)*vVel(i,j+1,kLev,bi,bj)
     &         ) ) * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'RFT     ',kLev,1,2,bi,bj,myThid)
      ENDIF

      RETURN
      END
      SUBROUTINE fizhi_tendency_apply_s(
     U                        gS_arr,
     I                        iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                        myTime, myIter, myThid )
C=======================================================================
C Routine: fizhi_tendency_apply_s
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to S tendency.
C
C INPUT:
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C=======================================================================
      IMPLICIT NONE

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

      _RL tmpdiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i, j
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif

      DO j=jMin,jMax
       DO i=iMin,iMax
        gS_arr(i,j) = gS_arr(i,j)
     &    + maskC(i,j,kLev,bi,bj)*gsphy(i,j,kLev,bi,bj)
       ENDDO
      ENDDO

      IF ( DIAGNOSTICS_IS_ON('DIABQDYN',myThid) ) THEN
       DO j=jMin,jMax
       DO i=iMin,iMax
        tmpdiag(i,j) = ( maskC(i,j,kLev,bi,bj) * gsphy(i,j,kLev,bi,bj) )
     &    * 86400
       ENDDO
       ENDDO
       CALL DIAGNOSTICS_FILL(tmpdiag,'DIABQDYN',kLev,1,2,bi,bj,myThid)
      ENDIF

      RETURN
      END
