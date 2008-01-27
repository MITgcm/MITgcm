C $Header: /u/gcmpack/MITgcm/pkg/mypackage/mypackage_tendency_apply.F,v 1.3 2008/01/27 20:52:11 jmc Exp $
C $Name:  $

#include "MYPACKAGE_OPTIONS.h"
C--  File mypackage_tendency_apply.F: Routines to apply MYPACKAGE tendencies
C--   Contents
C--   o MYPACKAGE_TENDENCY_APPLY_U
C--   o MYPACKAGE_TENDENCY_APPLY_V
C--   o MYPACKAGE_TENDENCY_APPLY_T
C--   o MYPACKAGE_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_U

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_U(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add mypackage tendency terms to U tendency.  Routine works for one
C     level at a time. Assumes that U and V tendencies are on C-Grid or
C     on A-grid.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef MYPACKAGE_TENDENCY
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendU .AND. myPa_Tend_Cgrid ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     + maskW(i,j,k,bi,bj) * myPa_TendVelU(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ELSEIF ( myPa_applyTendU ) THEN
C--   Assume Agrid position:
        DO j=jMin,jMax
         DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     + maskW(i,j,k,bi,bj)*( myPa_TendVelU(i-1,j,k,bi,bj)
     &                           +myPa_TendVelU( i ,j,k,bi,bj)
     &                          )*0.5 _d 0
         ENDDO
        ENDDO
      ENDIF

#endif /* MYPACKAGE_TENDENCY */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_V

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_V(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add mypackage tendency terms to V tendency.  Routine works for one
C     level at a time. Assumes that U and V tendencies are on C-Grid or
C     on A-grid.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef MYPACKAGE_TENDENCY
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendV .AND. myPa_Tend_Cgrid ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     + maskS(i,j,k,bi,bj) * myPa_TendVelV(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ELSEIF ( myPa_applyTendV ) THEN
C--   Assume Agrid position:
        DO j=jMin,jMax
         DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     + maskS(i,j,k,bi,bj)*( myPa_TendVelV(i,j-1,k,bi,bj)
     &                           +myPa_TendVelV(i, j ,k,bi,bj)
     &                          )*0.5 _d 0
         ENDDO
        ENDDO
      ENDIF

#endif /* MYPACKAGE_TENDENCY */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_T(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add mypackage tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef MYPACKAGE_TENDENCY
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendT ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gT(i,j,k,bi,bj) = gT(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * myPa_TendScal1(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* MYPACKAGE_TENDENCY */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MYPACKAGE_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE MYPACKAGE_TENDENCY_APPLY_S(
     &                     iMin, iMax, jMin, jMax,
     &                     bi, bj, k, myTime, myThid )

C     !DESCRIPTION:
C     Add mypackage tendency terms to S tendency.  Routine works for one
C     level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj, myThid
      _RL myTime
CEOP

#ifdef MYPACKAGE_TENDENCY
C     !LOCAL VARIABLES:
      INTEGER i, j

      IF ( myPa_applyTendS ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gS(i,j,k,bi,bj) = gS(i,j,k,bi,bj)
     &     + maskC(i,j,k,bi,bj) * myPa_TendScal2(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

#endif /* MYPACKAGE_TENDENCY */

      RETURN
      END
