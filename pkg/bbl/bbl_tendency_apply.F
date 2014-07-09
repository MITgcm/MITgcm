C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_tendency_apply.F,v 1.3 2014/07/09 17:00:49 jmc Exp $
C $Name:  $

#include "BBL_OPTIONS.h"
C--  File bbl_tendency_apply.F: Routines to apply BBL tendencies
C--   Contents
C--   o BBL_TENDENCY_APPLY_T
C--   o BBL_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_TENDENCY_APPLY_T

C !INTERFACE:
      SUBROUTINE BBL_TENDENCY_APPLY_T(
     U                     gT_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add bbl tendency terms to T (theta) tendency.  Routine works
C     for one level at a time

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "BBL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      DO j=jMin,jMax
       DO i=iMin,iMax
         IF ( k .EQ. kLowC(i,j,bi,bj) )
     &     gT_arr(i,j) = gT_arr(i,j)
     &       + maskC(i,j,k,bi,bj) * bbl_TendTheta(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: BBL_TENDENCY_APPLY_S

C !INTERFACE:
      SUBROUTINE BBL_TENDENCY_APPLY_S(
     U                     gS_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Add bbl tendency terms to S tendency.  Routine works for one
C     level at a time

C     !INPUT PARAMETERS:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "BBL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j

      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( k .EQ. kLowC(i,j,bi,bj) )
     &     gS_arr(i,j) = gS_arr(i,j)
     &       + maskC(i,j,k,bi,bj) * bbl_TendSalt(i,j,bi,bj)
       ENDDO
      ENDDO

      RETURN
      END
