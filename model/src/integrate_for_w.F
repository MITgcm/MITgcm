C $Header: /u/gcmpack/MITgcm/model/src/integrate_for_w.F,v 1.5 2001/02/07 03:00:00 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE INTEGRATE_FOR_W( 
     I        bi,bj,k,uFld,vFld,
     O        wFld,
     I        myThid)

C     /==========================================================\
C     | SUBROUTINE CALC_COMMON_FACTORS                           |
C     | o Calculate common data (such as volume flux) for use    |
C     |   by "Right hand side" subroutines.                      |
C     |==========================================================|
C     | Here, we calculate terms or spatially varying factors    |
C     | that are used at various points in the "RHS" subroutines.|
C     | This reduces the amount of total work, total memory      |
C     | and therefore execution time and is generally a good     |
C     | idea.                                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
      INTEGER bi,bj,k
      _RL  uFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  vFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL  wFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
      INTEGER i,j
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C--   Calculate velocity field "volume transports" through
C     tracer cell faces.
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          uTrans(i,j) = uFld(i,j,k,bi,bj)*
     &     _dyG(i,j,bi,bj)
     &     *drF(k)*_hFacW(i,j,k,bi,bj)
          vTrans(i,j) = vFld(i,j,k,bi,bj)*
     &     _dxG(i,j,bi,bj)
     &     *drF(k)*_hFacS(i,j,k,bi,bj)
        ENDDO
      ENDDO

C--   Calculate vertical "volume transport" through
C     tracer cell face *above* this level.
      IF (k.eq.1 .AND. rigidLid) THEN
        DO j=1-Oly,sNy+Oly-1
          DO i=1-Olx,sNx+Olx-1
            wFld(i,j,k,bi,bj) = 0.
          ENDDO
        ENDDO
      ELSEIF (k.eq.Nr) THEN
        DO j=1-Oly,sNy+Oly-1
          DO i=1-Olx,sNx+Olx-1
            wFld(i,j,k,bi,bj) =
     &         -(
     &            uTrans(i+1,j)-uTrans(i,j)
     &           +vTrans(i,j+1)-vTrans(i,j)
     &          )*recip_rA(i,j,bi,bj)
          ENDDO
        ENDDO
      ELSE
       DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
           wFld(i,j,k,bi,bj) = wFld(i,j,k+1,bi,bj)
     &         -(
     &            uTrans(i+1,j)-uTrans(i,j)
     &           +vTrans(i,j+1)-vTrans(i,j)
     &          )*recip_rA(i,j,bi,bj)
          ENDDO
        ENDDO
      ENDIF

      RETURN
      END
