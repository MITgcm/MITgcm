C $Header: /u/gcmpack/MITgcm/model/src/Attic/calc_common_factors.F,v 1.21 2006/06/18 23:20:35 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_COMMON_FACTORS
C     !INTERFACE:
      SUBROUTINE CALC_COMMON_FACTORS(
     I                uVel, vVel,
     O                uFld, vFld, uTrans, vTrans, xA, yA,
     I                k,bi,bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_COMMON_FACTORS
C     | o Calculate common data (such as volume flux) for use
C     |   by "Right hand side" subroutines.
C     *==========================================================*
C     | Here, we calculate terms or spatially varying factors
C     | that are used at various points in the "RHS" subroutines.
C     | This reduces the amount of total work, total memory
C     | and therefore execution time and is generally a good
C     | idea.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     uVel    :: velocity, zonal component
C     vVel    :: velocity, meridional component
C     uFld    :: 2-D local copy of horizontal velocity, zonal  component
C     vFld    :: 2-D local copy of horizontal velocity, merid. component
C     uTrans  :: Zonal volume transport through cell face
C     vTrans  :: Meridional volume transport through cell face
C     xA      :: Tracer cell face area normal to X
C     yA      :: Tracer cell face area normal to X
C     k,bi,bj :: vertical & tile indices for this calculation
C     myThid  :: my Thread Id. number

      _RL uVel  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vVel  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL uFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS xA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER k,bi,bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j :: Loop counters
      INTEGER i,j
CEOP

C--   Initialisation
c     DO j=1-OLy,sNy+OLy
c      DO i=1-OLx,sNx+OLx
c       xA(i,j)     = 0. _d 0
c       yA(i,j)     = 0. _d 0
c       uFld(i,j)   = 0. _d 0
c       vFld(i,j)   = 0. _d 0
c       uTrans(i,j) = 0. _d 0
c       vTrans(i,j) = 0. _d 0
c      ENDDO
c     ENDDO

C--   Calculate tracer cell face open areas
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         xA(i,j) = _dyG(i,j,bi,bj)*drF(k)*_hFacW(i,j,k,bi,bj)
         yA(i,j) = _dxG(i,j,bi,bj)*drF(k)*_hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Make a local copy of velocity component :
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uFld(i,j) = uVel(i,j,k,bi,bj)
         vFld(i,j) = vVel(i,j,k,bi,bj)
       ENDDO
      ENDDO

C--   Calculate "volume transports" through tracer cell faces.
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         uTrans(i,j) = uFld(i,j)*xA(i,j)
         vTrans(i,j) = vFld(i,j)*yA(i,j)
       ENDDO
      ENDDO

      RETURN
      END
