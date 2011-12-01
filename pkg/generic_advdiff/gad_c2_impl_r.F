C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c2_impl_r.F,v 1.6 2011/12/01 14:14:43 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_C2_IMPL_R
C     !INTERFACE:
      SUBROUTINE GAD_C2_IMPL_R(
     I           bi,bj,k, iMin,iMax,jMin,jMax,
     I           deltaTarg, rTrans, recip_hFac,
     O           a3d, b3d, c3d,
     I           myThid )

C     !DESCRIPTION:
C     Compute matrix element to solve vertical advection implicitly
C     using centered second-order scheme The contribution of vertical
C     transport at interface k is added to matrix lines k and k-1.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj        :: tile indices
C     k            :: vertical level
C     iMin,iMax    :: computation domain
C     jMin,jMax    :: computation domain
C     deltaTarg    :: time step
C     rTrans       :: vertical volume transport
C     recip_hFac   :: inverse of cell open-depth factor
C     a3d          :: lower diagonal of the tridiagonal matrix
C     b3d          :: main  diagonal of the tridiagonal matrix
C     c3d          :: upper diagonal of the tridiagonal matrix
C     myThid       :: thread number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      _RL     deltaTarg(Nr)
      _RL     rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS recip_hFac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     a3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     b3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL     c3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     == Local Variables ==
C     i,j          :: loop indices
C     rCenter      :: centered contribution
      INTEGER i,j
      _RL rCenter
CEOP

C--   process interior interface only:
      IF ( k.GT.1 .AND. k.LE.Nr ) THEN

C--    Add centered contribution
       DO j=jMin,jMax
         DO i=iMin,iMax
           rCenter = 0.5 _d 0 *rTrans(i,j)*recip_rA(i,j,bi,bj)*rkSign
           a3d(i,j,k)   = a3d(i,j,k)
     &                  - rCenter*deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
           b3d(i,j,k)   = b3d(i,j,k)
     &                  - rCenter*deltaTarg(k)
     &                   *recip_hFac(i,j,k)*recip_drF(k)
           b3d(i,j,k-1) = b3d(i,j,k-1)
     &                  + rCenter*deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
           c3d(i,j,k-1) = c3d(i,j,k-1)
     &                  + rCenter*deltaTarg(k-1)
     &                   *recip_hFac(i,j,k-1)*recip_drF(k-1)
         ENDDO
       ENDDO

C--   process interior interface only: end
      ENDIF

      RETURN
      END
