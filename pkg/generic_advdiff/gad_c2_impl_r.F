C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_c2_impl_r.F,v 1.1 2004/01/07 21:37:59 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_C2_IMPL_R
C     !INTERFACE:
      SUBROUTINE GAD_C2_IMPL_R( 
     I           bi,bj,k, iMin,iMax,jMin,jMax, 
     I           deltaTarg, rTrans,
     O           a3d, b3d, c3d,
     I           myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R GAD_C2_IMPL_R
C     | o Compute matrix element to solve vertical advection
C     |   implicitly using centered second-order scheme
C     *==========================================================*
C     | o contribution of vertical transport at interface k
C     |   is added to matrix lines k & k-1 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj                :: tile indices
C     k                    :: vertical level
C     iMin,iMax,jMin,jMax  :: computation domain
C     deltaTarg            :: time step
C     rTrans               :: vertical volume transport
C     a3d                  :: lower diagonal of the tridiagonal matrix 
C     b3d                  :: main  diagonal of the tridiagonal matrix 
C     c3d                  :: upper diagonal of the tridiagonal matrix 
C     myThid               :: thread number
      INTEGER bi,bj,k
      INTEGER iMin,iMax,jMin,jMax
      _RL deltaTarg
      _RL rTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL a3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL b3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL c3d   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER myThid

C     == Local Variables ==
C     i,j                  :: loop indices
C     rCenter              :: centered contribution
      INTEGER i,j
      _RL rCenter
CEOP

      IF ( k.GT.Nr .OR. k.LT.2 ) RETURN

C--    Add centered contribution
       DO j=jMin,jMax
         DO i=iMin,iMax
           rCenter = 0.5 _d 0 *deltaTarg*rTrans(i,j)
     &                        *recip_rA(i,j,bi,bj)*rkFac
           a3d(i,j,k)   = a3d(i,j,k)
     &                  + rCenter
     &                   *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           b3d(i,j,k)   = b3d(i,j,k)
     &                  + rCenter
     &                    *recip_hFacC(i,j,k,bi,bj)*recip_drF(k)
           b3d(i,j,k-1) = b3d(i,j,k-1)
     &                  - rCenter
     &                    *recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
           c3d(i,j,k-1) = c3d(i,j,k-1)
     &                  - rCenter
     &                    *recip_hFacC(i,j,k-1,bi,bj)*recip_drF(k-1)
         ENDDO
       ENDDO

      RETURN
      END
