C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_diffusion.F,v 1.3 2011/06/07 22:26:37 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C !ROUTINE: THSICE_DIFFUSION

C !INTERFACE: ==========================================================
      SUBROUTINE THSICE_DIFFUSION(
     I                  maskOc,
     U                  uIce, vIce,
     I                  bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE THSICE_DIFFUSION
C     | o Account for total (ice+snow) thickness diffusion by
C     |   modifying ice-velocity:
C     |   If no velocity in the first place, and if using 1rst Order
C     |   upwind adv.scheme, this is equivalent to a diffusion of
C     |   ice+snow thichness.
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     maskOc    :: ocean surface mask (0=land ; 1=ocean)
C     uIce/vIce :: current ice velocity on C-grid [m/s]
C     bi,bj     :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id number
      _RS     maskOc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     uIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,      :: Loop counters
C     iceFld    :: sea-ice + snow mass density
C     msgBuf    :: Informational/error message buffer
      INTEGER i, j
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     iceFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpFld, hIceEpsil
      LOGICAL dBugFlag
c#include "THSICE_DEBUG.h"
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      dBugFlag = debugLevel.GE.debLevC
      hIceEpsil = 1. _d -10

      IF ( thSIce_diffK .GT. 0. ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceFld(i,j)  = ( rhos*snowHeight(i,j,bi,bj)
     &                    +rhoi*iceHeight(i,j,bi,bj) )
c                        *iceMask(i,j,bi,bj)
         ENDDO
        ENDDO

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          tmpFld = MAX( iceFld(i-1,j),iceFld(i,j) )
     &                * maskOc(i-1,j)*maskOc(i,j)
          IF ( tmpFld.GT.hIceEpsil )
     &    uIce(i,j) = uIce(i,j)
     &              + thSIce_diffK*( iceFld(i-1,j)-iceFld(i,j) )
     &                            *recip_dxC(i,j,bi,bj)/tmpFld
         ENDDO
        ENDDO

        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          tmpFld = MAX( iceFld(i,j-1),iceFld(i,j) )
     &                 *maskOc(i,j-1)*maskOc(i,j)
          IF ( tmpFld.GT.hIceEpsil )
     &    vIce(i,j) = vIce(i,j)
     &              + thSIce_diffK*( iceFld(i,j-1)-iceFld(i,j) )
     &                            *recip_dyC(i,j,bi,bj)/tmpFld
         ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
