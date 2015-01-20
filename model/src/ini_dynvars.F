C $Header: /u/gcmpack/MITgcm/model/src/ini_dynvars.F,v 1.27 2015/01/20 20:46:55 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_DYNVARS
C     !INTERFACE:
      SUBROUTINE INI_DYNVARS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_DYNVARS
C     | o Initialise to zero all DYNVARS.h arrays
C     *==========================================================*
C     | Sets all the State variables to zero.
C     | Sets all the Gs (arrays used for multi-level time-stepping)
C     | to zero.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id. number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     i,j,k   :: loop index
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx

           uVel (i,j,k,bi,bj) = 0. _d 0
           vVel (i,j,k,bi,bj) = 0. _d 0
           wVel (i,j,k,bi,bj) = 0. _d 0
           theta(i,j,k,bi,bj) = 0. _d 0
           salt (i,j,k,bi,bj) = 0. _d 0

           gU(i,j,k,bi,bj) = 0. _d 0
           gV(i,j,k,bi,bj) = 0. _d 0
#ifdef USE_OLD_EXTERNAL_FORCING
           gT(i,j,k,bi,bj) = 0. _d 0
           gS(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_ADAMSBASHFORTH_3
           guNm(i,j,k,bi,bj,1) = 0. _d 0
           gvNm(i,j,k,bi,bj,1) = 0. _d 0
           gtNm(i,j,k,bi,bj,1) = 0. _d 0
           gsNm(i,j,k,bi,bj,1) = 0. _d 0
           guNm(i,j,k,bi,bj,2) = 0. _d 0
           gvNm(i,j,k,bi,bj,2) = 0. _d 0
           gtNm(i,j,k,bi,bj,2) = 0. _d 0
           gsNm(i,j,k,bi,bj,2) = 0. _d 0
#else
           guNm1(i,j,k,bi,bj) = 0. _d 0
           gvNm1(i,j,k,bi,bj) = 0. _d 0
           gtNm1(i,j,k,bi,bj) = 0. _d 0
           gsNm1(i,j,k,bi,bj) = 0. _d 0
#endif
           totPhiHyd (i,j,k,bi,bj) = 0. _d 0
           rhoInSitu (i,j,k,bi,bj) = 0. _d 0
           IVDConvCount(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-OLy,sNy+OLy
         DO I=1-OLx,sNx+OLx
          etaN(i,j,bi,bj) = 0. _d 0
          etaH(i,j,bi,bj) = 0. _d 0
          phiHydLow(i,j,bi,bj) = 0. _d 0
          hMixLayer(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END
