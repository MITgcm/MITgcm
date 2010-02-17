C $Header: /u/gcmpack/MITgcm/model/src/ini_dynvars.F,v 1.23 2010/02/17 23:39:32 gforget Exp $
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
C     I,J,K   :: loop index
      INTEGER bi, bj
      INTEGER I, J, K
CEOP

C--   Over all tiles
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C-      3D arrays
        DO K=1,Nr
         DO J=1-Oly,sNy+Oly
          DO I=1-Olx,sNx+Olx

           uVel(I,J,K,bi,bj)=0. _d 0
           vVel(I,J,K,bi,bj)=0. _d 0
           wVel(I,J,K,bi,bj)=0. _d 0
           theta(I,J,K,bi,bj)=0. _d 0
           salt(I,J,K,bi,bj)=0. _d 0

           gU(I,J,K,bi,bj)=0. _d 0
           gV(I,J,K,bi,bj)=0. _d 0
           gT(I,J,K,bi,bj)=0. _d 0
           gS(I,J,K,bi,bj)=0. _d 0
#ifdef ALLOW_ADAMSBASHFORTH_3
           guNm(I,J,K,bi,bj,1)=0. _d 0
           gvNm(I,J,K,bi,bj,1)=0. _d 0
           gtNm(I,J,K,bi,bj,1)=0. _d 0
           gsNm(I,J,K,bi,bj,1)=0. _d 0
           guNm(I,J,K,bi,bj,2)=0. _d 0
           gvNm(I,J,K,bi,bj,2)=0. _d 0
           gtNm(I,J,K,bi,bj,2)=0. _d 0
           gsNm(I,J,K,bi,bj,2)=0. _d 0
#else
           guNm1(I,J,K,bi,bj) =0. _d 0
           gvNm1(I,J,K,bi,bj) =0. _d 0
           gtNm1(I,J,K,bi,bj) =0. _d 0
           gsNm1(I,J,K,bi,bj) =0. _d 0
#endif
           totPhiHyd(I,J,K,bi,bj)=0. _d 0
           rhoInSitu(I,J,K,bi,bj)=0. _d 0
           IVDConvCount(i,j,k,bi,bj) = 0. _d 0
#ifdef ALLOW_ADDFLUID
           addMass(I,J,K,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO

C-      2D arrays
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          etaN(I,J,bi,bj)=0. _d 0
          etaH(I,J,bi,bj)=0. _d 0
          phiHydLow(I,J,bi,bj)=0. _d 0
          hMixLayer(I,J,bi,bj)=0. _d 0
         ENDDO
        ENDDO

       ENDDO
      ENDDO

      RETURN
      END
