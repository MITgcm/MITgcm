C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diag_calc_psivel.F,v 1.2 2011/07/06 01:45:32 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAG_CALC_PSIVEL
C     !INTERFACE:
      SUBROUTINE DIAG_CALC_PSIVEL(
     I                k, iPsi0, jPsi0, uTrans, vTrans,
     O                psiVel, psiLoc,
     U                myTIme, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DIAG_CALC_PSIVEL
C     | o Calculate horizontal transport stream-function
C     |   from non-divergent horizontal transport.
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
c#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     k       :: current level
C     i,jPsi0 :: indices of grid-point location where Psi == 0
C     uTrans  :: horizontal transport, u-component
C     vTrans  :: horizontal transport, u-component
C     psiVel  :: horizontal stream-function
C     psiLoc  :: horizontal stream-function at special location
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER k
      INTEGER iPsi0(nSx,nSy)
      INTEGER jPsi0(nSx,nSy)
      _RL  uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  psiVel(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL  psiLoc(2)
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ====
C     bi, bj  :: tile indices
C     i, j    :: loop indices
C     dPsiX   :: tile stream-function increment along X-dir
C     dPsiY   :: tile stream-function increment along Y-dir
C     psiOri  :: stream-function value at tile origin
      INTEGER bi, bj
      INTEGER i, j
      _RL    dPsiX (nSx,nSy)
      _RL    dPsiY (nSx,nSy)
      _RL    psiOri(nSx,nSy)
      _RL    offSet
      LOGICAL zeroPsi
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C--   Initialise
      zeroPsi = iPsi0(1,1).GE.0
      psiLoc(1) = 0.
      psiLoc(2) = 0.

C--   step.1 : compute Psi over each tile separately
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         dPsiX (bi,bj) = 0.
         dPsiY (bi,bj) = 0.
         psiOri(bi,bj) = 0.
         psiVel(1,1,bi,bj) = psiOri(bi,bj)
         j = 1
         DO i=1,sNx
           psiVel(i+1,j,bi,bj) = psiVel(i,j,bi,bj)
     &                         + vTrans(i,j,bi,bj)
         ENDDO
C-     note: can vectorise inner loop
         DO j=1,sNy
          DO i=1,sNx+1
           psiVel(i,j+1,bi,bj) = psiVel(i,j,bi,bj)
     &                         - uTrans(i,j,bi,bj)
          ENDDO
         ENDDO
         dPsiX (bi,bj) = psiVel(1+sNx,1,bi,bj)
         dPsiY (bi,bj) = psiVel(1,1+sNy,bi,bj)
       ENDDO
      ENDDO

      CALL CUMULSUM_Z_TILE_RL(
     O                  psiOri, psiLoc,
     I                  dPsiX, dPsiY, myThid )

C--   step.2 : account for Psi @ tile origin
      offSet = 0.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy+1
         DO i=1,sNx+1
           psiVel(i,j,bi,bj) = psiVel(i,j,bi,bj) + psiOri(bi,bj)
         ENDDO
        ENDDO
        IF ( iPsi0(bi,bj)*jPsi0(bi,bj).GT.0 )
     &   offSet = -psiVel(iPsi0(bi,bj),jPsi0(bi,bj),bi,bj)
       ENDDO
      ENDDO

      IF ( zeroPsi ) THEN
C--   step.3 : shift stream-function to satisfy Psi == 0 @ a particular location
        _GLOBAL_SUM_RL( offSet, myThid )
        psiLoc(1) = psiLoc(1) + offSet
        psiLoc(2) = psiLoc(2) + offSet
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1,sNy+1
           DO i=1,sNx+1
             psiVel(i,j,bi,bj) = psiVel(i,j,bi,bj) + offSet
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      RETURN
      END
