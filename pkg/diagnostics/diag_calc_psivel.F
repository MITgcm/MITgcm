C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diag_calc_psivel.F,v 1.5 2014/11/26 18:31:34 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAG_CALC_PSIVEL
C     !INTERFACE:
      SUBROUTINE DIAG_CALC_PSIVEL(
     I                k, iPsi0, jPsi0, uTrans, vTrans,
     O                psiVel, psiLoc,
     I                prtMsg, myTime, myIter, myThid )
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
#ifdef ALLOW_OBCS
# include "GRID.h"
# include "OBCS_GRID.h"
#endif /* ALLOW_OBCS */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     k       :: current level
C     i,jPsi0 :: indices of grid-point location where Psi == 0
C     uTrans  :: horizontal transport, u-component
C     vTrans  :: horizontal transport, u-component
C     psiVel  :: horizontal stream-function
C     psiLoc  :: horizontal stream-function at special location
C     prtMsg  :: do print message to standard-output
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
      LOGICAL prtMsg
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
#ifdef ALLOW_OBCS
      INTEGER is, js, ix, jx, iy, jy, ijCnt
      INTEGER npass, nPts, prev_nPts
      LOGICAL kPsi(1:sNx+1,1:sNy+1)
#endif /* ALLOW_OBCS */
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DIAG_CALC_PSIVEL',myThid)
#endif

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
        IF ( useOBCS ) THEN
C-    Case with OBC:
#ifdef ALLOW_OBCS
C- Note: OB may introduce discontinuity in domain & tile stream-function map;
C     within a tile: define a local "is-set" mask (=kPsi) and
C                    propagate stream-function value without assumption.
C     between tiles: present code is not "general", likely to work with
C                    simple OB setting and/or simple tile connection (no exch2).
C     A truly general algorithm requires to change CUMULSUM_Z_TILE (adding 1
C     more input dPsi/tile) and to account for disabled tile-connection due
C     to OB when setting cumsum tile-mapping (W2_SET_MAP_CUMSUM).
         DO j=1,sNy+1
          DO i=1,sNx+1
            kPsi(i,j) = .FALSE.
            psiVel(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
C-    select starting point
         ijCnt = sNx+sNy+1
         is = 0
         js = 0
         DO j=1,sNy
          DO i=1,sNx
           IF ( OBCS_insideMask(i,j,bi,bj).EQ.1.
     &                      .AND. (i+j).LE.ijCnt ) THEN
             is = i
             js = j
             ijCnt = i+j
           ENDIF
          ENDDO
         ENDDO
         IF ( is.EQ.0 ) THEN
           nPts = 0
         ELSE
           kPsi(is,js) = .TRUE.
           nPts = 1
         ENDIF
         npass = 0
         prev_nPts = 0
         DO WHILE ( nPts.GT.prev_nPts )
           prev_nPts = nPts
           npass = npass + 1
           DO j=1,sNy+1
            DO i=1,sNx
             IF ( OBCS_insideMask(i,j-1,bi,bj).EQ.1. .OR.
     &            OBCS_insideMask(i, j ,bi,bj).EQ.1. ) THEN
               IF ( kPsi(i,j) .AND. .NOT.kPsi(i+1,j) ) THEN
                 nPts = nPts + 1
                 kPsi(i+1,j) = .TRUE.
                 psiVel(i+1,j,bi,bj) = psiVel(i,j,bi,bj)
     &                               + vTrans(i,j,bi,bj)
               ENDIF
               IF ( .NOT.kPsi(i,j) .AND. kPsi(i+1,j) ) THEN
                 nPts = nPts + 1
                 kPsi(i,j) = .TRUE.
                 psiVel(i,j,bi,bj) = psiVel(i+1,j,bi,bj)
     &                               - vTrans(i,j,bi,bj)
               ENDIF
             ENDIF
            ENDDO
           ENDDO
           DO j=1,sNy
            DO i=1,sNx+1
             IF ( OBCS_insideMask(i-1,j,bi,bj).EQ.1. .OR.
     &            OBCS_insideMask( i ,j,bi,bj).EQ.1. ) THEN
               IF ( kPsi(i,j) .AND. .NOT.kPsi(i,j+1) ) THEN
                 nPts = nPts + 1
                 kPsi(i,j+1) = .TRUE.
                 psiVel(i,j+1,bi,bj) = psiVel(i,j,bi,bj)
     &                               - uTrans(i,j,bi,bj)
               ENDIF
               IF ( .NOT.kPsi(i,j) .AND. kPsi(i,j+1) ) THEN
                 nPts = nPts + 1
                 kPsi(i,j) = .TRUE.
                 psiVel(i,j,bi,bj) = psiVel(i,j+1,bi,bj)
     &                               + uTrans(i,j,bi,bj)
               ENDIF
             ENDIF
            ENDDO
           ENDDO
           IF ( prtMsg .AND. nPts.GT.prev_nPts ) THEN
             _BEGIN_MASTER( myThid )
             WRITE(standardMessageUnit,'(A,2I4,A,I6,I8)')
     &          ' diag_calc_psivel: bi,bj=', bi, bj,
     &          ' : npass,nPts=', npass, nPts
             _END_MASTER( myThid )
           ENDIF
C-    end do while (npass count)
         ENDDO
C-    set tile increments
         ix = 0
         jx = 0
         DO i=sNx+1,1,-1
          DO j=1,sNy
           IF ( kPsi(i,j) .AND. jx.EQ.0 ) THEN
             ix = i
             jx = j
           ENDIF
          ENDDO
         ENDDO
         IF ( jx.NE.0 ) dPsiX (bi,bj) = psiVel(ix,jx,bi,bj)
         iy = 0
         jy = 0
         DO j=sNy+1,1,-1
          DO i=1,sNx
           IF ( kPsi(i,j) .AND. iy.EQ.0 ) THEN
             iy = i
             jy = j
           ENDIF
          ENDDO
         ENDDO
         IF ( iy.NE.0 ) dPsiY (bi,bj) = psiVel(iy,jy,bi,bj)
         IF ( prtMsg ) THEN
           _BEGIN_MASTER( myThid )
           WRITE(standardMessageUnit,'(3(A,2I4))')
     &          ' diag_calc_psivel:            is,js=', is,js,
     &                 ' ; ix,jx=', ix,jx, ' ; iy,jy=', iy,jy
c          IF ( iPsi0(bi,bj)*jPsi0(bi,bj).GT.0 )
c    &     WRITE(standardMessageUnit,'(A,L5))')
c    &          ' diag_calc_psivel:          kPsi @ i,jPsi0 =',
c    &                          kPsi(iPsi0(bi,bj),jPsi0(bi,bj))
           _END_MASTER( myThid )
         ENDIF
#endif /* ALLOW_OBCS */
        ELSE
C-    Case without OBC:
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
C-    end with/without OBC cases
        ENDIF
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

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DIAG_CALC_PSIVEL',myThid)
#endif

      RETURN
      END
