C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_diag_balance.F,v 1.4 2014/11/21 20:59:41 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_DIAG_BALANCE

C     !INTERFACE:
      SUBROUTINE OBCS_DIAG_BALANCE(
     U                              div2d,
     I                              uTrans, vTrans, k,
     I                              myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_DIAG_BALANCE
C     | o For diagnostics purpose, modify horizontal divergence
C     |   next (but outside) OB to ensure zero net inflow
C     | o use similar scheme as OBCS_BALANCE_FLOW
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     div2d   :: horizontal divergence x grid-cell volume [m^3/s]
C     uTrans  :: horizontal transport to balance [m^3/s]
C     vTrans  :: horizontal transport to balance [m^3/s]
C     k       :: current level index
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      _RL div2d (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER k
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_DIAGNOSTICS

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     bi, bj       :: tile indices
C     i,j,k        :: loop indices
C     iB, jB       :: local index of open boundary
C     msgBuf       :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, iB, jB
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL areaOB, inFlow, tmpA
      _RL tileArea(nSx,nSy)
      _RL tileFlow(nSx,nSy)
      _RL tileAreaOB(nSx,nSy)
      _RL tileInFlow(nSx,nSy)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_DIAG_BALANCE',myThid)
#endif

C--   Integrate the transport through each OB
      areaOB = 0. _d 0
      inFlow = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileAreaOB(bi,bj) = 0.
        tileInFlow(bi,bj) = 0.
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS_EAST
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBE(bi,bj) ) THEN
         DO j=1,sNy
           iB = OB_Ie(j,bi,bj)
           IF ( iB.NE.OB_indexNone .AND. iB.GT.1 ) THEN
            tmpA = drF(k)*hFacW(iB,j,k,bi,bj)*dyG(iB,j,bi,bj)
     &                   *maskInW(iB,j,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + uTrans(iB,j,bi,bj)
           ENDIF
         ENDDO
         tileInFlow(bi,bj) = tileInFlow(bi,bj) - tileFlow(bi,bj)
         tileAreaOB(bi,bj) = tileAreaOB(bi,bj) + tileArea(bi,bj)
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBW(bi,bj) ) THEN
         DO j=1,sNy
           iB = OB_Iw(j,bi,bj)
           IF ( iB.NE.OB_indexNone .AND. iB.LT.sNx ) THEN
            iB = 1+iB
            tmpA = drF(k)*hFacW(iB,j,k,bi,bj)*dyG(iB,j,bi,bj)
     &                   *maskInW(iB,j,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + uTrans(iB,j,bi,bj)
           ENDIF
         ENDDO
         tileInFlow(bi,bj) = tileInFlow(bi,bj) + tileFlow(bi,bj)
         tileAreaOB(bi,bj) = tileAreaOB(bi,bj) + tileArea(bi,bj)
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBN(bi,bj) ) THEN
         DO i=1,sNx
           jB = OB_Jn(i,bi,bj)
           IF ( jB.NE.OB_indexNone .AND. jB.GT.1 ) THEN
            tmpA = drF(k)*hFacS(i,jB,k,bi,bj)*dxG(i,jB,bi,bj)
     &                   *maskInS(i,jB,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + vTrans(i,jB,bi,bj)
           ENDIF
         ENDDO
         tileInFlow(bi,bj) = tileInFlow(bi,bj) - tileFlow(bi,bj)
         tileAreaOB(bi,bj) = tileAreaOB(bi,bj) + tileArea(bi,bj)
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBS(bi,bj) ) THEN
         DO i=1,sNx
           jB = OB_Js(i,bi,bj)
           IF ( jB.NE.OB_indexNone .AND. jB.LT.sNy ) THEN
            jB = 1+jB
            tmpA = drF(k)*hFacS(i,jB,k,bi,bj)*dxG(i,jB,bi,bj)
     &                   *maskInS(i,jB,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + vTrans(i,jB,bi,bj)
           ENDIF
         ENDDO
         tileInFlow(bi,bj) = tileInFlow(bi,bj) + tileFlow(bi,bj)
         tileAreaOB(bi,bj) = tileAreaOB(bi,bj) + tileArea(bi,bj)
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_SOUTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Calculate a unique velocity correction for all (OBCS_balanceFac>0) OB
C     and correct each OB using corresponding weight factor OBCS_balanceFac

      CALL GLOBAL_SUM_TILE_RL( tileAreaOB, areaOB, myThid )
      IF ( areaOB.GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileInFlow, inFlow, myThid )
        IF ( debugLevel.GE.debLevC ) THEN
          WRITE(msgBuf,'(A,I9,A,1P2E16.8)') 'OBCS_DIAG_balance (it=',
     &       myIter, ' ) correct for inFlow:', inFlow, inFlow/areaOB
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
        inFlow = inFlow / areaOB
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Add correction:

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_OBCS_EAST
         IF ( tileHasOBE(bi,bj) ) THEN
           DO j=1-OLy,sNy+OLy
            IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
             iB = OB_Ie(j,bi,bj)
             div2d(iB,j,bi,bj) = div2d(iB,j,bi,bj)
     &                   + inFlow*drF(k)*hFacW(iB,j,k,bi,bj)
     &                           *dyG(iB,j,bi,bj)*maskInW(iB,j,bi,bj)
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
         IF ( tileHasOBW(bi,bj) ) THEN
           DO j=1-OLy,sNy+OLy
            IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
             iB = 1+OB_Iw(j,bi,bj)
             div2d(iB-1,j,bi,bj) = div2d(iB-1,j,bi,bj)
     &                   + inFlow*drF(k)*hFacW(iB,j,k,bi,bj)
     &                           *dyG(iB,j,bi,bj)*maskInW(iB,j,bi,bj)
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
         IF ( tileHasOBN(bi,bj) ) THEN
           DO i=1-OLx,sNx+OLx
            IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
             jB = OB_Jn(i,bi,bj)
             div2d(i,jB,bi,bj) = div2d(i,jB,bi,bj)
     &                   + inFlow*drF(k)*hFacS(i,jB,k,bi,bj)
     &                           *dxG(i,jB,bi,bj)*maskInS(i,jB,bi,bj)
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
         IF ( tileHasOBS(bi,bj) ) THEN
           DO i=1-OLx,sNx+OLx
            IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
             jB = 1+OB_Js(i,bi,bj)
             div2d(i,jB-1,bi,bj) = div2d(i,jB-1,bi,bj)
     &                   + inFlow*drF(k)*hFacS(i,jB,k,bi,bj)
     &                           *dxG(i,jB,bi,bj)*maskInS(i,jB,bi,bj)
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_SOUTH */

       ENDDO
      ENDDO

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_DIAG_BALANCE',myThid)
#endif

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_OBCS */

      RETURN
      END
