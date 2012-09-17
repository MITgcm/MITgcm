C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_balance_flow.F,v 1.7 2012/09/17 22:03:18 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_BALANCE_FLOW

C     !INTERFACE:
      SUBROUTINE OBCS_BALANCE_FLOW( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_BALANCE_FLOW
C     | o Modify OB normal flow to ensure no net inflow
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
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
#ifdef ALLOW_OBCS_BALANCE

C     !FUNCTIONS:

C     !LOCAL VARIABLES:
C     bi, bj       :: tile indices
C     i,j,k        :: loop indices
C     iB, jB       :: local index of open boundary
C     msgBuf       :: Informational/error message buffer
      INTEGER bi, bj
      INTEGER i, j, k, iB, jB
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL areaOB, areaE, areaW, areaN, areaS, tmpA
      _RL inFlow, flowE, flowW, flowN, flowS
      _RL tileArea(nSx,nSy)
      _RL tileFlow(nSx,nSy)
      _RL tileAreaOB(nSx,nSy)
      _RL tileInFlow(nSx,nSy)
      LOGICAL flag

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Old method (OBCS_balanceFac < 0): balance each OB separately
C-- New method applies to all OB with BCS_balanceFac >=0 :
C      ensure that the net inflow through all OB is balanced by correcting
C      each OB normal flow with a uniform velocity, using the corresponding
C      weight factor OBCS_balanceFac.
C  e.g., OBCS_balanceFac_E,W,N,S= 1, -1, 2, 0 :
C      => correct Western OBWu (by substracting a uniform velocity) to ensure
C         zero net transport through Western OB
C      => correct Eastern and Northern normal flow (twice larger Northern
C         velocity correction than Eastern correction) to ensure that
C         the total inflow through E+N+S OB is balanced

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_BALANCE_FLOW',myThid)
#endif

C--   Integrate the transport through each OB
      flag = .FALSE.
      areaOB = 0. _d 0
      inFlow = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileAreaOB(bi,bj) = 0.
        tileInFlow(bi,bj) = 0.
       ENDDO
      ENDDO

#ifdef ALLOW_OBCS_EAST
      areaE = 0. _d 0
      flowE = 0. _d 0
      flag = flag .OR. ( OBCS_balanceFacE.GT.0. )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBE(bi,bj) ) THEN
         DO k=1,Nr
          DO j=1,sNy
           iB = OB_Ie(j,bi,bj)
C-    If 1 OB location is on 2 tiles (@ edge of 2 tiles), select the one which
C     communicates with tile interior (sNx+1) rather than with halo region (i=1)
           IF ( iB.NE.OB_indexNone .AND. iB.GT.1 ) THEN
            tmpA = drF(k)*hFacW(iB,j,k,bi,bj)*dyG(iB,j,bi,bj)
     &                   *maskInW(iB,j,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + tmpA*OBEu(j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         IF ( OBCS_balanceFacE.GE.0. ) THEN
           tileInFlow(bi,bj) = tileInFlow(bi,bj) - tileFlow(bi,bj)
           tileAreaOB(bi,bj) = tileAreaOB(bi,bj)
     &                       + tileArea(bi,bj)*OBCS_balanceFacE
         ENDIF
         areaE = areaE + tileArea(bi,bj)
         flowE = flowE + tileFlow(bi,bj)
        ENDIF
       ENDDO
      ENDDO
c     WRITE(standardMessageUnit,'(A,I9,1P2E16.8)')
c    &   'OBCS_balance it,areaE,flowE=', myIter, areaE, flowE
      IF ( OBCS_balanceFacE.LT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileArea, areaE, myThid )
        IF ( areaE.GT.0. ) THEN
          CALL GLOBAL_SUM_TILE_RL( tileFlow, flowE, myThid )
          IF ( debugLevel.GE.debLevC ) THEN
            WRITE(msgBuf,'(A,I9,A,1P2E16.8)') 'OBCS_balance (it=',
     &            myIter, ' ) correct OBEu:', flowE, -flowE/areaE
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDIF
          flowE = -flowE/areaE
        ENDIF
      ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      areaW = 0. _d 0
      flowW = 0. _d 0
      flag = flag .OR. ( OBCS_balanceFacW.GT.0. )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBW(bi,bj) ) THEN
         DO k=1,Nr
          DO j=1,sNy
           iB = OB_Iw(j,bi,bj)
C-    If 1 OB location is on 2 tiles (@ edge of 2 tiles), select the one which
C     communicates with tile interior (i=0) rather than with halo region (i=sNx)
           IF ( iB.NE.OB_indexNone .AND. iB.LT.sNx ) THEN
            tmpA = drF(k)*hFacW(1+iB,j,k,bi,bj)*dyG(1+iB,j,bi,bj)
     &                   *maskInW(1+iB,j,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + tmpA*OBWu(j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         IF ( OBCS_balanceFacW.GE.0. ) THEN
           tileInFlow(bi,bj) = tileInFlow(bi,bj) + tileFlow(bi,bj)
           tileAreaOB(bi,bj) = tileAreaOB(bi,bj)
     &                       + tileArea(bi,bj)*OBCS_balanceFacW
         ENDIF
         areaW = areaW + tileArea(bi,bj)
         flowW = flowW + tileFlow(bi,bj)
        ENDIF
       ENDDO
      ENDDO
c     WRITE(standardMessageUnit,'(A,I9,1P2E16.8)')
c    &   'OBCS_balance it,areaW,flowW=', myIter, areaW, flowW
      IF ( OBCS_balanceFacW.LT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileArea, areaW, myThid )
        IF ( areaW.GT.0. ) THEN
          CALL GLOBAL_SUM_TILE_RL( tileFlow, flowW, myThid )
          IF ( debugLevel.GE.debLevC ) THEN
            WRITE(msgBuf,'(A,I9,A,1P2E16.8)') 'OBCS_balance (it=',
     &            myIter, ' ) correct OBWu:', flowW, -flowW/areaW
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDIF
          flowW = -flowW/areaW
        ENDIF
      ENDIF
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
      areaN = 0. _d 0
      flowN = 0. _d 0
      flag = flag .OR. ( OBCS_balanceFacN.GT.0. )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBN(bi,bj) ) THEN
         DO k=1,Nr
          DO i=1,sNx
           jB = OB_Jn(i,bi,bj)
C-    If 1 OB location is on 2 tiles (@ edge of 2 tiles), select the one which
C     communicates with tile interior (sNy+1) rather than with halo region (j=1)
           IF ( jB.NE.OB_indexNone .AND. jB.GT.1 ) THEN
            tmpA = drF(k)*hFacS(i,jB,k,bi,bj)*dxG(i,jB,bi,bj)
     &                   *maskInS(i,jB,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + tmpA*OBNv(i,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         IF ( OBCS_balanceFacN.GE.0. ) THEN
           tileInFlow(bi,bj) = tileInFlow(bi,bj) - tileFlow(bi,bj)
           tileAreaOB(bi,bj) = tileAreaOB(bi,bj)
     &                       + tileArea(bi,bj)*OBCS_balanceFacN
         ENDIF
         areaN = areaN + tileArea(bi,bj)
         flowN = flowN + tileFlow(bi,bj)
        ENDIF
       ENDDO
      ENDDO
c     WRITE(standardMessageUnit,'(A,I9,1P2E16.8)')
c    &   'OBCS_balance it,areaN,flowN=', myIter, areaN, flowN
      IF ( OBCS_balanceFacN.LT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileArea, areaN, myThid )
        IF ( areaN.GT.0. ) THEN
          CALL GLOBAL_SUM_TILE_RL( tileFlow, flowN, myThid )
          IF ( debugLevel.GE.debLevC ) THEN
            WRITE(msgBuf,'(A,I9,A,1P2E16.8)') 'OBCS_balance (it=',
     &            myIter, ' ) correct OBNv:', flowN, -flowN/areaN
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDIF
          flowN = -flowN/areaN
        ENDIF
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      areaS = 0. _d 0
      flowS = 0. _d 0
      flag = flag .OR. ( OBCS_balanceFacS.GT.0. )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        tileArea(bi,bj) = 0.
        tileFlow(bi,bj) = 0.
        IF ( tileHasOBS(bi,bj) ) THEN
         DO k=1,Nr
          DO i=1,sNx
           jB = OB_Js(i,bi,bj)
C-    If 1 OB location is on 2 tiles (@ edge of 2 tiles), select the one which
C     communicates with tile interior (j=0) rather than with halo region (j=sNy)
           IF ( jB.NE.OB_indexNone .AND. jB.LT.sNy ) THEN
            tmpA = drF(k)*hFacS(i,1+jB,k,bi,bj)*dxG(i,1+jB,bi,bj)
     &                   *maskInS(i,1+jB,bi,bj)
            tileArea(bi,bj) = tileArea(bi,bj) + tmpA
            tileFlow(bi,bj) = tileFlow(bi,bj) + tmpA*OBSv(i,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
         IF ( OBCS_balanceFacS.GE.0. ) THEN
           tileInFlow(bi,bj) = tileInFlow(bi,bj) + tileFlow(bi,bj)
           tileAreaOB(bi,bj) = tileAreaOB(bi,bj)
     &                       + tileArea(bi,bj)*OBCS_balanceFacS
         ENDIF
         areaS = areaS + tileArea(bi,bj)
         flowS = flowS + tileFlow(bi,bj)
        ENDIF
       ENDDO
      ENDDO
c     WRITE(standardMessageUnit,'(A,I9,1P2E16.8)')
c    &   'OBCS_balance it,areaS,flowS=', myIter, areaS, flowS
      IF ( OBCS_balanceFacS.LT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileArea, areaS, myThid )
        IF ( areaS.GT.0. ) THEN
          CALL GLOBAL_SUM_TILE_RL( tileFlow, flowS, myThid )
          IF ( debugLevel.GE.debLevC ) THEN
            WRITE(msgBuf,'(A,I9,A,1P2E16.8)') 'OBCS_balance (it=',
     &            myIter, ' ) correct OBSv:', flowS, -flowS/areaS
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDIF
          flowS = -flowS/areaS
        ENDIF
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Calculate a unique velocity correction for all (OBCS_balanceFac>0) OB
C     and correct each OB using corresponding weight factor OBCS_balanceFac

      IF ( flag ) CALL GLOBAL_SUM_TILE_RL( tileAreaOB, areaOB, myThid )
      IF ( areaOB.GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileInFlow, inFlow, myThid )
        IF ( debugLevel.GE.debLevC ) THEN
          WRITE(msgBuf,'(A,I9,A,1P2E16.8)') 'OBCS_balance (it=',
     &       myIter, ' ) correct for inFlow:', inFlow, inFlow/areaOB
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
        inFlow = inFlow / areaOB
      ENDIF
      IF ( OBCS_balanceFacE.GE.0. ) flowE =  inFlow*OBCS_balanceFacE
      IF ( OBCS_balanceFacW.GE.0. ) flowW = -inFlow*OBCS_balanceFacW
      IF ( OBCS_balanceFacN.GE.0. ) flowN =  inFlow*OBCS_balanceFacN
      IF ( OBCS_balanceFacS.GE.0. ) flowS = -inFlow*OBCS_balanceFacS

      IF ( debugLevel.GE.debLevC  .AND. areaOB.GT.0. ) THEN
          WRITE(msgBuf,'(A,1P2E16.8)')
     &     'OBCS_balance correction to OBEu,OBWu:', flowE, flowW
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(A,1P2E16.8)')
     &     'OBCS_balance correction to OBNv,OBSv:', flowN, flowS
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
      ENDIF

c     IF ( .NOT.useOBCSbalance ) RETURN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Add correction:

#ifdef ALLOW_OBCS_EAST
      IF ( OBCS_balanceFacE.NE.0. ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( tileHasOBE(bi,bj) ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            iB = OB_Ie(j,bi,bj)
            IF ( iB.NE.OB_indexNone ) THEN
             OBEu(j,k,bi,bj) = OBEu(j,k,bi,bj)
     &                       + flowE*maskW(iB,j,k,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      IF ( OBCS_balanceFacW.NE.0. ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( tileHasOBW(bi,bj) ) THEN
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            iB = OB_Iw(j,bi,bj)
            IF ( iB.NE.OB_indexNone ) THEN
             OBWu(j,k,bi,bj) = OBWu(j,k,bi,bj)
     &                       + flowW*maskW(1+iB,j,k,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
      IF ( OBCS_balanceFacW.NE.0. ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( tileHasOBN(bi,bj) ) THEN
          DO k=1,Nr
           DO i=1-OLx,sNx+OLx
            jB = OB_Jn(i,bi,bj)
            IF ( jB.NE.OB_indexNone ) THEN
             OBNv(i,k,bi,bj) = OBNv(i,k,bi,bj)
     &                       + flowN*maskS(i,jB,k,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      IF ( OBCS_balanceFacS.NE.0. ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         IF ( tileHasOBS(bi,bj) ) THEN
          DO k=1,Nr
           DO i=1-OLx,sNx+OLx
            jB = OB_Js(i,bi,bj)
            IF ( jB.NE.OB_indexNone ) THEN
             OBSv(i,k,bi,bj) = OBSv(i,k,bi,bj)
     &                       + flowS*maskS(i,1+jB,k,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDIF
        ENDDO
       ENDDO
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_BALANCE_FLOW',myThid)
#endif

#endif /* ALLOW_OBCS_BALANCE */
#endif /* ALLOW_OBCS */

      RETURN
      END
