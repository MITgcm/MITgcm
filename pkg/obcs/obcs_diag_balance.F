C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_diag_balance.F,v 1.6 2014/12/19 05:41:20 jmc Exp $
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
      INTEGER bi, bj, n
      INTEGER i, j, iB, jB, iBt, jBt
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL areaOB(OBCS_maxConnect), tmpA
      _RL inFlow(OBCS_maxConnect)
      _RL tileAreaOB(nSx,nSy,OBCS_maxConnect)
      _RL tileInFlow(nSx,nSy,OBCS_maxConnect)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_DIAG_BALANCE',myThid)
#endif

C--   Integrate the transport through each OB
      DO n=1,OB_connectNumber(k)
        areaOB(n)= 0. _d 0
        inFlow(n)= 0. _d 0
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          tileAreaOB(bi,bj,n) = 0.
          tileInFlow(bi,bj,n) = 0.
         ENDDO
        ENDDO
      ENDDO

#ifdef ALLOW_OBCS_EAST
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( tileHasOBE(bi,bj) ) THEN
         DO j=1,sNy
           iB = OB_Ie(j,bi,bj)
           IF ( iB.NE.OB_indexNone .AND. iB.GT.1 ) THEN
            n = OBE_connect(j,k,bi,bj)
            IF ( n.GE.1 ) THEN
              tmpA = drF(k)*hFacW(iB,j,k,bi,bj)*dyG(iB,j,bi,bj)
     &                     *maskInW(iB,j,bi,bj)
              tileAreaOB(bi,bj,n) = tileAreaOB(bi,bj,n) + tmpA
              tileInFlow(bi,bj,n) = tileInFlow(bi,bj,n)
     &                            - uTrans(iB,j,bi,bj)
            ENDIF
           ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( tileHasOBW(bi,bj) ) THEN
         DO j=1,sNy
           iB = OB_Iw(j,bi,bj)
           IF ( iB.NE.OB_indexNone .AND. iB.LT.sNx ) THEN
            n = OBW_connect(j,k,bi,bj)
            IF ( n.GE.1 ) THEN
              iB = 1+iB
              tmpA = drF(k)*hFacW(iB,j,k,bi,bj)*dyG(iB,j,bi,bj)
     &                     *maskInW(iB,j,bi,bj)
              tileAreaOB(bi,bj,n) = tileAreaOB(bi,bj,n) + tmpA
              tileInFlow(bi,bj,n) = tileInFlow(bi,bj,n)
     &                            + uTrans(iB,j,bi,bj)
            ENDIF
           ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( tileHasOBN(bi,bj) ) THEN
         DO i=1,sNx
           jB = OB_Jn(i,bi,bj)
           IF ( jB.NE.OB_indexNone .AND. jB.GT.1 ) THEN
            n = OBN_connect(i,k,bi,bj)
            IF ( n.GE.1 ) THEN
              tmpA = drF(k)*hFacS(i,jB,k,bi,bj)*dxG(i,jB,bi,bj)
     &                     *maskInS(i,jB,bi,bj)
              tileAreaOB(bi,bj,n) = tileAreaOB(bi,bj,n) + tmpA
              tileInFlow(bi,bj,n) = tileInFlow(bi,bj,n)
     &                            - vTrans(i,jB,bi,bj)
            ENDIF
           ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( tileHasOBS(bi,bj) ) THEN
         DO i=1,sNx
           jB = OB_Js(i,bi,bj)
           IF ( jB.NE.OB_indexNone .AND. jB.LT.sNy ) THEN
            n = OBS_connect(i,k,bi,bj)
            IF ( n.GE.1 ) THEN
              jB = 1+jB
              tmpA = drF(k)*hFacS(i,jB,k,bi,bj)*dxG(i,jB,bi,bj)
     &                     *maskInS(i,jB,bi,bj)
              tileAreaOB(bi,bj,n) = tileAreaOB(bi,bj,n) + tmpA
              tileInFlow(bi,bj,n) = tileInFlow(bi,bj,n)
     &                            + vTrans(i,jB,bi,bj)
            ENDIF
           ENDIF
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_OBCS_SOUTH */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   For each set of OB connected points, calculate a unique velocity
C     correction and correct to the corresponding OB point divergence

      DO n=1,OB_connectNumber(k)
       CALL GLOBAL_SUM_TILE_RL( tileAreaOB(1,1,n), areaOB(n), myThid )
       IF ( areaOB(n).GT.0. ) THEN
        CALL GLOBAL_SUM_TILE_RL( tileInFlow(1,1,n), inFlow(n), myThid )
        IF ( debugLevel.GE.debLevC ) THEN
          WRITE(msgBuf,'(A,I9,2I4,A,1P2E16.8)')
     &     'OBCS_DIAG_BALANCE (it,k,n=', myIter, k, n,
     &       ' ) inFlow:',inFlow(n),inFlow(n)/areaOB(n)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
        ENDIF
        inFlow(n) = inFlow(n) / areaOB(n)
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Add correction:

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef ALLOW_OBCS_EAST
         IF ( tileHasOBE(bi,bj) ) THEN
c          DO j=1-OLy,sNy+OLy
           DO j=1,sNy
            IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
             iB = OB_Ie(j,bi,bj)
             n = OBE_connect(j,k,bi,bj)
             IF ( n.EQ.0 ) THEN
               div2d(iB ,j,bi,bj) = div2d(iB ,j,bi,bj)
     &                            - uTrans(iB,j,bi,bj)
             ELSE
               div2d(iB ,j,bi,bj) = div2d(iB,j,bi,bj)
     &                 + inFlow(n)*drF(k)*hFacW(iB,j,k,bi,bj)
     &                            *dyG(iB,j,bi,bj)*maskInW(iB,j,bi,bj)
             ENDIF
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_EAST */

#ifdef ALLOW_OBCS_WEST
         IF ( tileHasOBW(bi,bj) ) THEN
c          DO j=1-OLy,sNy+OLy
           DO j=1,sNy
            IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
             iBt = OB_Iw(j,bi,bj)
             iB = 1+iBt
             n = OBW_connect(j,k,bi,bj)
             IF ( n.EQ.0 ) THEN
               div2d(iBt,j,bi,bj) = div2d(iBt,j,bi,bj)
     &                            + uTrans(iB,j,bi,bj)
             ELSE
               div2d(iBt,j,bi,bj) = div2d(iBt,j,bi,bj)
     &                 + inFlow(n)*drF(k)*hFacW(iB,j,k,bi,bj)
     &                            *dyG(iB,j,bi,bj)*maskInW(iB,j,bi,bj)
             ENDIF
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_WEST */

#ifdef ALLOW_OBCS_NORTH
         IF ( tileHasOBN(bi,bj) ) THEN
c          DO i=1-OLx,sNx+OLx
           DO i=1,sNx
            IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
             jB = OB_Jn(i,bi,bj)
             n = OBN_connect(i,k,bi,bj)
             IF ( n.EQ.0 ) THEN
               div2d(i,jB ,bi,bj) = div2d(i,jB ,bi,bj)
     &                            - vTrans(i,jB,bi,bj)
             ELSE
               div2d(i,jB ,bi,bj) = div2d(i,jB ,bi,bj)
     &                 + inFlow(n)*drF(k)*hFacS(i,jB,k,bi,bj)
     &                            *dxG(i,jB,bi,bj)*maskInS(i,jB,bi,bj)
             ENDIF
            ENDIF
           ENDDO
         ENDIF
#endif /* ALLOW_OBCS_NORTH */

#ifdef ALLOW_OBCS_SOUTH
         IF ( tileHasOBS(bi,bj) ) THEN
c          DO i=1-OLx,sNx+OLx
           DO i=1,sNx
            IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
             jBt = OB_Js(i,bi,bj)
             jB = 1+jBt
             n = OBS_connect(i,k,bi,bj)
             IF ( n.EQ.0 ) THEN
               div2d(i,jBt,bi,bj) = div2d(i,jBt,bi,bj)
     &                            + vTrans(i,jB,bi,bj)
             ELSE
               div2d(i,jBt,bi,bj) = div2d(i,jBt,bi,bj)
     &                 + inFlow(n)*drF(k)*hFacS(i,jB,k,bi,bj)
     &                            *dxG(i,jB,bi,bj)*maskInS(i,jB,bi,bj)
             ENDIF
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
