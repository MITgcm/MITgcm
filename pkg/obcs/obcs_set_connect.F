C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_set_connect.F,v 1.2 2014/11/25 15:26:10 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: OBCS_SET_CONNECT

C     !INTERFACE:
      SUBROUTINE OBCS_SET_CONNECT( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBCS_SET_CONNECT
C     | o Set OB connected piece Id for each level
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

C     !INPUT/OUTPUT PARAMETERS:
C     myThid   :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_OBCS
C     !LOCAL VARIABLES:
C     msgBuf   :: Informational/error message buffer
C     bi,bj    :: tile indices
C     i, j, k  :: Loop counters
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj
      INTEGER i, j, k
      INTEGER idN, idS, idE, idW
      INTEGER fp, prtMsg
      INTEGER n, newConnect, maxConnect
      INTEGER numConnect, listConnect(OBCS_maxConnect)
      INTEGER numLocal(nSx,nSy), listLocal(OBCS_maxConnect,nSx,nSy)
      INTEGER tmpConnect(sNx+sNx+sNy+sNy)
      _RS tmpXZ(1-OLx:sNx+OLx,Nr,nSx,nSy)
      _RS tmpYZ(1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL tmpRL

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('OBCS_SET_CONNECT',myThid)
#endif

C--   Initialise domain connected-piece Id for OB grid points:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        idN = 0
        idS = 0
        idE = 0
        idW = 0
        IF ( tileHasOBN(bi,bj) ) idN = 1
        IF ( tileHasOBS(bi,bj) ) idS = 1
        IF ( tileHasOBE(bi,bj) ) idE = 1
        IF ( tileHasOBW(bi,bj) ) idW = 1
        DO k=1,Nr
         DO i=1,sNx
          OBN_connect(i,k,bi,bj) = idN
          OBS_connect(i,k,bi,bj) = idS
         ENDDO
         DO j=1,sNy
          OBE_connect(j,k,bi,bj) = idE
          OBW_connect(j,k,bi,bj) = idW
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Read from files domain connected-piece Id for OB grid points:
C-    Note: current Section READ routines (MDS_READ_SEC_XZ,_YZ) are
C     single (Master) thread (output only available to Master Thread)
      prtMsg = 0
      fp = readBinaryPrec
      _BARRIER
      IF ( OBNconnectFile.NE.' ' ) THEN
        CALL READ_REC_XZ_RS( OBNconnectFile, fp,Nr, tmpXZ, 1,0,myThid )
        _BEGIN_MASTER(myThid)
        DO bj = 1,nSy
         DO bi = 1,nSx
          IF ( tileHasOBN(bi,bj) ) THEN
           DO k=1,Nr
            DO i=1,sNx
             OBN_connect(i,k,bi,bj) = NINT( tmpXZ(i,k,bi,bj) )
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        _END_MASTER(myThid)
        prtMsg = 1
      ENDIF
      IF ( OBSconnectFile.NE.' ' ) THEN
        CALL READ_REC_XZ_RS( OBSconnectFile, fp,Nr, tmpXZ, 1,0,myThid )
        _BEGIN_MASTER(myThid)
        DO bj = 1,nSy
         DO bi = 1,nSx
          IF ( tileHasOBS(bi,bj) ) THEN
           DO k=1,Nr
            DO i=1,sNx
             OBS_connect(i,k,bi,bj) = NINT( tmpXZ(i,k,bi,bj) )
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        _END_MASTER(myThid)
        prtMsg = 1
      ENDIF
      IF ( OBEconnectFile.NE.' ' ) THEN
        CALL READ_REC_YZ_RS( OBEconnectFile, fp,Nr, tmpYZ, 1,0,myThid )
        _BEGIN_MASTER(myThid)
        DO bj = 1,nSy
         DO bi = 1,nSx
          IF ( tileHasOBE(bi,bj) ) THEN
           DO k=1,Nr
            DO j=1,sNy
             OBE_connect(j,k,bi,bj) = NINT( tmpYZ(j,k,bi,bj) )
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        _END_MASTER(myThid)
        prtMsg = 1
      ENDIF
      IF ( OBWconnectFile.NE.' ' ) THEN
        CALL READ_REC_YZ_RS( OBWconnectFile, fp,Nr, tmpYZ, 1,0,myThid )
        _BEGIN_MASTER(myThid)
        DO bj = 1,nSy
         DO bi = 1,nSx
          IF ( tileHasOBW(bi,bj) ) THEN
           DO k=1,Nr
            DO j=1,sNy
             OBW_connect(j,k,bi,bj) = NINT( tmpYZ(j,k,bi,bj) )
            ENDDO
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        _END_MASTER(myThid)
        prtMsg = 1
      ENDIF
      _BARRIER

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO i=1,sNx
          IF (OB_Jn(i,bi,bj).EQ.OB_indexNone) OBN_connect(i,k,bi,bj)=0
          IF (OB_Js(i,bi,bj).EQ.OB_indexNone) OBS_connect(i,k,bi,bj)=0
         ENDDO
         DO j=1,sNy
          IF (OB_Ie(j,bi,bj).EQ.OB_indexNone) OBE_connect(j,k,bi,bj)=0
          IF (OB_Iw(j,bi,bj).EQ.OB_indexNone) OBW_connect(j,k,bi,bj)=0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C--   Count how many connected parts there are  for each level:
      prtMsg = prtMsg*debugLevel
      DO k=1,Nr

        maxConnect = 0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

C-    make a local copy
          DO i=1,sNx
            tmpConnect(i) = OBN_connect(i,k,bi,bj)
            tmpConnect(sNx+i) = OBS_connect(i,k,bi,bj)
          ENDDO
          DO j=1,sNy
            tmpConnect(sNx*2+j) = OBW_connect(j,k,bi,bj)
            tmpConnect(sNx*2+sNy+j) = OBE_connect(j,k,bi,bj)
          ENDDO

C-   make a list for each tile
          numLocal(bi,bj) = 0
          DO n=1,OBCS_maxConnect
            listLocal(n,bi,bj) = 0
          ENDDO
          newConnect = 1
          DO WHILE ( newConnect.NE. 0 )
           newConnect = 0
           DO i=1,(sNx+sNy)*2
            IF ( tmpConnect(i).GE.1 ) THEN
              IF ( newConnect.EQ.0 ) THEN
               newConnect = tmpConnect(i)
               numLocal(bi,bj) = numLocal(bi,bj) + 1
               IF ( numLocal(bi,bj).LE.OBCS_maxConnect )
     &           listLocal(numLocal(bi,bj),bi,bj) = newConnect
              ENDIF
              IF ( tmpConnect(i).EQ.newConnect )
     &          tmpConnect(i) = 0
            ENDIF
           ENDDO
          ENDDO
          IF ( numLocal(bi,bj).GT.OBCS_maxConnect ) THEN
            WRITE(msgBuf,'(A,3(A,I4),2(A,I10))') 'OBCS_SET_CONNECT: ',
     &       'k=', k, ' numLocal(', bi,',',bj,')=', numLocal(bi,bj),
     &       ' exceeds OBCS_maxConnect=', OBCS_maxConnect
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R OBCS_SET_CONNECT'
          ENDIF
          IF ( prtMsg.GE.debLevC ) THEN
           IF ( numLocal(bi,bj).EQ.0 ) THEN
            WRITE(msgBuf,'(A,2I4,A,I8)') 'OBCS_SET_CONNECT: bi,bj=',
     &       bi, bj, ' , numLocal=', numLocal(bi,bj)
           ELSE
            WRITE(msgBuf,'(A,2I4,2(A,I8))') 'OBCS_SET_CONNECT: bi,bj=',
     &       bi, bj, ' , numLocal=', numLocal(bi,bj),
     &           ' , listLocal:', listLocal(1,bi,bj)
           ENDIF
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
            DO j=2,numLocal(bi,bj),15
              n = MIN(numLocal(bi,bj),j+14)
              WRITE(msgBuf,'(A,15I8)')
     &            ' ... ', (listLocal(i,bi,bj),i=j,n)
              CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                            SQUEEZE_RIGHT, myThid )
            ENDDO
          ENDIF
          DO n=1,numLocal(bi,bj)
            maxConnect = MAX( maxConnect, listLocal(n,bi,bj) )
          ENDDO

         ENDDO
        ENDDO

        tmpRL = maxConnect
        _GLOBAL_MAX_RL( tmpRL, myThid )
        maxConnect = NINT( tmpRL )

C-   combine set of list (1 per tile) into 1 global list
        numConnect = 0
        DO j=1,maxConnect
          tmpRL = zeroRL
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO n=1,numLocal(bi,bj)
             IF ( listLocal(n,bi,bj).EQ.j ) tmpRL = oneRL
            ENDDO
           ENDDO
          ENDDO
          _GLOBAL_MAX_RL( tmpRL, myThid )
          IF ( tmpRL.EQ.oneRL ) THEN
            numConnect = numConnect + 1
            IF ( numConnect.LE.OBCS_maxConnect )
     &        listConnect(numConnect) = j
          ENDIF
        ENDDO
        IF ( numConnect.GT.OBCS_maxConnect ) THEN
          WRITE(msgBuf,'(A,I4,2(A,I10))') 'OBCS_SET_CONNECT: @ k=', k,
     &     ' numConnect=', numConnect,
     &     ' exceeds OBCS_maxConnect=', OBCS_maxConnect
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R OBCS_SET_CONNECT'
        ENDIF
        IF ( prtMsg.GE.debLevA ) THEN
          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(A,I4,2(A,I10))') 'OBCS_SET_CONNECT: @ k=', k,
     &     ', maxConnect=', maxConnect, ' , numConnect=', numConnect
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          DO j=1,numConnect,15
            n = MIN(numConnect,j+14)
            WRITE(msgBuf,'(A,15I8)')
     &          ' listConnect:', (listConnect(i),i=j,n)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )
          ENDDO
          _END_MASTER(myThid)
        ENDIF

C-   reset connected Id in order to use all number from 1 to numConnect
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)

C-    make a local copy
          DO i=1,sNx
            tmpConnect(i) = OBN_connect(i,k,bi,bj)
            tmpConnect(sNx+i) = OBS_connect(i,k,bi,bj)
          ENDDO
          DO j=1,sNy
            tmpConnect(sNx*2+j) = OBW_connect(j,k,bi,bj)
            tmpConnect(sNx*2+sNy+j) = OBE_connect(j,k,bi,bj)
          ENDDO
          DO n=1,numConnect
C-    change Id value: listConnect(n) to n
           IF ( listConnect(n).NE.n ) THEN
            DO i=1,(sNx+sNy)*2
             IF ( tmpConnect(i).EQ.listConnect(n) ) tmpConnect(i) = n
            ENDDO
           ENDIF
          ENDDO
C-    copy back into OB[N,S,E,W]_connect arrays
          DO i=1,sNx
            OBN_connect(i,k,bi,bj) = tmpConnect(i)
            OBS_connect(i,k,bi,bj) = tmpConnect(sNx+i)
          ENDDO
          DO j=1,sNy
            OBW_connect(j,k,bi,bj) = tmpConnect(sNx*2+j)
            OBE_connect(j,k,bi,bj) = tmpConnect(sNx*2+sNy+j)
          ENDDO

         ENDDO
        ENDDO

C-    store numConnect in common block
        _BEGIN_MASTER(myThid)
        OB_connectNumber(k) = numConnect
        _END_MASTER(myThid)

C--   end k loop
      ENDDO

      _BARRIER

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('OBCS_SET_CONNECT',myThid)
#endif

#endif /* ALLOW_OBCS */
      RETURN
      END
