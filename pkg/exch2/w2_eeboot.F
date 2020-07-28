#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: W2_EEBOOT

C     !INTERFACE:
      SUBROUTINE W2_EEBOOT

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE W2_EEBOOT
C     | o Setup execution "environment" for WRAPPER2
C     *==========================================================*
C     | WRAPPER2 provides complex topology support. In this routine
C     | we setup the base topology for the default halo operations.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_FNAM) fName
      INTEGER stdUnit, iLen
      INTEGER myThid
      INTEGER ii, I, J
      CHARACTER*(10) fmtStr
      INTEGER iTmp

C     Set dummy myThid value (we are not multi-threaded here)
      myThid = 1

C     Initialise to zero EXCH2_TOPOLOGY common blocks
      exch2_nTiles = 0
      DO I = 1,W2_maxNbTiles
        exch2_tNx(I)    = 0
        exch2_tNy(I)    = 0
        exch2_tBasex(I) = 0
        exch2_tBasey(I) = 0
        exch2_txGlobalo(I) = 0
        exch2_tyGlobalo(I) = 0
        exch2_isWedge(I) = 0
        exch2_isNedge(I) = 0
        exch2_isEedge(I) = 0
        exch2_isSedge(I) = 0
        exch2_myFace(I)  = 0
        exch2_mydNx(I)   = 0
        exch2_mydNy(I)   = 0
        exch2_nNeighbours(I) = 0
        DO J = 1,W2_maxNeighbours
          exch2_neighbourId(J,I)  = 0
          exch2_opposingSend(J,I) = 0
          DO ii = 1,4
           exch2_pij(ii,J,I) = 0
          ENDDO
          exch2_oi(J,I)  = 0
          exch2_oj(J,I)  = 0
          exch2_iLo(J,I) = 0
          exch2_iHi(J,I) = 0
          exch2_jLo(J,I) = 0
          exch2_jHi(J,I) = 0
        ENDDO
      ENDDO
      W2_oUnit = standardMessageUnit

C     Set W2-EXCH2 parameters
      CALL W2_READPARMS( myThid )

      stdUnit = standardMessageUnit
      WRITE(msgBuf,'(A)') '===== Start setting W2 TOPOLOGY:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )

C     Open message output-file (if needed)
      IF ( W2_printMsg .LT. 0 ) THEN
        iTmp = MAX(4,1 + INT(LOG10(DFLOAT(nPx*nPy))))
        WRITE(fmtStr,'(2(A,I1),A)') '(A,I',iTmp,'.',iTmp,',A)'
        WRITE(fName,fmtStr) 'w2_tile_topology.',myProcId,'.log'
        iLen = ILNBLNK(fName)
        CALL MDSFINDUNIT( W2_oUnit, myThid )
        OPEN( W2_oUnit, file=fName(1:iLen),
     &                  status='unknown', form='formatted')
        WRITE(msgBuf,'(2A)') ' write to log-file: ', fName(1:iLen)
        CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

C     Define topology for every tile
      CALL W2_E2SETUP( myThid )

C--   Decide which tiles this process handles;
C     fill also W2_procTileList for Single-CPU-IO and check also tile-size;
C     print tiles connection for this process and set myCommonFlag :
      CALL W2_MAP_PROCS( myThid )

C     Print out the topology communication schedule
      IF ( W2_printMsg .NE. 0 ) THEN
        CALL W2_PRINT_COMM_SEQUENCE( myThid )
      ENDIF

C     Close message output-file (if needed)
      IF ( W2_oUnit.NE.standardMessageUnit ) THEN
        WRITE(msgBuf,'(A)') '===  End TOPOLOGY report ==='
        CALL PRINT_MESSAGE( msgBuf, W2_oUnit, SQUEEZE_BOTH, myThid )
        CLOSE( W2_oUnit )
      ENDIF
      WRITE(msgBuf,'(A)') '=====       setting W2 TOPOLOGY: Done'
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, stdUnit, SQUEEZE_RIGHT, myThid )

      RETURN
      END
