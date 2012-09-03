C $Header: /u/gcmpack/MITgcm/eesupp/src/ini_threading_environment.F,v 1.14 2012/09/03 19:30:33 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"
#include "PACKAGES_CONFIG.h"

CBOP
C     !ROUTINE: INI_THREADING_ENVIRONMENT

C     !INTERFACE:
      SUBROUTINE INI_THREADING_ENVIRONMENT

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE INI\_THREADING\_ENVIRONMENT
C     | o Initialise multi-threaded environment.
C     *==========================================================*
C     | Generally we do not start separate threads here.
C     | The separate threads a spawned at later on.
C     | Here we perform initialisation of data-structures
C     | that indicate which of the nSx x nSy tiles a thread is
C     | responsible for.
C     | The multiple threads are spawned in the top level MAIN
C     | routine.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bXPerThread - Blocks of size sNx per thread.
C     byPerThread - Blocks of size sNy per thread.
C     thId        - Thread index. Temporary used in loops
C                   which set per. thread values on a
C                   cartesian grid.
C     bxLo, bxHi  - Work vars. for thread index
C     byLo, byHi    range. bxLo is the lowest i index
C                   that a thread covers, bxHi is the
C                   highest i index. byLo is the lowest
C                   j index, byHi is the highest j index.
C     I, J        - Loop counter
C     msgBuf      - I/O buffer for reporting status information.
C     myThid      - Dummy thread id for use in printed messages
C                   ( this routine "INI_THREADING_ENVIRONMENT" is
C                     called before multi-threading has started.)
      INTEGER bxPerThread
      INTEGER byPerThread
      INTEGER thId
      INTEGER bxLo, bxHi
      INTEGER byLo, byHi
      INTEGER I, J
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER myThid
#ifndef ALLOW_EXCH2
      LOGICAL flag
#endif
CEOP

C--   Set default for all threads of having no blocks to
C--   work on - except for thread 1.
      myBxLo(1) = 1
      myBxHi(1) = nSx
      myByLo(1) = 1
      myByHi(1) = nSy
      DO I = 2, MAX_NO_THREADS
       myBxLo(I) = 0
       myBxHi(I) = 0
       myByLo(I) = 0
       myByHi(I) = 0
      ENDDO
      myThid = 1
      commName(COMM_NONE) = 'none'
      commName(COMM_MSG ) = 'messages'
      commName(COMM_PUT ) = 'put'
      commName(COMM_GET ) = 'get'

C--   If there are multiple threads allocate different range of the
C--   nSx*nSy blocks to each thread.
C     For now handle simple case of no. blocks nSx = n*nTx and
C     no. blocks nSy = m*nTy ( where m and n are integer ). This
C     is handled by simply mapping threads to blocks in sequence
C     with the x thread index moving fastest.
C     Later code which sets the thread number of neighboring blocks
C     needs to be consistent with the code here.
      nThreads = nTx * nTy
      IF   ( nThreads .GT. MAX_NO_THREADS ) THEN
       WRITE(msgBuf,'(2A,2I6)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Total number of threads exceeds MAX_NO_THREADS',
     &   nTx*nTy, MAX_NO_THREADS
       CALL PRINT_ERROR(msgBuf, myThid)
       WRITE(msgBuf,'(2A)')
     &    ' Needs to increase MAX_NO_THREADS',
     &    ' in file "EEPARAMS.h" and to re-compile'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF

C--   Initialise the barrier mechanisms
C     BAR2 will eventually replace barrier everywhere.
      CALL BARRIER_INIT
      DO I=1, MAX_NO_THREADS
       CALL BAR2_INIT(I)
      ENDDO

C--   Initialise exchange mechanism
      CALL EXCH_INIT

      IF   ( nThreads .NE. nTx*nTy ) THEN
       WRITE(msgBuf,'(A,A,A,I5,A,I5)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Total number of threads is not the same as nTx*nTy.',
     &  ' nTx * nTy = ',nTx*nTy,' nThreads = ',nThreads
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      bxPerThread = nSx/nTx
      IF ( bxPerThread*nTx .NE. nSx ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Number of blocks in X (nSx)',
     &  ' must be exact multiple of threads in X (nTx).'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      byPerThread = nSy/nTy
      IF ( byPerThread*nTy .NE. nSy ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &  'S/R INI_THREADING_ENVIRONMENT:',
     &  ' Number of blocks in Y (nSy)',
     &  ' must be exact multiple of threads in Y (nTy).'
       CALL PRINT_ERROR(msgBuf, myThid)
       eeBootError = .TRUE.
       STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( .NOT. eeBootError ) THEN
       byLo = 1
       DO J=1,nTy
        byHi = byLo+byPerThread-1
        bxLo = 1
        DO I=1,nTx
         thId = (J-1)*nTx+I
         bxHi = bxLo+bxPerThread-1
         myBxLo(thId) = bxLo
         myBxHi(thId) = bxHi
         myByLo(thId) = byLo
         myByHi(thId) = byHi
         bxLo = bxHi+1
        ENDDO
        byLo = byHi+1
       ENDDO
      ENDIF

      DO thId=1,nThreads
       CALL INI_COMMUNICATION_PATTERNS( thId )
      ENDDO

C--   Print mapping of threads to grid points.
      WRITE(msgBuf,'(A)')
     &'// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Mapping of tiles to threads'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
C     o Write list of tiles each thread is responsible for
      WRITE(msgBuf,'(A)')
     &'// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      DO I=1,nThreads
       WRITE(msgBuf,'(A,I4,A,4(I4,A1))')
     & '// -o- Thread',I,', tiles (',
     & myBxLo(I),':',myBxHi(I),',',myByLo(I),':',myByHi(I),')'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_BOTH , 1)
      ENDDO
      WRITE(msgBuf,'(A)')  ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT , 1)

#ifndef ALLOW_EXCH2
C     o For each tile print its communication method(s)
      WRITE(msgBuf,'(A)')
     &'// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)') '// Tile <-> Tile connectvity table'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      WRITE(msgBuf,'(A)')
     &'// ======================================================'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &  SQUEEZE_RIGHT , 1)
      DO J=1,nSy
       DO I=1,nSx
        WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A)')
     &   '//',' Tile number: ',tileNo(I,J),
     &   ' (process no. = ',myPid,')'
        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT , 1)
C       o West communication details
        IF ( tileNoW(I,J).NE. NULL_TILE ) THEN
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
     &   '//        WEST: ',
     &   'Tile = ',tileNoW(I,J),
     &   ', Process = ',tilePidW(I,J),
     &   ', Comm = ',commName(tileCommModeW(I,J))
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
     &   '//              ',
     &   '  bi = ',tileBiW(I,J),
     &   ', bj = ',tileBjW(I,J)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ELSE
         WRITE(msgBuf,'(A)')
     &   '//         WEST: no neighbor'
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ENDIF
C       o East communication details
        IF ( tileNoE(I,J).NE. NULL_TILE ) THEN
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
     &   '//        EAST: ',
     &   'Tile = ',tileNoE(I,J),
     &   ', Process = ',tilePidE(I,J),
     &   ', Comm = ',commName(tileCommModeE(I,J))
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
     &   '//              ',
     &   '  bi = ',tileBiE(I,J),
     &   ', bj = ',tileBjE(I,J)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ELSE
         WRITE(msgBuf,'(A)')
     &   '//         EAST: no neighbor'
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ENDIF
C       o South communication method
        IF ( tileNoS(I,J).NE. NULL_TILE ) THEN
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
     &   '//       SOUTH: ',
     &   'Tile = ',tileNoS(I,J),
     &   ', Process = ',tilePidS(I,J),
     &   ', Comm = ',commName(tileCommModeS(I,J))
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
     &   '//              ',
     &   '  bi = ',tileBiS(I,J),
     &   ', bj = ',tileBjS(I,J)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ELSE
         WRITE(msgBuf,'(A)')
     &   '//        SOUTH: no neighbor'
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ENDIF
C       o North communication method
        IF ( tileNoN(I,J).NE. NULL_TILE ) THEN
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6,A,A)')
     &   '//       NORTH: ',
     &   'Tile = ',tileNoN(I,J),
     &   ', Process = ',tilePidN(I,J),
     &   ', Comm = ',commName(tileCommModeN(I,J))
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
         WRITE(msgBuf,'(A,A,I6.6,A,I6.6)')
     &   '//              ',
     &   '  bi = ',tileBiN(I,J),
     &   ', bj = ',tileBjN(I,J)
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ELSE
         WRITE(msgBuf,'(A)')
     &   '//        NORTH: no neighbor'
         CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
        ENDIF
       ENDDO
      ENDDO
      WRITE(msgBuf,'(A)')  ' '
      CALL PRINT_MESSAGE( msgBuf,standardMessageUnit,SQUEEZE_RIGHT, 1)
#endif /* ndef ALLOW_EXCH2 */

C--   Check EXCH-1 options
#ifndef ALLOW_EXCH2
      IF ( usingMPI .AND. useCubedSphereExchange ) THEN
C-    not working with multi-procs (checked within EXCH1-CUBE S/R) and
C-    if compiled with MPI (without EXCH2) safer to set usingMPI to False.
        WRITE(msgBuf,'(2A)') 'EXCH-1 useCubedSphereExchange',
     &                       ' unsafe with usingMPI=True'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( nThreads.GT.1 .AND. useCubedSphereExchange ) THEN
C-    multi-threads not working for local arrays; could remove the stop if
C     we are sure that only shared array (=in common blocks) are exchanged.
        WRITE(msgBuf,'(2A)') 'EXCH-1 useCubedSphereExchange',
     &                       ' unsafe with multi-threads'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
      ENDIF
      IF ( nThreads.GT.1 ) THEN
       flag = .FALSE.
       DO J=1,nSy
        DO I=1,nSx
          flag = flag
     &       .OR. tileCommModeW(I,J).EQ.COMM_GET
     &       .OR. tileCommModeE(I,J).EQ.COMM_GET
     &       .OR. tileCommModeS(I,J).EQ.COMM_GET
     &       .OR. tileCommModeN(I,J).EQ.COMM_GET
        ENDDO
       ENDDO
       IF ( flag ) THEN
C-    multi-threads not working for local arrays; not safe neither for shared arrays
        WRITE(msgBuf,'(3A)') 'EXCH-1 using Comm = ',
     &   commName(COMM_GET), ' unsafe with multi-threads'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R INI_THREADING_ENVIRONMENT'
       ENDIF
      ENDIF
#endif /* ndef ALLOW_EXCH2 */

      RETURN
      END
