C $Header: /u/gcmpack/MITgcm/eesupp/inc/EXCH.h,v 1.7 2010/03/16 00:02:33 jmc Exp $
C $Name:  $
C
CBOP
C     !ROUTINE: EXCH.h
C     !INTERFACE:
C     include "EXCH.h"
C
C     !DESCRIPTION:
C     *==========================================================*
C     | EXCH.h
C     *==========================================================*
C     | Support data structures for
C     | the MITgcm-UV "exchange routines" code. This data should
C     | be private to the execution environment routines.
C     *==========================================================*
CEOP

#ifndef _RL
#define _RL Real*8
#endif

#ifndef _RS
#define _RS Real*4
#endif

#ifndef _R8
#define _R8 Real*8
#endif

#ifndef _R4
#define _R4 Real*4
#endif

#ifndef _tileCommModeW
#define _tileCommModeW(a,b) tileCommModeW(a,b)
#endif

#ifndef _tileCommModeE
#define _tileCommModeE(a,b) tileCommModeE(a,b)
#endif

#ifndef _tileBiE
#define _tileBiE(a,b) tileBiE(a,b)
#endif

#ifndef _tileBiW
#define _tileBiW(a,b) tileBiW(a,b)
#endif

#ifndef _tileBjE
#define _tileBjE(a,b) tileBjE(a,b)
#endif

#ifndef _tileBjW
#define _tileBjW(a,b) tileBjW(a,b)
#endif

#ifndef _mpiPidW
#define _mpiPidW(a,b) pidW(a,b)
#endif

#ifndef _mpiIdE
#define _mpiIdE(a,b) pidE(a,b)
#endif

#ifndef _tileTagSendW
#define _tileTagSendW(a,b) tileTagSendW(a,b)
#endif

#ifndef _tileTagSendE
#define _tileTagSendE(a,b) tileTagSendE(a,b)
#endif

#ifndef _tileTagRecvW
#define _tileTagRecvW(a,b) tileTagRecvW(a,b)
#endif

#ifndef _tileTagRecvE
#define _tileTagRecvE(a,b) tileTagRecvE(a,b)
#endif

#ifndef _tileCommModeS
#define _tileCommModeS(a,b) tileCommModeS(a,b)
#endif

#ifndef _tileCommModeN
#define _tileCommModeN(a,b) tileCommModeN(a,b)
#endif

#ifndef _tileBiN
#define _tileBiN(a,b) tileBiN(a,b)
#endif

#ifndef _tileBiS
#define _tileBiS(a,b) tileBiS(a,b)
#endif

#ifndef _tileBjN
#define _tileBjN(a,b) tileBjN(a,b)
#endif

#ifndef _tileBjS
#define _tileBjS(a,b) tileBjS(a,b)
#endif

#ifndef _mpiPidS
#define _mpiPidS(a,b) pidS(a,b)
#endif

#ifndef _mpiIdN
#define _mpiIdN(a,b) pidN(a,b)
#endif

#ifndef _tileTagSendS
#define _tileTagSendS(a,b) tileTagSendS(a,b)
#endif

#ifndef _tileTagSendN
#define _tileTagSendN(a,b) tileTagSendN(a,b)
#endif

#ifndef _tileTagRecvS
#define _tileTagRecvS(a,b) tileTagRecvS(a,b)
#endif

#ifndef _tileTagRecvN
#define _tileTagRecvN(a,b) tileTagRecvN(a,b)
#endif

#ifndef _theSimulationMode
#define _theSimulationMode theSimulationMode
#endif

#ifndef _EXCH_SPIN_LIMIT
#define _EXCH_SPIN_LIMIT EXCH_SPIN_LIMIT
#endif

C      MAX_OLX_EXCH - Maximum overlap region allowed in X
C      MAX_OLY_EXCH - Maximum overlap region allowed in Y
C      MAX_NR_EXCH  - Maximum number of vertical levels allowed
C      NUMBER_OF_BUFFER_LEVELS - Number of levels of buffer allowed.
C      EXCH_SPIN_LIMIT - Error trapping threshold for deadlocked exchange
       INTEGER MAX_OLX_EXCH
       PARAMETER ( MAX_OLX_EXCH = MAX_OLX )
       INTEGER MAX_OLY_EXCH
       PARAMETER ( MAX_OLY_EXCH = MAX_OLY )
       INTEGER MAX_NR_EXCH
       PARAMETER ( MAX_NR_EXCH  = nR + 1 )
       INTEGER NUMBER_OF_BUFFER_LEVELS
       PARAMETER ( NUMBER_OF_BUFFER_LEVELS = 1 )
       INTEGER EXCH_SPIN_LIMIT
       PARAMETER ( EXCH_SPIN_LIMIT = 100000000 )

C
C      L_BUFFER[XY]  - Maximum size for exchange buffer in
C      L_WBUFFER    west,
C      L_EBUFFER    east,
C      L_SBUFFER   south,
C      L_NBUFFER   north.
       INTEGER L_BUFFERX
       PARAMETER ( L_BUFFERX =
     &  (sNy+2*MAX_OLY_EXCH)*MAX_OLX_EXCH*MAX_NR_EXCH )
       INTEGER L_BUFFERY
       PARAMETER ( L_BUFFERY =
     &  (sNx+2*MAX_OLX_EXCH)*MAX_OLY_EXCH*MAX_NR_EXCH )
       INTEGER L_WBUFFER
       INTEGER L_EBUFFER
       INTEGER L_SBUFFER
       INTEGER L_NBUFFER
       PARAMETER ( L_WBUFFER = L_BUFFERX,
     &             L_EBUFFER = L_BUFFERX,
     &             L_SBUFFER = L_BUFFERY,
     &             L_NBUFFER = L_BUFFERY )

C--    COMMON / EXCH_L / LOGICAL number common arrays for exchanges
C      exchNeedsMemSync - TRUE if memory sync. required to ensure
C                         memory consistency during exchange
C      exchUsesBarrier  - TRUE if we use a call to BAR to do sync.
C                         between processes. On some machines we wont
C                         spin on the Ack setting ( the T90 ),
C                         instead we will use s system barrier.
C                         On the T90 the system barrier is very fast and
C                         switches out the thread while it waits. On most
C                         machines the system barrier is much too slow and if
C                         we own the machine and have one thread per process
C                         preemption is not a problem.
C      exchCollectStatistics - Turns exchange statistics collecting on and off.

       COMMON / EXCH_L / exchNeedsMemSync, exchUsesBarrier,
     &                   exchCollectStatistics
       LOGICAL exchNeedsMemSync
       LOGICAL exchUsesBarrier
       LOGICAL exchCollectStatistics

C--    COMMON / EXCH_R / REAL number common arrays for exchanges
C      xxxxSendBuf - Buffer used for sending data to another tile.
C      xxxxRecvBuf - Buffer used for receiving data from another tile.
       COMMON / EXCH_R /
     &  westSendBuf_RL, eastSendBuf_RL,
     &  southSendBuf_RL, northSendBuf_RL,
     &  westRecvBuf_RL, eastRecvBuf_RL,
     &  southRecvBuf_RL, northRecvBuf_RL,
     &  westSendBuf_RS, eastSendBuf_RS,
     &  southSendBuf_RS, northSendBuf_RS,
     &  westRecvBuf_RS, eastRecvBuf_RS,
     &  southRecvBuf_RS, northRecvBuf_RS,
     &  westSendBuf_R8, eastSendBuf_R8,
     &  southSendBuf_R8, northSendBuf_R8,
     &  westRecvBuf_R8, eastRecvBuf_R8,
     &  southRecvBuf_R8, northRecvBuf_R8,
     &  westSendBuf_R4, eastSendBuf_R4,
     &  southSendBuf_R4, northSendBuf_R4,
     &  westRecvBuf_R4, eastRecvBuf_R4,
     &  southRecvBuf_R4, northRecvBuf_R4
       _RL   westSendBuf_RL( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL   eastSendBuf_RL( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL  southSendBuf_RL( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL  northSendBuf_RL( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL   westRecvBuf_RL( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL   eastRecvBuf_RL( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL  southRecvBuf_RL( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RL  northRecvBuf_RL( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS   westSendBuf_RS( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS   eastSendBuf_RS( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS  southSendBuf_RS( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS  northSendBuf_RS( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS   westRecvBuf_RS( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS   eastRecvBuf_RS( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS  southRecvBuf_RS( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _RS  northRecvBuf_RS( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8   westSendBuf_R8( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8   eastSendBuf_R8( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8  southSendBuf_R8( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8  northSendBuf_R8( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8   westRecvBuf_R8( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8   eastRecvBuf_R8( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8  southRecvBuf_R8( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R8  northRecvBuf_R8( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4   westSendBuf_R4( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4   eastSendBuf_R4( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4  southSendBuf_R4( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4  northSendBuf_R4( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4   westRecvBuf_R4( L_WBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4   eastRecvBuf_R4( L_EBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4  southRecvBuf_R4( L_SBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       _R4  northRecvBuf_R4( L_NBUFFER, NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )

C--    COMMON / EXCH_I / INTEGER common arrays for exchanges
C      xxxxSendAck - Flag indicating ready to send data.
C      xxxxRecvAck - Falg indicating receive data is ready.
C      exchBufferLevel - Current cyclic buffer level.
C      exchNReqsX, exchNReqsY - Pending message counts
C      exchReqIdX, exchReqIdY -Pending message identifiers
C      *Spin* - Exchange statistics holder
C       Count - No. spins for each thread
C         Max - Maximum spins for an exchange
C         Min - Minimum spins for an exchange
       COMMON / EXCH_I /
     &  westSendAck, eastSendAck, southSendAck, northSendAck,
     &  westRecvAck, eastRecvAck, southRecvAck, northRecvAck,
     &  exchangeBufLevel,
     &  exchNReqsX, exchNReqsY, exchReqIdX, exchReqIdY,
     &  exchRecvXSpinCount, exchRecvXSpinMax, exchRecvXSpinMin,
     &  exchRecvXExchCount,
     &  exchRecvYSpinCount, exchRecvYSpinMax, exchRecvYSpinMin,
     &  exchRecvYExchCount
       INTEGER  westSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER  eastSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER southSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER northSendAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER  westRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER  eastRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER southRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER northRecvAck(            NUMBER_OF_BUFFER_LEVELS,
     &                       nSx, nSy )
       INTEGER exchangeBufLevel(  cacheLineSize/4, nSx, nSy )
       INTEGER exchNReqsX(cacheLineSize/4,nSx,nSy)
       INTEGER exchNReqsY(cacheLineSize/4,nSx,nSy)
       INTEGER exchReqIdX(2*nSx+2*nSy,cacheLineSize/4,nSx,nSy)
       INTEGER exchReqIdY(2*nSx+2*nSy,cacheLineSize/4,nSx,nSy)
       INTEGER exchRecvXSpinCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvXExchCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvXSpinMax  (cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvXSpinMin  (cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYSpinCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYExchCount(cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYSpinMax  (cacheLineSize/4, nSx, nSy)
       INTEGER exchRecvYSpinMin  (cacheLineSize/4, nSx, nSy)

