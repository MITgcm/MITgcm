C $Header: /u/gcmpack/MITgcm/eesupp/src/Attic/exch_control.F,v 1.6 2001/02/04 14:38:43 cnh Exp $
C $Name:  $

C      MITgcmUV Exchange routine
C      -------------------------
C      These routines include support for arrays with different 
C      overlap extents and a mechanism to allow reverse mode 
C      exchanges for adjoint based minimisation experiments.
C      Differing extents for array overlap regions allows for example
C      the conjugate gradient solver to be optimised for edge
C      exchages of width one whilst for Shapiro filtered 
C      atmospheric fileds overlap regions of 8 or more can
C      be defined.                                            
C       Another important new feature is a tile by tile definition
C      of the class of exchange operation used to update the
C      tile. This provides support for partially tiled regular
C      domains where land-filled tiles are eliminated.
C      Communication between tiles
C      can be based on any of 
C       1. message passing.
C       2. writes to remote memory.
C       3. direct reads from remote tiles.
C      The class of communication utilised is specified on a 
C      tile by tile basis. So that one face of a tile can
C      exchange via MPI, another via UMP, another via shared
C      memory and another not at all. The only requirement is that
C      of symmetry i.e. if one tile sends its data to another tile via
C      some mechanism the "sent to" tile must receive the data by the
C      same mechanism. There is no support for multiple communication
C      channels for a single tile face.
C      To support this a tile has the following attributes associated 
C      with it:
C      tileNo        - A unique number identifying the tile
C      tileNoW       - tileNo for the tile to my west
C      tileNoN       - tileNo for the tile to my north
C      tileNoS       - tileNo for the tile to my south
C      tileNoE       - tileNo for the tile to my east
C      tilePid       - Process id for this tile
C      tilePidW      - Process id for tile to west
C      tilePidE      - Process id for tile to east
C      tilePidN      - Process id for tile to north
C      tilePidS      - Process id for tile to south
C      tileCommModeW - Style of communication used to west
C      tileCommModeE - Style of communication used to east
C      tileCommModeN - Style of communication used to north
C      tileCommModeS - Style of communication used to south
C      tileTagSendW  - Tag for identifying send from tiles west
C                      communication "channel".
C      tileTagSendE  - Tag for identifying send from tiles east
C                      communication "channel".
C      tileTagSendN  - Tag for identifying send from tiles north
C                      communication "channel".
C      tileTagSendS  - Tag for identifying send from tiles south
C                      communication "channel".
C      tileTagRecvW  - Tag for identifying send from tiles west
C                      communication "channel".
C      tileTagRecvE  - Tag for identifying send from tiles east
C                      communication "channel".
C      tileTagRecvN  - Tag for identifying send from tiles north
C                      communication "channel".
C      tileTagRecvS  - Tag for identifying send from tiles south
C                      communication "channel".
C      tileB[ij]W    - bi and bj index for tile to west
C      tileB[ij]E    - bi and bj index for tile to east
C      tileB[ij]N    - bi and bj index for tile to north
C      tileB[ij]S    - bi and bj index for tile to south
C      The code in here although intricate is fairly
C      straightforward. There are four routines, one
C      for each of the data patterns we wish to do overlap
C      updates on - as listed below. Each routine has two
C      parts. The first part does overlap updates to and from
C      remote "processes", that is processes who do not truly
C      share the address space of this process. This part
C      requires a facility like MPI, CRAY shmem, Memory Channel UMP,
C      VMMC VIA. In the case of a simple
C      serial execution nothing happens in this part.
C      The second part of each routine does the true shared memory
C      overlap copying i.e. copying from one part of array phi
C      to another part. This part is always active, in the case
C      of a single threaded messaging code, however, this part
C      will not do any copies as all edges will be flagged as using
C      for example MPI.
C


CStartOfInterface
       SUBROUTINE EXCH_RL( 
     U            array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            simulationMode, cornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_RL                                       |
C     | o Control edge exchanges for RL array.                   |
C     |==========================================================|
C     |                                                          |
C     | Controlling routine for exchange of XY edges of an array |
C     | distributed in X and Y. The routine interfaces to        |
C     | communication routines that can use messages passing     |
C     | exchanges, put type exchanges or get type exchanges.     |
C     |  This allows anything from MPI to raw memory channel to  |
C     | memmap segments to be used as a inter-process and/or     |
C     | inter-thread communiation and synchronisation            |
C     | mechanism.                                               |
C     | Notes --                                                 |
C     | 1. Some low-level mechanisms such as raw memory-channel  |
C     | or SGI/CRAY shmem put do not have direct Fortran bindings|
C     | and are invoked through C stub routines.                 |
C     | 2. Although this routine is fairly general but it does   |
C     | require nSx and nSy are the same for all innvocations.   |
C     | There are many common data structures ( myByLo,          |
C     | westCommunicationMode, mpiIdW etc... ) tied in with      |
C     | (nSx,nSy). To support arbitray nSx and nSy would require |
C     | general forms of these.                                  |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C      == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C      == Routine arguments ==
C      array - Array with edges to exchange.
C      myOLw - West, East, North and South overlap region sizes.
C      myOLe
C      myOLn
C      myOLs
C      exchWidthX - Width of data region exchanged in X.
C      exchWidthY - Width of data region exchanged in Y.
C                   Note -- 
C                   1. In theory one could have a send width and
C                   a receive width for each face of each tile. The only
C                   restriction woul be that the send width of one
C                   face should equal the receive width of the sent to
C                   tile face. Dont know if this would be useful. I 
C                   have left it out for now as it requires additional 
C                   bookeeping.
C      simulationMode - Forward or reverse mode exchange ( provides 
C                       support for adjoint integration of code. )
C      cornerMode     - Flag indicating whether corner updates are 
C                       needed.
C      myThid         - Thread number of this instance of S/R EXCH...
       INTEGER myOLw
       INTEGER myOLe
       INTEGER myOLs
       INTEGER myOLn
       INTEGER myNz
       INTEGER exchWidthX
       INTEGER exchWidthY
       INTEGER simulationMode
       INTEGER cornerMode
       INTEGER myThid
       _RL array(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn, 
     &           myNZ, nSx, nSy)
CEndOfInterface

C      == Local variables ==
C      theSimulationMode - Holds working copy of simulation mode
C      theCornerMode     - Holds working copy of corner mode
       INTEGER theSimulationMode
       INTEGER theCornerMode
       INTEGER I,J,K,bi,bj

       theSimulationMode = simulationMode
       theCornerMode     = cornerMode

C--    Error checks
       IF ( exchWidthX .GT. myOLw   ) 
     &  STOP ' S/R EXCH_RL: exchWidthX .GT. myOLw'
       IF ( exchWidthX .GT. myOLe   ) 
     &  STOP ' S/R EXCH_RL: exchWidthX .GT. myOLe'
       IF ( exchWidthY .GT. myOLs   ) 
     &  STOP ' S/R EXCH_RL: exchWidthY .GT. myOLs'
       IF ( exchWidthY .GT. myOLn   ) 
     &  STOP ' S/R EXCH_RL: exchWidthY .GT. myOLn'
       IF ( myOLw      .GT. MAX_OLX_EXCH ) 
     &  STOP ' S/R EXCH_RL: myOLw .GT. MAX_OLX_EXCH'
       IF ( myOLe      .GT. MAX_OLX_EXCH ) 
     &  STOP ' S/R EXCH_RL: myOLe .GT. MAX_OLX_EXCH'
       IF ( myOLn      .GT. MAX_OLX_EXCH ) 
     &  STOP ' S/R EXCH_RL: myOLn .GT. MAX_OLY_EXCH'
       IF ( myOLs      .GT. MAX_OLY_EXCH ) 
     &  STOP ' S/R EXCH_RL: myOLs .GT. MAX_OLY_EXCH'
       IF ( myNZ       .GT. MAX_NR_EXCH  ) 
     &  STOP ' S/R EXCH_RL: myNZ  .GT. MAX_NR_EXCH '
       IF (       theSimulationMode .NE. FORWARD_SIMULATION
     &      .AND. theSimulationMode .NE. REVERSE_SIMULATION
     &    ) STOP ' S/R EXCH_RL: Unrecognised simulationMode '
       IF (       theCornerMode .NE. EXCH_IGNORE_CORNERS
     &      .AND. theCornerMode .NE. EXCH_UPDATE_CORNERS
     &    ) STOP ' S/R EXCH_RL: Unrecognised cornerMode '

C--    Cycle edge buffer level
       CALL EXCH_CYCLE_EBL( myThid )

       IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
C--    "Put" east and west edges.
       CALL EXCH_RL_SEND_PUT_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )

C--    If corners are important then sync and update east and west edges 
C--    before doing north and south exchanges.
       IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS ) THEN
        CALL EXCH_RL_RECV_GET_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )
       ENDIF

C      "Put" north and south edges.
       CALL EXCH_RL_SEND_PUT_Y( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )


C--    Sync and update north, south (and east, west if corner updating
C--    not active).
       IF ( theCornerMode .NE. EXCH_UPDATE_CORNERS ) THEN
        CALL EXCH_RL_RECV_GET_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )
       ENDIF
       CALL EXCH_RL_RECV_GET_Y( array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            theSimulationMode, theCornerMode, myThid )
       ENDIF

       IF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
C       "Put" north and south edges.
        CALL EXCH_RL_SEND_PUT_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
C--     If corners are important then sync and update east and west edges
C--     before doing north and south exchanges.
        IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS ) THEN
         CALL EXCH_RL_RECV_GET_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
        ENDIF
C--     "Put" east and west edges.
        CALL EXCH_RL_SEND_PUT_X( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
C--     Sync and update east, west (and north, south if corner updating
C--     not active).
        IF ( theCornerMode .NE. EXCH_UPDATE_CORNERS ) THEN
         CALL EXCH_RL_RECV_GET_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
        ENDIF
        CALL EXCH_RL_RECV_GET_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )
       ENDIF




C      Special case for zonal average model i.e. case where sNx == 1
C      In this case a forward mode exchange simply sets array to
C      the i=1 value for all i.
       IF ( sNx .EQ. 1 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K = 1,myNz
           DO J = 1-myOLs,sNy+myOLn
            DO I = 1-myOLw,sNx+myOLe
             array(I,J,K,bi,bj) = array(sNx,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       RETURN
       END

CStartOfInterface
       SUBROUTINE EXCH_RS( 
     U            array,
     I            myOLw, myOLe, myOLs, myOLn, myNz,
     I            exchWidthX, exchWidthY,
     I            simulationMode, cornerMode, myThid )
C     /==========================================================\
C     | SUBROUTINE EXCH_RS                                       |
C     | o Control edge exchanges for RS array.                   |
C     |==========================================================|
C     |                                                          |
C     | Controlling routine for exchange of XY edges of an array |
C     | distributed in X and Y. The routine interfaces to        |
C     | communication routines that can use messages passing     |
C     | exchanges, put type exchanges or get type exchanges.     |
C     |  This allows anything from MPI to raw memory channel to  |
C     | memmap segments to be used as a inter-process and/or     |
C     | inter-thread communiation and synchronisation            |
C     | mechanism.                                               |
C     | Notes --                                                 |
C     | 1. Some low-level mechanisms such as raw memory-channel  |
C     | or SGI/CRAY shmem put do not have direct Fortran bindings|
C     | and are invoked through C stub routines.                 |
C     | 2. Although this routine is fairly general but it does   |
C     | require nSx and nSy are the same for all innvocations.   |
C     | There are many common data structures ( myByLo,          |
C     | westCommunicationMode, mpiIdW etc... ) tied in with      |
C     | (nSx,nSy). To support arbitray nSx and nSy would require |
C     | general forms of these.                                  |
C     |                                                          |
C     \==========================================================/
      IMPLICIT NONE

C      == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C      == Routine arguments ==
C      array - Array with edges to exchange.
C      myOLw - West, East, North and South overlap region sizes.
C      myOLe
C      myOLn
C      myOLs
C      exchWidthX - Width of data region exchanged in X.
C      exchWidthY - Width of data region exchanged in Y.
C                   Note -- 
C                   1. In theory one could have a send width and
C                   a receive width for each face of each tile. The only
C                   restriction woul be that the send width of one
C                   face should equal the receive width of the sent to
C                   tile face. Dont know if this would be useful. I 
C                   have left it out for now as it requires additional 
C                   bookeeping.
C      simulationMode - Forward or reverse mode exchange ( provides 
C                       support for adjoint integration of code. )
C      cornerMode     - Flag indicating whether corner updates are 
C                       needed.
C      myThid         - Thread number of this instance of S/R EXCH...
       INTEGER myOLw
       INTEGER myOLe
       INTEGER myOLs
       INTEGER myOLn
       INTEGER myNz
       INTEGER exchWidthX
       INTEGER exchWidthY
       INTEGER simulationMode
       INTEGER cornerMode
       INTEGER myThid
       _RS array(1-myOLw:sNx+myOLe,
     &           1-myOLs:sNy+myOLn, 
     &           myNZ, nSx, nSy)
CEndOfInterface

C      == Local variables ==
C      theSimulationMode - Holds working copy of simulation mode
C      theCornerMode     - Holds working copy of corner mode
       INTEGER theSimulationMode
       INTEGER theCornerMode
       INTEGER I,J,K,bi,bj

       theSimulationMode = simulationMode
       theCornerMode     = cornerMode

C--    Error checks
       IF ( exchWidthX .GT. myOLw   ) 
     &  STOP ' S/R EXCH_RS: exchWidthX .GT. myOLw'
       IF ( exchWidthX .GT. myOLe   ) 
     &  STOP ' S/R EXCH_RS: exchWidthX .GT. myOLe'
       IF ( exchWidthY .GT. myOLs   ) 
     &  STOP ' S/R EXCH_RS: exchWidthY .GT. myOLs'
       IF ( exchWidthY .GT. myOLn   ) 
     &  STOP ' S/R EXCH_RS: exchWidthY .GT. myOLn'
       IF ( myOLw      .GT. MAX_OLX_EXCH ) 
     &  STOP ' S/R EXCH_RS: myOLw .GT. MAX_OLX_EXCH'
       IF ( myOLe      .GT. MAX_OLX_EXCH ) 
     &  STOP ' S/R EXCH_RS: myOLe .GT. MAX_OLX_EXCH'
       IF ( myOLn      .GT. MAX_OLX_EXCH ) 
     &  STOP ' S/R EXCH_RS: myOLn .GT. MAX_OLY_EXCH'
       IF ( myOLs      .GT. MAX_OLY_EXCH ) 
     &  STOP ' S/R EXCH_RS: myOLs .GT. MAX_OLY_EXCH'
       IF ( myNZ       .GT. MAX_NR_EXCH  ) 
     &  STOP ' S/R EXCH_RS: myNZ  .GT. MAX_NR_EXCH '
       IF (       theSimulationMode .NE. FORWARD_SIMULATION
     &      .AND. theSimulationMode .NE. REVERSE_SIMULATION
     &    ) STOP ' S/R EXCH_RS: Unrecognised simulationMode '
       IF (       theCornerMode .NE. EXCH_IGNORE_CORNERS
     &      .AND. theCornerMode .NE. EXCH_UPDATE_CORNERS
     &    ) STOP ' S/R EXCH_RS: Unrecognised cornerMode '

C--    Cycle edge buffer level
       CALL EXCH_CYCLE_EBL( myThid )

       IF ( theSimulationMode .EQ. FORWARD_SIMULATION ) THEN
C--     "Put" east and west edges.
        CALL EXCH_RS_SEND_PUT_X( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
C--     If corners are important then sync and update east and west edges 
C--     before doing north and south exchanges.
        IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS ) THEN
         CALL EXCH_RS_RECV_GET_X( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
        ENDIF
C       "Put" north and south edges.
        CALL EXCH_RS_SEND_PUT_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
C--     Sync and update north, south (and east, west if corner updating
C--     not active).
        IF ( theCornerMode .NE. EXCH_UPDATE_CORNERS ) THEN
         CALL EXCH_RS_RECV_GET_X( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
        ENDIF
        CALL EXCH_RS_RECV_GET_Y( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )
       ENDIF

       IF ( theSimulationMode .EQ. REVERSE_SIMULATION ) THEN
C       "Put" north and south edges.
        CALL EXCH_RS_SEND_PUT_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
C--     If corners are important then sync and update east and west edges
C--     before doing north and south exchanges.
        IF ( theCornerMode .EQ. EXCH_UPDATE_CORNERS ) THEN
         CALL EXCH_RS_RECV_GET_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
        ENDIF
C--     "Put" east and west edges.
        CALL EXCH_RS_SEND_PUT_X( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
C--     Sync and update east, west (and north, south if corner updating
C--     not active).
        IF ( theCornerMode .NE. EXCH_UPDATE_CORNERS ) THEN
         CALL EXCH_RS_RECV_GET_Y( array,
     I              myOLw, myOLe, myOLs, myOLn, myNz,
     I              exchWidthX, exchWidthY,
     I              theSimulationMode, theCornerMode, myThid )
        ENDIF
        CALL EXCH_RS_RECV_GET_X( array,
     I             myOLw, myOLe, myOLs, myOLn, myNz,
     I             exchWidthX, exchWidthY,
     I             theSimulationMode, theCornerMode, myThid )
       ENDIF
C      Special case for zonal average model i.e. case where sNx == 1
C      In this case a forward mode exchange simply sets array to
C      the i=1 value for all i.
       IF ( sNx .EQ. 1 ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO K = 1,myNz
           DO J = 1-myOLs,sNy+myOLn
            DO I = 1-myOLw,sNx+myOLe
             array(I,J,K,bi,bj) = array(sNx,J,K,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       RETURN
       END
