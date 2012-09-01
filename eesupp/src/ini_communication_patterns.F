C $Header: /u/gcmpack/MITgcm/eesupp/src/ini_communication_patterns.F,v 1.11 2012/09/01 17:37:30 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: INI_COMMUNICATION_PATTERNS

C     !INTERFACE:
      SUBROUTINE INI_COMMUNICATION_PATTERNS( myThid )
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE INI\_COMMUNICATION\_PATTERNS
C     | o Initialise between tile communication data structures.
C     *==========================================================*
C     | This routine assigns identifiers to each tile and then
C     | defines a map of neighbors for each tile.
C     | For each neighbor a communication method is defined.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global data ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "EXCH.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: Thread number we are dealing with in this call
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     pxW   :: Process X coord of process to west.
C     pxE   :: Process X coord of process to west.
C     pyN   :: Process Y coord of process to north.
C     pyS   :: Process Y coord of process to south.
C     procW :: Process Id of process to west.
C     procE :: Process Id of process to east.
C     procN :: Process Id of process to north.
C     procS :: Process Id of process to south.
C     totalTileCount :: Total number of tiles
C     tagW0, tagE0, tagS0, tagN0, theTag :: Working variables for
C                                           calculating message tags.
C     biW, biE, bjN, bjS :: Tile x and y indices to west, east,
C                           south and north.
C     bi, bj   :: Tile loop counter
C     picnt, pjcnt   :: Process loop counter
C     bi0, bj0 :: Base global index coordinate ( on CS there is no global
C                 coord).
      INTEGER bi0(nPx)
      INTEGER bj0(nPy)
      INTEGER bi, bj, picnt, pjcnt
      INTEGER pxW, pxE, pyN, pyS
      INTEGER procW, procE, procN, procS
      INTEGER totalTileCount
      INTEGER tagW0, tagE0, tagS0, tagN0, theTag
      INTEGER biE, biW, bjN, bjS
      INTEGER thePx, thePy, theBj, theBi
CEOP

C--   Define a globally unique tile numbers for each tile.
C--   We aslo define the tile numbers for our east, west, south
C--   and north neighbor tiles here. As coded below this is done from
C--   a simple cartesian formula. To handle irregular tile distributions
C--   the code below would be changed. For instance we could read
C--   the neighbor tile information from a file rather than deriving
C--   it in-line. This allows general tile distributions and connectivity
C--   both within a thread, between threads and between processors.
C     Notes --
C     1. The cartesian based formula coded below works as follows:
C       i. Each tile has one west neighbor, one east neighbor
C          one north neignbor and one south neighbor.
C      ii. For each of my neighbors store the following
C          - neighbor tile id
C          - neighbor process id
C     2. The information that is stored is then used to determine
C        the between tile communication method. The method used
C        depends on whether the tile is part of the same process,
C        on the same machine etc...
C     3. To initialise a tile distribution with holes in it
C        i.e. tiles that are not computed on. Set tile number to
C        the value NULL_TILE. This must also be done for tileNoW,
C        tileNoE, tileNoS, tileNoN.
C     4. The default formula below assigns tile numbers sequentially
C        in X on the **global** grid. Within a process the tile numbers
C        will not necessairily be sequential. This means that the tile
C        numbering label does not change when nTx, nTy, nPx or nPy change.
C        It will only change if the tile size changes or the global
C        grid changes.
C     bi0 and bj0 are the base global tile grid coordinate for the first
C     tile in this process.
      DO picnt = 1, nPx
       bi0(picnt) = picnt
      ENDDO
      DO pjcnt = 1, nPy
       bj0(pjcnt) = pjcnt
      ENDDO
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C       o My tile identifier
Crg     tileNo(bi,bj) = (bj0(myPy)-1+bj-1)*nSx*nPx+bi0(myPx)+bi-1
        thePx = myPx
        thePy = myPy
        theBj = bj
        theBi = bi
        tileNo(bi,bj) =
     &    ((thePy-1)*nSy+theBj-1)*nSx*nPx
     &   + (thePx-1)*nSx
     &   + theBi
C       o My west neighbor tile and process identifier
        biW   = bi-1
        pxW   = myPx
        procW = myPid
        IF ( biW .LT. 1 ) THEN
         biW   = nSx
         pxW   = myPx-1
         procW = pidW
         IF ( pxW .LT. 1 ) pxW   = nPx
        ENDIF
Crg     tileNoW (bi,bj) = (bj0(myPy)-1+bj-1)*nSx*nPx+bi0(pxW)+biW-1
        thePx = pxW
        thePy = myPy
        theBj = bj
        theBi = biW
        tileNoW (bi,bj) =
     &    ((thePy-1)*nSy+theBj-1)*nSx*nPx
     &   + (thePx-1)*nSx
     &   + theBi
#ifdef ALLOW_NEST_CHILD
#ifndef ALLOW_USE_MPI
        tileNoW (bi,bj) = NULL_TILE
#endif
#endif
        tilePidW(bi,bj) = procW
        tileBiW (bi,bj) = biW
        tileBjW (bi,bj) = bj
C       o My east neighbor tile and process identifier
        biE   = bi+1
        pxE   = myPx
        procE = myPid
        IF ( biE .GT. nSx ) THEN
         biE = 1
         pxE = myPx+1
         procE = pidE
         IF ( pxE .GT. nPx ) pxE   = 1
        ENDIF
Crg     tileNoE(bi,bj)  = (bj0(myPy)-1+bj-1)*nSx*nPx+bi0(pxE)+biE-1
        thePx = pxE
        thePy = myPy
        theBi = biE
        theBj = bj
        tileNoE(bi,bj) =
     &    ((thePy-1)*nSy+theBj-1)*nSx*nPx
     &   + (thePx-1)*nSx
     &   + theBi
#ifdef ALLOW_NEST_CHILD
#ifndef ALLOW_USE_MPI
        tileNoE (bi,bj) = NULL_TILE
#endif
#endif
        tilePidE(bi,bj) = procE
        tileBiE (bi,bj) = biE
        tileBjE (bi,bj) = bj
C       o My north neighbor tile and process identifier
        bjN   = bj+1
        pyN   = myPy
        procN = myPid
        IF ( bjN .GT. nSy ) THEN
         bjN = 1
         pyN = myPy+1
         procN = pidN
         IF ( pyN .GT. nPy ) pyN   = 1
        ENDIF
Crg     tileNoN(bi,bj) = (bj0(pyN)-1+bjN-1)*nSx*nPx+bi0(myPx)+bi-1
        thePx = myPx
        thePy = pyN
        theBi = bi
        theBj = bjN
        tileNoN(bi,bj) =
     &    ((thePy-1)*nSy+theBj-1)*nSx*nPx
     &   + (thePx-1)*nSx
     &   + theBi
        tilePidN(bi,bj) = procN
         tileBiN(bi,bj) = bi
         tileBjN(bi,bj) = bjN
C       o My south neighbor tile and process identifier
        bjS   = bj-1
        pyS   = myPy
        procS = myPid
        IF ( bjS .LT. 1 ) THEN
         bjS = nSy
         pyS = pyS-1
         procS = pidS
         IF ( pyS .LT. 1 ) pyS = nPy
        ENDIF
Crg     tileNoS(bi,bj) = (bj0(pyS+1)-1+bjS-1)*nSx*nPx+bi0(myPx+1)+bi-1
        thePx = myPx
        thePy = pyS
        theBi = bi
        theBj = bjS
        tileNoS(bi,bj) =
     &    ((thePy-1)*nSy+theBj-1)*nSx*nPx
     &   + (thePx-1)*nSx
     &   + theBi
        tilePidS(bi,bj) = procS
         tileBiS(bi,bj) = bi
         tileBjS(bi,bj) = bjS
       ENDDO
      ENDDO

C--   Define the total count of tiles.
      totalTileCount = nSx*nSy*nPx*nPy

C--   Set tags for each tile face.
C     Tags are used to distinguish exchanges from particular
C     faces of particular tiles.
C     Tag numbers are based on
C      i - The tile number
C     ii - The direction (N,S,W,E) of the message
C     We dont check for the NULL_TILE tile number here as it
C     should not actually be used.
      TagW0=1
      TagE0=2
      TagN0=3
      TagS0=4
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C       Send tags
C       o Tag I use for messages I send to west
        theTag = TagW0*totalTileCount+tileNo(bi,bj)-1
        tileTagSendW(bi,bj) = theTag
C       o Tag I use for messages I send to east
        theTag = TagE0*totalTileCount+tileNo(bi,bj)-1
        tileTagSendE(bi,bj) = theTag
C       o Tag I use for messages I send to north
        theTag = TagN0*totalTileCount+tileNo(bi,bj)-1
        tileTagSendN(bi,bj) = theTag
C       o Tag I use for messages I send to south
        theTag = TagS0*totalTileCount+tileNo(bi,bj)-1
        tileTagSendS(bi,bj) = theTag
C       Receive tags
C       o Tag on messages I receive from my east
        theTag = TagW0*totalTileCount+tileNoE(bi,bj)-1
        tileTagRecvE(bi,bj) = theTag
C       o Tag on messages I receive from my west
        theTag = TagE0*totalTileCount+tileNoW(bi,bj)-1
        tileTagRecvW(bi,bj) = theTag
C       o Tag on messages I receive from my north
        theTag = TagS0*totalTileCount+tileNoN(bi,bj)-1
        tileTagRecvN(bi,bj) = theTag
C       o Tag on messages I receive from my north
        theTag = TagN0*totalTileCount+tileNoS(bi,bj)-1
        tileTagRecvS(bi,bj) = theTag
       ENDDO
      ENDDO

C--   Set the form of excahnge to use between neighboring tiles.
C     For now use either shared memory, messages or nothing. Further
C     rules can be added later to allow shm regions and ump regions etc...
C     Notes -
C     1. We require symmetry here. If one face of a tile uses
C        communication method A then the matching face on its neighbor
C        tile must also use communication method A.
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C      o West face communication
       IF ( tileNoW(bi,bj) .EQ. NULL_TILE ) THEN
        tileCommModeW(bi,bj) = COMM_NONE
       ELSE
        IF ( myPid .EQ. tilePidW(bi,bj) ) THEN
         tileCommModeW(bi,bj) = COMM_PUT
        ELSE
         tileCommModeW(bi,bj) = COMM_MSG
        ENDIF
       ENDIF
C      o East face communication
       IF ( tileNoE(bi,bj) .EQ. NULL_TILE ) THEN
        tileCommModeE(bi,bj) = COMM_NONE
       ELSE
        IF ( myPid .EQ. tilePidE(bi,bj) ) THEN
         tileCommModeE(bi,bj) = COMM_PUT
        ELSE
         tileCommModeE(bi,bj) = COMM_MSG
        ENDIF
       ENDIF
C      o South face communication
       IF ( tileNoS(bi,bj) .EQ. NULL_TILE ) THEN
        tileCommModeS(bi,bj) = COMM_NONE
       ELSE
        IF ( myPid .EQ. tilePidS(bi,bj) ) THEN
         tileCommModeS(bi,bj) = COMM_PUT
        ELSE
         tileCommModeS(bi,bj) = COMM_MSG
        ENDIF
       ENDIF
C      o North face communication
       IF ( tileNoN(bi,bj) .EQ. NULL_TILE ) THEN
        tileCommModeN(bi,bj) = COMM_NONE
       ELSE
        IF ( myPid .EQ. tilePidN(bi,bj) ) THEN
         tileCommModeN(bi,bj) = COMM_PUT
        ELSE
         tileCommModeN(bi,bj) = COMM_MSG
        ENDIF
       ENDIF

       ENDDO
      ENDDO

C     Initialise outstanding exchange request counter
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        exchNReqsX(1,bi,bj) = 0
        exchNReqsY(1,bi,bj) = 0
       ENDDO
      ENDDO

      RETURN
      END
