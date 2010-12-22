C $Header: /u/gcmpack/MITgcm/pkg/exch2/W2_EXCH2_TOPOLOGY.h,v 1.9 2010/12/22 21:22:46 jahn Exp $
C $Name:  $

CBOP
C     !ROUTINE: W2_EXCH2_TOPOLOGY.h
C     !INTERFACE:
C     #include W2_EXCH2_TOPOLOGY.h

C     !DESCRIPTION:
C     *==========================================================*
C     | W2_EXCH2_TOPOLOGY.h
C     | o Header defining tile exchange and mapping for W2_EXCH2
C     *==========================================================*
C     | 1rst part holds the full topology structure (same for all
C     |  process) and is independent of tile-processor repartition
C     |  (needs W2_EXCH2_SIZE.h to be included before)
C     | 2nd part (put in this header for convenience) holds
C     |   Tile Ids and is function of tile-process repartition
C     |  (needs SIZE.h to be included before)
C     *==========================================================*
CEOP
 
C---   Parameters for enumerating directions
       INTEGER W2_NORTH, W2_SOUTH, W2_EAST, W2_WEST
       PARAMETER ( W2_NORTH = 1 )
       PARAMETER ( W2_SOUTH = 2 )
       PARAMETER ( W2_EAST  = 3 )
       PARAMETER ( W2_WEST  = 4 )

C---   Topology data structures
C      exch2_global_Nx   :: Global-file domain length.
C      exch2_global_Ny   :: Global-file domain height.
C      exch2_xStack_Nx   :: Length of domain used for north/south OBCS.
C      exch2_xStack_Ny   :: Height of domain used for north/south OBCS.
C      exch2_yStack_Nx   :: Length of domain used for east/west OBCS.
C      exch2_yStack_Ny   :: Height of domain used for east/west OBCS.
C---   Tiling and Exch data structures
C      exch2_tProc       :: Rank of process owning tile (filled at run time).
C      exch2_myFace      :: Face number for each tile (used for I/O).
C      exch2_mydNx       :: Face size in X for each tile (for I/O).
C      exch2_mydNy       :: Face size in Y for each tile (for I/O).
C      exch2_tNx         :: Size in X for each tile.
C      exch2_tNy         :: Size in Y for each tile.
C      exch2_tBasex      :: Tile offset in X within its sub-domain (cube face)
C      exch2_tBasey      :: Tile offset in Y within its sub-domain (cube face)
C      exch2_txGlobalo   :: Tile base X index within global index space.
C      exch2_tyGlobalo   :: Tile base Y index within global index space.
C      exch2_txXStackLo  :: Tile base X index within N/S OBCS index space.
C      exch2_tyXStackLo  :: Tile base Y index within N/S OBCS index space.
C      exch2_txYStackLo  :: Tile base X index within E/W OBCS index space.
C      exch2_tyYStackLo  :: Tile base Y index within E/W OBCS index space.
C      exch2_isWedge     :: 1 if West  is at domain edge, 0 if not.
C      exch2_isNedge     :: 1 if North is at domain edge, 0 if not.
C      exch2_isEedge     :: 1 if East  is at domain edge, 0 if not.
C      exch2_isSedge     :: 1 if South is at domain edge, 0 if not.
C      exch2_nNeighbours :: Tile neighbour entries count.
C      exch2_neighbourId :: Tile number for each neighbour entry.
C      exch2_opposingSend:: Neighbour entry in target tile send
C                        :: which has this tile and neighbour as its target.
C      exch2_pij(:,n,t)  :: Matrix which applies to target-tile indices to get
C                        :: source-tile "t" indices, for neighbour entry "n".
C      exch2_oi(n,t)     :: Source-tile "t" X index offset in target
C                        :: to source connection (neighbour entry "n").
C      exch2_oj(n,t)     :: Source-tile "t" Y index offset in target
C                        :: to source connection (neighbour entry "n").
       INTEGER exch2_global_Nx
       INTEGER exch2_global_Ny
       INTEGER exch2_xStack_Nx
       INTEGER exch2_xStack_Ny
       INTEGER exch2_yStack_Nx
       INTEGER exch2_yStack_Ny
       INTEGER exch2_tProc( W2_maxNbTiles )
       INTEGER exch2_myFace( W2_maxNbTiles )
       INTEGER exch2_mydNx( W2_maxNbTiles )
       INTEGER exch2_mydNy( W2_maxNbTiles )
       INTEGER exch2_tNx( W2_maxNbTiles )
       INTEGER exch2_tNy( W2_maxNbTiles )
       INTEGER exch2_tBasex( W2_maxNbTiles )
       INTEGER exch2_tBasey( W2_maxNbTiles )
       INTEGER exch2_txGlobalo(W2_maxNbTiles )
       INTEGER exch2_tyGlobalo(W2_maxNbTiles )
       INTEGER exch2_txXStackLo(W2_maxNbTiles )
       INTEGER exch2_tyXStackLo(W2_maxNbTiles )
       INTEGER exch2_txYStackLo(W2_maxNbTiles )
       INTEGER exch2_tyYStackLo(W2_maxNbTiles )
       INTEGER exch2_isWedge( W2_maxNbTiles )
       INTEGER exch2_isNedge( W2_maxNbTiles )
       INTEGER exch2_isEedge( W2_maxNbTiles )
       INTEGER exch2_isSedge( W2_maxNbTiles )
       INTEGER exch2_nNeighbours( W2_maxNbTiles )
       INTEGER exch2_neighbourId(  W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_opposingSend( W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_neighbourDir( W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_pij(4,W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_oi (  W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_oj (  W2_maxNeighbours, W2_maxNbTiles )

       COMMON /W2_EXCH2_TOPO_I/
     &        exch2_global_Nx, exch2_global_Ny,
     &        exch2_xStack_Nx, exch2_xStack_Ny,
     &        exch2_yStack_Nx, exch2_yStack_Ny,
     &        exch2_tProc,
     &        exch2_myFace, exch2_mydNx, exch2_mydNy,
     &        exch2_tNx, exch2_tNy,
     &        exch2_tBasex, exch2_tBasey,
     &        exch2_txGlobalo,exch2_tyGlobalo,
     &        exch2_txXStackLo,exch2_tyXStackLo,
     &        exch2_txYStackLo,exch2_tyYStackLo,
     &        exch2_isWedge, exch2_isNedge,
     &        exch2_isEedge, exch2_isSedge,
     &        exch2_nNeighbours, exch2_neighbourId,
     &        exch2_opposingSend, exch2_neighbourDir,
     &        exch2_pij,
     &        exch2_oi, exch2_oj

C---   Exchange execution loop data structures
C      exch2_iLo,iHi(n,t) :: X-index range of this tile "t" halo-region
C                         :: to be updated with neighbour entry "n".
C      exch2_jLo,jHi(n,t) :: Y-index range of this tile "t" halo-region
C                         :: to be updated with neighbour entry "n".
       INTEGER exch2_iLo( W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_iHi( W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_jLo( W2_maxNeighbours, W2_maxNbTiles )
       INTEGER exch2_jHi( W2_maxNeighbours, W2_maxNbTiles )
       COMMON /W2_EXCH2_HALO_SPEC/
     &        exch2_iLo, exch2_iHi,
     &        exch2_jLo, exch2_jHi

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   COMMON /W2_EXCH2_TILE_ID/ tile ids
C     W2 tile id variables (tile ids are no longer a function of
C     process and subgrid indices).
C     W2_myTileList   :: list of tiles owned by this process
C     W2_procTileList :: same as W2_myTileList, but contains
C                        information for all processes
      INTEGER W2_myTileList(nSx,nSy)
      INTEGER W2_procTileList(nSx,nSy,nPx*nPy)
      COMMON /W2_EXCH2_TILE_ID/
     &        W2_myTileList, W2_procTileList

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
