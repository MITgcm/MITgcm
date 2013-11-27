C $Header: /u/gcmpack/MITgcm/pkg/exch2/W2_EXCH2_TOPOLOGY.h,v 1.13 2013/11/27 00:34:05 jmc Exp $
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
C      exch2_nTiles      :: Number of tiles in this topology
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
       INTEGER exch2_nTiles
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
     &        exch2_nTiles,
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

C---   Cumulated Sum operator
C      W2_tMC1, W2_tMC2 :: tile that holds Missing Corners (=f1.NW,f2.SE)
C      W2_cumSum_facet(1,f1,f2) :: cum-sum at facet f2 origin function of
C                                  facet f1 X-increment
C      W2_cumSum_facet(2,f1,f2) :: cum-sum at tile f2 origin function of
C                                  facet f1 Y-increment
C      W2_cumSum_tiles(1,t1,t2) :: cum-sum at tile t2 origin function of
C                                  tile t1 X-increment
C      W2_cumSum_tiles(2,t1,t2) :: cum-sum at tile t2 origin function of
C                                  tile t1 Y-increment
       INTEGER W2_tMC1, W2_tMC2
       INTEGER W2_cumSum_facet( 2, W2_maxNbFacets,W2_maxNbFacets)
       COMMON /W2_CUMSUM_TOPO_I/
     &        W2_tMC1, W2_tMC2,
     &        W2_cumSum_facet
#ifdef W2_CUMSUM_USE_MATRIX
       INTEGER W2_cumSum_tiles( 2, W2_maxNbTiles, W2_maxNbTiles )
       COMMON /W2_CUMSUM_MATRIX/
     &        W2_cumSum_tiles
#endif /* W2_CUMSUM_USE_MATRIX */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   COMMON /W2_MAP_TILE2PROC/ mapping of tiles to process:
C     get W2 tile Id from process Id + subgrid indices (bi,bj) or the reverse
C     (tile ids are no longer a simple function of process and subgrid indices).
C
C     W2_tileProc(tN) :: Rank of process owning tile tN (filled at run time).
C     W2_tileIndex(tN):: local subgrid index of tile tN
C     W2_tileRank(tN) :: rank of tile tN in full-tile list (without blank)
C     W2_myTileList   :: list of tiles owned by this process
C     W2_procTileList :: same as W2_myTileList, but contains
C                        information for all processes
      INTEGER W2_tileProc ( W2_maxNbTiles )
      INTEGER W2_tileIndex( W2_maxNbTiles )
c     INTEGER W2_tileRank ( W2_maxNbTiles )
      INTEGER W2_myTileList ( nSx,nSy )
      INTEGER W2_procTileList(nSx,nSy,nPx*nPy )
      COMMON /W2_MAP_TILE2PROC/
     &        W2_tileProc,
     &        W2_tileIndex,
c    &        W2_tileRank,
     &        W2_myTileList, W2_procTileList

C--   COMMON /W2_EXCH2_COMMFLAG/ EXCH2 character Flag for type of communication
      CHARACTER W2_myCommFlag( W2_maxNeighbours, nSx, nSy )
      COMMON /W2_EXCH2_COMMFLAG/ W2_myCommFlag

C--   COMMON /EXCH2_FILLVAL_RX/ real type filling value used by EXCH2
C     e2FillValue_RX :: filling value for null regions (facet-corner
C                    :: halo regions)
      _RL e2FillValue_RL
      _RS e2FillValue_RS
      _R4 e2FillValue_R4
      _R8 e2FillValue_R8
      COMMON /EXCH2_FILLVAL_RL/ e2FillValue_RL
      COMMON /EXCH2_FILLVAL_RS/ e2FillValue_RS
      COMMON /EXCH2_FILLVAL_R4/ e2FillValue_R4
      COMMON /EXCH2_FILLVAL_R8/ e2FillValue_R8

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
