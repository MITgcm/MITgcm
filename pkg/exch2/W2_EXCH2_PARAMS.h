C $Header: /u/gcmpack/MITgcm/pkg/exch2/W2_EXCH2_PARAMS.h,v 1.5 2005/07/22 18:21:55 jmc Exp $
C
C     /==========================================================\
C     | W2_EXCH2_PARAMS.h Parameters for a WRAPPER2 topology     |
C     |==========================================================|
C     |                                                          |
C     \==========================================================/

C     W2 tile id variables (tile ids are no longer a function of
C     process and subgrid indicies).
C     W2_myTileList     :: list of tiles owned by this process
C     W2_mpi_myTileList :: same as W2_myTileList, but contains
C                          information for all processes
      INTEGER W2_myTileList(nSx), W2_mpi_myTileList(nPx*nPy,nSx)
c     INTEGER E2_MSGHANDLES(2,MAX_NEIGHBOURS, nSx)
      COMMON /W2_EXCH2_PARAMS_I/ W2_myTileList, W2_mpi_myTileList

      CHARACTER W2_myCommFlag(MAX_NEIGHBOURS,nSx)
      COMMON /W2_EXCH2_PARAMS_C/ W2_myCommFlag

      INTEGER e2BufrRecSize
      PARAMETER ( 
     & e2BufrRecSize = ( (sNx+2*OLx)*2*OLy+(sNy+2*OLy)*2*OLx)*Nr 
     &)
      _RL E2BUFR1_RL( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _RL E2BUFR2_RL( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _RL E2BUFR1_RS( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _RL E2BUFR2_RS( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _R4 E2BUFR1_R4( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _R4 E2BUFR2_R4( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _R8 E2BUFR1_R8( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      _R8 E2BUFR2_R8( e2BufrRecSize, MAX_NEIGHBOURS, nSx, 2 )
      COMMON /W2_EXCH2_BUF_RL/ E2BUFR1_RL, E2BUFR2_RL
      COMMON /W2_EXCH2_BUF_RS/ E2BUFR1_RS, E2BUFR2_RS
      COMMON /W2_EXCH2_BUF_R4/ E2BUFR1_R4, E2BUFR2_R4
      COMMON /W2_EXCH2_BUF_R8/ E2BUFR1_R8, E2BUFR2_R8
