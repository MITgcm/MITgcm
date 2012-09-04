C $Header: /u/gcmpack/MITgcm/pkg/exch2/W2_EXCH2_PARAMS.h,v 1.13 2012/09/04 00:43:13 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: W2_EXCH2_PARAMS.h
C     !INTERFACE:
C     #include W2_EXCH2_PARAMS.h

C     !DESCRIPTION:
C     *==========================================================*
C     | W2_EXCH2_PARAMS.h
C     | o Header file defining WRAPPER2-EXCH2 topology parameters
C     *==========================================================*
CEOP

C--   COMMON /W2_EXCH2_PARM_I/ Integer valued parameters used by W2-EXCH2
C     preDefTopol :: pre-defined Topology selector:
C                 :: = 0 : topology defined from processing "data.exch2";
C                 :: = 1 : simple, single facet topology;
C                 :: = 2 : customized topology (w2_set_myown_facets)
C                 :: = 3 : 6-face Cube (3 face-dims: nRed, nGreen, nBlue).
C     nFacets     :: Number of facets (or domains) in this topology
C     facet_dims  :: facet pair of dimensions (n1x,n1y, n2x,n2y, ...)
C     nTiles      :: Number of tiles in this topology <- moved to W2_EXCH2_TOPOLOGY.h
C     nBlankTiles :: Number of "Blank-Tiles"
C     blankList   :: List of "Blank-Tiles" (non active)
C--
C     W2_mapIO    :: select option for global-IO mapping:
C             =-1 :: old format, put facets 1 after the other in the X dir.;
C                 :: this is not necessary "compact".
C             = 1 :: compact format, 1 facet after the other (mostly in Y dir.)
C                 :: but needs to fold some facet (domain) if too large
C             = 0 :: compact format (= 1 long line), one facet after the other.
C--
C     W2_oUnit    :: output fortran Unit for printing Std messages
C     W2_printMsg :: select option for information messages printing
C             < 0 :: open and print to "w2_tile_topology" log file
C             = 0 :: print the minimum, to StdOut
C             = 1 :: no duplicated print from different processes (only Master)
C             = 2 :: all processes do print (used to check).
      INTEGER preDefTopol
      INTEGER nFacets
      INTEGER facet_dims(2*W2_maxNbFacets)
      INTEGER nBlankTiles
      INTEGER blankList(W2_maxNbTiles)
      INTEGER W2_mapIO
      INTEGER W2_oUnit, W2_printMsg
      COMMON /W2_EXCH2_PARM_I/
     &        preDefTopol,
     &        nFacets, facet_dims,
     &        nBlankTiles, blankList,
     &        W2_mapIO,
     &        W2_oUnit, W2_printMsg

C--   COMMON /W2_EXCH2_PARM_L/ Logical valued parameters used by W2-EXCH2
C     W2_useE2ioLayOut :: =T: use Exch2 global-IO Layout; =F: use model default
      LOGICAL W2_useE2ioLayOut
      COMMON /W2_EXCH2_PARM_L/
     &        W2_useE2ioLayOut

C--   COMMON /W2_EXCH2_BUILD_I/ Integer variables used to build topology
C     facet_owns  :: Range of tiles this facet "owns"
C     facet_pij   \  ::
C     facet_oi     } :: indices correspondence matrix (facet_pij) & offsets:
C     facet_oj    /  ::
C-with:  suffix "so" for indices of source facet j ;
C        suffix "tg" for indices of target facet jj= INT(facet_link(i,j))
C      pij(:,i,j) : matrix which gives so indices when applied to tg indices
C        iso = pij(1)*itg + pij(2)*jtg + oi
C        jso = pij(3)*itg + pij(4)*jtg + oj
C-----
      INTEGER facet_owns(2,W2_maxNbFacets)
      INTEGER facet_pij(4,4,W2_maxNbFacets)
      INTEGER facet_oi(4,W2_maxNbFacets)
      INTEGER facet_oj(4,W2_maxNbFacets)
      COMMON /W2_EXCH2_BUILD_I/
     &        facet_owns,
     &        facet_pij, facet_oi, facet_oj

C--   COMMON /W2_EXCH2_PARM_R/ Real*4 valued parameters used by W2-EXCH2
C--   topology defined from processing "data.exch2" (preDefTopol=0):
C     facet_link  :: Face-Edge connectivity map:
C       facet_link(i,j)=XX.1 : face(j)-edge(i) (i=1,2,3,4 <==> N,S,E,W)
C       is connected to Northern edge of face "XX" ; similarly,
C       = XX.2 : to Southern.E, XX.3 = Eastern.E, XX.4 = Western.E of face "XX".
      Real*4  facet_link( 4, W2_maxNbFacets )
      COMMON /W2_EXCH2_PARM_R/ facet_link

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
