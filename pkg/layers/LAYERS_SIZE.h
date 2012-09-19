C $Header: /u/gcmpack/MITgcm/pkg/layers/LAYERS_SIZE.h,v 1.2 2012/09/19 18:48:18 gforget Exp $
C $Name:  $

C ======================================================================
C * Compiled-in size options for the LAYERS package *
C
C  - Just as you have to define Nr in SIZE.h, you must define the number
C    of vertical layers for isopycnal averaging so that the proper array
C    sizes can be declared in the LAYERS.h header file.
C
C  - Variables -
C        NLayers      :: the number if isopycnal layers (must match data.layers)
C        FineGridFact :: how many fine-grid cells per dF cell
C        FineGridMax  :: the number of points in the finer vertical grid
C                         used for interpolation
      INTEGER    Nlayers, FineGridFact, FineGridMax, layers_maxNum
      PARAMETER( Nlayers = 20 )
      PARAMETER( FineGridFact = 10 )
      PARAMETER( FineGridMax = Nr * FineGridFact )
      PARAMETER( layers_maxNum = 1 )

