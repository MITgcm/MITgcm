C $Header: /u/gcmpack/MITgcm/pkg/exch2/W2_EXCH2_BUFFER.h,v 1.4 2012/09/04 00:43:13 jmc Exp $
C $Name:  $

CBOP
C     !ROUTINE: W2_EXCH2_BUFFER.h
C     !INTERFACE:
C     #include W2_EXCH2_BUFFER.h

C     !DESCRIPTION:
C     *==========================================================*
C     | W2_EXCH2_BUFFER.h
C     | o Definition of W2-EXCH2 Buffers
C     *==========================================================*
CEOP

C     e2BufrRecSize  :: Exch2 Buffer size
      INTEGER e2BufrRecSize
      PARAMETER ( e2BufrRecSize
     &            = ( (sNx+2*OLx)*2*OLy+(sNy+2*OLy)*2*OLx)*Nr
     &          )

C--   COMMON /W2_EXCH2_BUF_RX/ real type Buffer used by W2-EXCH2
      _RL e2Bufr1_RL( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _RL e2Bufr2_RL( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _RS e2Bufr1_RS( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _RS e2Bufr2_RS( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _R4 e2Bufr1_R4( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _R4 e2Bufr2_R4( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _R8 e2Bufr1_R8( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      _R8 e2Bufr2_R8( e2BufrRecSize, W2_maxNeighbours, nSx, nSy, 2 )
      COMMON /W2_EXCH2_BUF_RL/ e2Bufr1_RL, e2Bufr2_RL
      COMMON /W2_EXCH2_BUF_RS/ e2Bufr1_RS, e2Bufr2_RS
      COMMON /W2_EXCH2_BUF_R4/ e2Bufr1_R4, e2Bufr2_R4
      COMMON /W2_EXCH2_BUF_R8/ e2Bufr1_R8, e2Bufr2_R8

C--   COMMON /W2_EXCH2_BUF_I/ integer type Buffer used by W2-EXCH2
C     iBuf1Filled :: actual length of buffer-1 which has been filled in.
C     iBuf2Filled :: actual length of buffer-2 which has been filled in.
      INTEGER iBuf1Filled( W2_maxNeighbours, nSx, nSy )
      INTEGER iBuf2Filled( W2_maxNeighbours, nSx, nSy )
      COMMON /W2_EXCH2_BUF_I/ iBuf1Filled, iBuf2Filled

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
