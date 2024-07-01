CBOP
C     !ROUTINE: EEBUFF_SIZE.h
C     !INTERFACE:
C     include "EEBUFF_SIZE.h"
C     !DESCRIPTION:
C     *==========================================================*
C     | EEBUFF_SIZE.h
C     | o Execution Environment buffer size
C     *==========================================================*
C     | Define maximum size/length for few EE relevant buffers
C     *==========================================================*
CEOP

C     GSVec_size  :: Maximum size/length of Global Sum Vector array
C Note : may need to increase "GSVec_size" depending on individual
C        pkg requirement since value here are deliberately small.
      INTEGER GSVec_size
      PARAMETER ( GSVec_size = 2*Nr + 100 )

