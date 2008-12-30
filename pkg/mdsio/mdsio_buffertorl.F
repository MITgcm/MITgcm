C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_buffertorl.F,v 1.1 2008/12/30 01:54:26 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MDS_BUFFERtoRL

C !INTERFACE:
      SUBROUTINE MDS_BUFFERtoRL( vecBuf, arr, narr, copyTo, myThid )

C !DESCRIPTION:
C Copy from Buffer to RL array "arr", if copyTo=T ; otherwise do the reverse

C !USES:
      IMPLICIT NONE
c#include "EEPARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C vecBuf ( RL )    :: buffer vector array (Input/Output if copyTo=T/F)
C arr    ( RL )    :: model  vector array (Output/Input if copyTo=T/F)
C narr   (integer) :: dimension of buffer & model vector array
C copyTo (logical) :: if =T, copy buffer -> model ; if =F: copy buffer <- model
C myThid (integer) :: my Thread Id number
      INTEGER narr
      _RL vecBuf(narr)
      _RL arr(narr)
      LOGICAL copyTo
      INTEGER myThid
CEOP

C !LOCAL VARIABLES:
      INTEGER L

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (copyTo) THEN
        DO L=1,narr
         arr(L) = vecBuf(L)
        ENDDO
      ELSE
        DO L=1,narr
          vecBuf(L) = arr(L)
        ENDDO
      ENDIF

      RETURN
      END
