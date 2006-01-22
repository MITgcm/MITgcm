C $Header: /u/gcmpack/MITgcm/eesupp/src/lef_zero.F,v 1.1 2006/01/22 01:54:49 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP
C     !ROUTINE: LEF_ZERO
C     !INTERFACE:
      SUBROUTINE LEF_ZERO( arr ,myThid )
C     !DESCRIPTION: \bv
C     This routine simply sets the argument array to zero
C     \ev
C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
C     !INPUT/OUTPUT PARAMETERS:
C     === Arguments ===
      _RS  arr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,bi,bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          arr(i,j,bi,bj)=0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      RETURN
      END
