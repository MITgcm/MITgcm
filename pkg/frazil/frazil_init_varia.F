C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_init_varia.F,v 1.2 2012/03/03 16:04:38 dimitri Exp $
C $Name:  $

#include "FRAZIL_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: FRAZIL_INIT_VARIA

C !INTERFACE:
      SUBROUTINE FRAZIL_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize FRAZIL variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FRAZIL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_FRAZIL

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
C     3-dim.
        DO k=1,Nr
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           FrazilForcingT(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C- end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_FRAZIL */

      RETURN
      END
