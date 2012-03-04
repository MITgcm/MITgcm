C $Header: /u/gcmpack/MITgcm/pkg/frazil/frazil_init_varia.F,v 1.3 2012/03/04 23:11:20 dimitri Exp $
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
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_FRAZIL
# include "FRAZIL.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid :: my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_FRAZIL

C     !LOCAL VARIABLES:
      INTEGER i,j,k,bi,bj

C     Initialize arrays in common blocks :
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx
           FrazilForcingT(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_FRAZIL */

      RETURN
      END
