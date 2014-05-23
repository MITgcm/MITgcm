C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_init_varia.F,v 1.4 2014/05/23 20:03:24 jmc Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize SALT_PLUME variables

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SALT_PLUME.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME

C     !LOCAL VARIABLES:
      INTEGER i,j, bi,bj,Nrp1
#ifdef SALT_PLUME_VOLUME
      INTEGER k
#endif

      Nrp1=Nr+1
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
C     2-dim.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          SaltPlumeDepth (i,j,bi,bj) = 0. _d 0
          saltPlumeFlux  (i,j,bi,bj) = 0. _d 0
#ifdef SALT_PLUME_VOLUME
          SPbrineVolFlux (i,j,bi,bj) = 0. _d 0
          SPforcS1       (i,j,bi,bj) = 0. _d 0
          DO k=1,Nr
           dSPvolSurf2kLev (i,j,k,bi,bj) = 0. _d 0
           dSPvolkLev2Above(i,j,k,bi,bj) = 0. _d 0
           SPforcingS      (i,j,k,bi,bj) = 0. _d 0
           SPforcingT      (i,j,k,bi,bj) = 0. _d 0
           SPplumek        (i,j,k,bi,bj) = 1. _d 0
          ENDDO
          dSPvolkLev2Above (i,j,Nrp1,bi,bj) = 0. _d 0
          SPplumek         (i,j,Nrp1,bi,bj) = 1. _d 0
#endif
         ENDDO
        ENDDO

C- end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
