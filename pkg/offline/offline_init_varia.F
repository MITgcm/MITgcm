C $Header: /u/gcmpack/MITgcm/pkg/offline/offline_init_varia.F,v 1.2 2009/10/10 21:28:20 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"

C !INTERFACE: ==========================================================
      SUBROUTINE OFFLINE_INIT_VARIA( myThid )

C !DESCRIPTION:
C calls subroutines that initializes variables for offline code

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OFFLINE.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j,k,bi,bj  :: loop indices
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k=1,Nr
            DO j=1-Oly,sNy+OLy
              DO i=1-Olx,sNx+Olx
                 save0(i,j,k,bi,bj) = 0. _d 0
                 tave0(i,j,k,bi,bj) = 0. _d 0
                 uvel0(i,j,k,bi,bj) = 0. _d 0
                 vvel0(i,j,k,bi,bj) = 0. _d 0
                 wvel0(i,j,k,bi,bj) = 0. _d 0
                 conv0(i,j,k,bi,bj) = 0. _d 0
                 gmkx0(i,j,k,bi,bj) = 0. _d 0
                 gmky0(i,j,k,bi,bj) = 0. _d 0
                 gmkz0(i,j,k,bi,bj) = 0. _d 0
c
                 save1(i,j,k,bi,bj) = 0. _d 0
                 tave1(i,j,k,bi,bj) = 0. _d 0
                 uvel1(i,j,k,bi,bj) = 0. _d 0
                 vvel1(i,j,k,bi,bj) = 0. _d 0
                 wvel1(i,j,k,bi,bj) = 0. _d 0
                 conv1(i,j,k,bi,bj) = 0. _d 0
                 gmkx1(i,j,k,bi,bj) = 0. _d 0
                 gmky1(i,j,k,bi,bj) = 0. _d 0
                 gmkz1(i,j,k,bi,bj) = 0. _d 0
              ENDDO
            ENDDO
          ENDDO
          DO j=1-Oly,sNy+OLy
            DO i=1-Olx,sNx+Olx
                 hflx0(i,j,bi,bj) = 0. _d 0
                 sflx0(i,j,bi,bj) = 0. _d 0
                 hflx1(i,j,bi,bj) = 0. _d 0
                 sflx1(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END
