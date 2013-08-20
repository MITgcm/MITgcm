C $Header: /u/gcmpack/MITgcm/pkg/offline/offline_init_varia.F,v 1.6 2013/08/20 20:20:47 jmc Exp $
C $Name:  $

#include "OFFLINE_OPTIONS.h"

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
         offlineLdRec(bi,bj) = 0
         offline_Wght(1,bi,bj) = 0. _d 0
         offline_Wght(2,bi,bj) = 0. _d 0
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             uvel0(i,j,k,bi,bj) = 0. _d 0
             uvel1(i,j,k,bi,bj) = 0. _d 0
             vvel0(i,j,k,bi,bj) = 0. _d 0
             vvel1(i,j,k,bi,bj) = 0. _d 0
             wvel0(i,j,k,bi,bj) = 0. _d 0
             wvel1(i,j,k,bi,bj) = 0. _d 0
             tave0(i,j,k,bi,bj) = 0. _d 0
             tave1(i,j,k,bi,bj) = 0. _d 0
             save0(i,j,k,bi,bj) = 0. _d 0
             save1(i,j,k,bi,bj) = 0. _d 0
             gmkx0(i,j,k,bi,bj) = 0. _d 0
             gmkx1(i,j,k,bi,bj) = 0. _d 0
             gmky0(i,j,k,bi,bj) = 0. _d 0
             gmky1(i,j,k,bi,bj) = 0. _d 0
             gmkz0(i,j,k,bi,bj) = 0. _d 0
             gmkz1(i,j,k,bi,bj) = 0. _d 0
             conv0(i,j,k,bi,bj) = 0. _d 0
             conv1(i,j,k,bi,bj) = 0. _d 0
             kdfs0(i,j,k,bi,bj) = 0. _d 0
             kdfs1(i,j,k,bi,bj) = 0. _d 0
             kght0(i,j,k,bi,bj) = 0. _d 0
             kght1(i,j,k,bi,bj) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
c            hflx0(i,j,bi,bj) = 0. _d 0
c            hflx1(i,j,bi,bj) = 0. _d 0
             sflx0(i,j,bi,bj) = 0. _d 0
             sflx1(i,j,bi,bj) = 0. _d 0
c            icem0(i,j,bi,bj) = 0. _d 0
c            icem1(i,j,bi,bj) = 0. _d 0
           ENDDO
         ENDDO
C--   end bi,bj loops.
       ENDDO
      ENDDO

C     Used to be called only when using pkg/dic. but since main state
C     variables are nor read from pickup-files (when useOffLine),
C     seems logical to always load them here from offline files.
      IF ( nonlinFreeSurf.LE.0 ) THEN
        CALL OFFLINE_FIELDS_LOAD( startTime, nIter0, myThid )
      ENDIF

      RETURN
      END
