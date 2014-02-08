C $Header: /u/gcmpack/MITgcm/model/src/add_walls2masks.F,v 1.1 2014/02/08 17:20:06 jmc Exp $
C $Name:  $

c#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: ADD_WALLS2MASKS
C     !INTERFACE:
      SUBROUTINE ADD_WALLS2MASKS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ADD_WALLS2MASKS
C     | o Apply additional closing of Western and Southern edges
C     |   grid-cell open-water factor
C     *==========================================================*
C     | Reset to zero hFacW and/or hFacS grid factors at some
C     | specific locations. In particular, allow to prevent fluid
C     | transport (at any detph) between 2 adjacent vertical
C     | column by adding a "thin wall" between the 2.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid  :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     i,j,k   :: Loop counters
C     tmpfld  :: Temporary array used to load file in
      _RS tmpFldW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS tmpFldS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER bi, bj
      INTEGER i, j, k
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( addWwallFile.NE.' ' .OR. addSwallFile.NE.' ' ) THEN

C--   Load files "addWwallFile" and/or "addSwallFile":
        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             tmpFldW(i,j,bi,bj) = zeroRS
             tmpFldS(i,j,bi,bj) = zeroRS
            ENDDO
           ENDDO
         ENDDO
        ENDDO
        IF ( addWwallFile.NE.' ' ) THEN
          CALL READ_FLD_XY_RS( addWwallFile, ' ', tmpFldW, 0, myThid )
        ENDIF
        IF ( addSwallFile.NE.' ' ) THEN
          CALL READ_FLD_XY_RS( addSwallFile, ' ', tmpFldS, 0, myThid )
        ENDIF
        CALL EXCH_UV_XY_RS( tmpFldW, tmpFldS, .FALSE., myThid )

      ENDIF

      IF ( selectSigmaCoord.EQ.0 ) THEN
C---  r-coordinate with partial-cell or full cell mask

C--   The following block allows thin walls representation of non-periodic
C     boundaries such as happen on the lat-lon grid at the N/S poles.
C     We should really supply a flag for doing this.
c      IF ( closedBoundaryAtPoles ) THEN
        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              IF ( dyG(i,j,bi,bj).EQ.zeroRS ) THEN
                DO k=1,Nr
                 hFacW(i,j,k,bi,bj) = zeroRS
                ENDDO
                rLowW (i,j,bi,bj) = rF(1)
                rSurfW(i,j,bi,bj) = rF(1)
              ENDIF
              IF ( dxG(i,j,bi,bj).EQ.zeroRS ) THEN
                DO k=1,Nr
                 hFacS(i,j,k,bi,bj) = zeroRS
                ENDDO
                rLowS (i,j,bi,bj) = rF(1)
                rSurfS(i,j,bi,bj) = rF(1)
              ENDIF
            ENDDO
           ENDDO
         ENDDO
        ENDDO
c      ENDIF

C--   Addtional closing of Western and Southern grid-cell edges
C      as provided in files: "addWwallFile" or "addSwallFile"
       IF ( addWwallFile.NE.' ' .OR. addSwallFile.NE.' ' ) THEN
        DO bj=myByLo(myThid), myByHi(myThid)
         DO bi=myBxLo(myThid), myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              IF ( tmpFldW(i,j,bi,bj).EQ.oneRS ) THEN
                DO k=1,Nr
                 hFacW(i,j,k,bi,bj) = zeroRS
                ENDDO
                rLowW (i,j,bi,bj) = rF(1)
                rSurfW(i,j,bi,bj) = rF(1)
              ENDIF
              IF ( tmpFldS(i,j,bi,bj).EQ.oneRS ) THEN
                DO k=1,Nr
                 hFacS(i,j,k,bi,bj) = zeroRS
                ENDDO
                rLowS (i,j,bi,bj) = rF(1)
                rSurfS(i,j,bi,bj) = rF(1)
              ENDIF
            ENDDO
           ENDDO
         ENDDO
        ENDDO
       ENDIF

      ELSE
#ifndef DISABLE_SIGMA_CODE
C---  Sigma and Hybrid-Sigma set-up:
#endif /* DISABLE_SIGMA_CODE */
      ENDIF

      RETURN
      END
