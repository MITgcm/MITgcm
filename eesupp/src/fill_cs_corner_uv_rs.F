C $Header: /u/gcmpack/MITgcm/eesupp/src/fill_cs_corner_uv_rs.F,v 1.4 2009/06/28 01:02:17 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FILL_CS_CORNER_UV_RS

C     !INTERFACE:
      SUBROUTINE FILL_CS_CORNER_UV_RS(
     I     withSigns,
     U     uFld, vFld,
     I     bi,bj, myThid)
      IMPLICIT NONE

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE FILL_CS_CORNER_UV_RS
C     | o Fill the corner-halo region of CS-grid,
C     |   for a 2 components, C-grid vector field
C     *==========================================================*
C     | o the corner halo region is filled with valid values
C     |   in order to extend in X and Y direction calculations
C     |   of fluxes, on a wide stencil.
C     *==========================================================*

C     !USES:
C     == Global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif /* ALLOW_EXCH2 */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C
C     withSigns :: uFld,vFld are vector components
C     uFld      :: u-component, at C-grid West  point location
C     vFld      :: v-component, at C-grid SOuth point location
C     bi,bj     :: tile indices
C     myThid    :: thread number
      LOGICAL withSigns
      _RS     uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS     vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C      i,j           :: loop indices
C      myTile        :: tile number
      INTEGER i,j
      LOGICAL southWestCorner
      LOGICAL southEastCorner
      LOGICAL northWestCorner
      LOGICAL northEastCorner
      _RL negOne
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif
CEOP

      IF (useCubedSphereExchange) THEN

       negOne = 1. _d 0
       IF (withSigns) negOne = -1. _d 0

#ifdef ALLOW_EXCH2
       myTile = W2_myTileList(bi,bj)
       southWestCorner = exch2_isWedge(myTile).EQ.1
     &             .AND. exch2_isSedge(myTile).EQ.1
       southEastCorner = exch2_isEedge(myTile).EQ.1
     &             .AND. exch2_isSedge(myTile).EQ.1
       northEastCorner = exch2_isEedge(myTile).EQ.1
     &             .AND. exch2_isNedge(myTile).EQ.1
       northWestCorner = exch2_isWedge(myTile).EQ.1
     &             .AND. exch2_isNedge(myTile).EQ.1
#else
       southWestCorner = .TRUE.
       southEastCorner = .TRUE.
       northWestCorner = .TRUE.
       northEastCorner = .TRUE.
#endif

C--    To extend calculations in X direction, fill uFld array
C        with valid value in the corner.
C  e.g., NW corner: copy V(    0,sNy  ) into U(   -1,sNy+1)
C                   copy V(    0,sNy-1) into U(   -2,sNy+1)
C                   copy V(   -1,sNy  ) into U(   -1,sNy+2)
C                   copy V(   -1,sNy-1) into U(   -2,sNy+2)
C          |             |
C      U(-1,sNy+1)   U(1,sNy+1)   U(2,sNy+1)
C          | ^           |             |
C     -----|--\--------corner----------|
C          |   \         |             |
C          |    \        |
C          |     \       |
C     -----|---V(0,sNy)--|---------------
C          |             |
C
C--    to extend calculations in Y direction, fill vFld array
C        with valid value in the corner.
C  e.g., NW corner: copy U(    2,sNy+1) into V(   0,sNy+2)
C                   copy U(    3,sNy+1) into V(   0,sNy+3)
C                   copy U(    2,sNy+2) into V(  -1,sNy+2)
C                   copy U(    3,sNy+2) into V(  -1,sNy+3)
C          |              |              |
C     -----|--V(0,sNy+2)--|--------------|
C          |    ^====================\
C      U(-1,sNy+1)     U(1,sNy+1)     U(2,sNy+1)
C          |              |              |
C     -----|--V(0,sNy+1)--o--------------|
C          |              |\             |
C          |              | \=corner
C          |              |
C     -----|--V(0,sNy)----|--
C          |              |
C

        IF ( southWestCorner ) THEN
C-        prepare for direction X:
          DO j=1,OLy
           DO i=1,OLx
            uFld( 1-i , 1-j ) = negOne*vFld( 1-j , 1+i )
           ENDDO
          ENDDO
C-        prepare for direction Y:
          DO j=1,OLy
           DO i=1,OLx
            vFld( 1-i , 1-j ) = negOne*uFld( 1+j , 1-i )
           ENDDO
          ENDDO
        ENDIF

        IF ( southEastCorner ) THEN
C-        prepare for direction X:
          DO j=1,OLy
           DO i=2,OLx
            uFld(sNx+i, 1-j ) = vFld(sNx+j, i  )
           ENDDO
          ENDDO
C-        prepare for direction Y:
          DO j=1,OLy
           DO i=1,OLx
            vFld(sNx+i, 1-j ) = uFld(sNx+1-j, 1-i  )
           ENDDO
          ENDDO
        ENDIF

        IF ( northWestCorner ) THEN
C-        prepare for direction X:
          DO j=1,OLy
           DO i=1,OLx
            uFld( 1-i ,sNy+j) = vFld( 1-j , sNy+1-i )
           ENDDO
          ENDDO
C-        prepare for direction Y:
          DO j=2,OLy
           DO i=1,OLx
            vFld( 1-i ,sNy+j) = uFld(  j  , sNy+i )
           ENDDO
          ENDDO
        ENDIF

        IF ( northEastCorner ) THEN
C-        prepare for direction X:
          DO j=1,OLy
           DO i=2,OLx
            uFld(sNx+i,sNy+j) = negOne*vFld(sNx+j, sNy+2-i )
           ENDDO
          ENDDO
C-        prepare for direction Y:
          DO j=2,OLy
           DO i=1,OLx
            vFld(sNx+i,sNy+j) = negOne*uFld(sNx+2-j, sNy+i )
           ENDDO
          ENDDO
        ENDIF

C--   End useCubedSphereExchange
      ENDIF

      RETURN
      END
