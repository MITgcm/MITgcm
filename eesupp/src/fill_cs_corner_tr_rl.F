C $Header: /u/gcmpack/MITgcm/eesupp/src/fill_cs_corner_tr_rl.F,v 1.6 2009/06/28 01:02:17 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: FILL_CS_CORNER_TR_RL

C     !INTERFACE:
      SUBROUTINE FILL_CS_CORNER_TR_RL(
     I     fill4dir, withSigns,
     U     trFld,
     I     bi,bj, myThid)

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE FILL_CS_CORNER_TR_RL
C     | o Fill the corner-halo region of CS-grid,
C     |   for a tracer variable (center of grid cell)
C     *==========================================================*
C     | o the corner halo region is filled with valid values
C     |   in order to compute (later on) gradient in X or Y
C     |   direction on a wide stencil.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

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
C     fill4dir  :: = 0 fill corner with zeros
C                  = 1 copy to prepare for X direction calculations
C                  = 2 copy to prepare for Y direction calculations
C                  = 3 fill corner with averaged value
C     withSigns :: True = account for sign of X & Y directions
C     trFld     :: tracer field array with empty corners to fill
C     bi,bj     :: tile indices
C     myThid    :: thread number
      INTEGER fill4dir
      LOGICAL withSigns
      _RL trFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
      _RL     negOne
#ifdef ALLOW_EXCH2
      INTEGER myTile
#endif
CEOP

      negOne = 1.
      IF (withSigns) negOne = -1.

      IF (useCubedSphereExchange) THEN

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

       IF ( fill4dir .EQ. 0 ) THEN
C--    Just fill corner with zero (e.g., used for 6 tracer points average)

         IF ( southWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld( 1-i , 1-j ) = 0. _d 0
           ENDDO
          ENDDO
         ENDIF
         IF ( southEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld(sNx+i, 1-j ) = 0. _d 0
           ENDDO
          ENDDO
         ENDIF
         IF ( northWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld( 1-i ,sNy+j) = 0. _d 0
           ENDDO
          ENDDO
         ENDIF
         IF ( northEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld(sNx+i,sNy+j) = 0. _d 0
           ENDDO
          ENDDO
         ENDIF

       ELSEIF ( fill4dir .EQ. 1 ) THEN
C--    Internal exchange for calculations in X

C-     For cube face corners we need to duplicate the
C-     i-1 and i+1 values into the null space as follows:
C
C
C      o NW corner: copy T(    0,sNy  ) into T(    0,sNy+1) e.g.
C                      |
C         x T(0,sNy+1) |
C        /\            |
C      --||------------|-----------
C        ||            |
C         x T(0,sNy)   |   x T(1,sNy)
C                      |
C
C      o SW corner: copy T(0,1) into T(0,0) e.g.
C                      |
C         x T(0,1)     |  x T(1,1)
C        ||            |
C      --||------------|-----------
C        \/            |
C         x T(0,0)     |
C                      |
C
C      o NE corner: copy T(sNx+1,sNy  ) into T(sNx+1,sNy+1) e.g.
C                      |
C                      |   x T(sNx+1,sNy+1)
C                      |  /\
C      ----------------|--||-------
C                      |  ||
C         x T(sNx,sNy) |   x T(sNx+1,sNy  )
C                      |
C      o SE corner: copy T(sNx+1,1    ) into T(sNx+1,0    ) e.g.
C                      |
C         x T(sNx,1)   |   x T(sNx+1,    1)
C                      |  ||
C      ----------------|--||-------
C                      |  \/
C                      |   x T(sNx+1,    0)
         IF ( southWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld( 1-i , 1-j ) = negOne*trFld( 1-j , i  )
           ENDDO
          ENDDO
         ENDIF
         IF ( southEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld(sNx+i, 1-j ) = negOne*trFld(sNx+j, i  )
           ENDDO
          ENDDO
         ENDIF
         IF ( northWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld( 1-i ,sNy+j) = negOne*trFld( 1-j , sNy+1-i )
           ENDDO
          ENDDO
         ENDIF
         IF ( northEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            trFld(sNx+i,sNy+j) = negOne*trFld(sNx+j, sNy+1-i )
           ENDDO
          ENDDO
         ENDIF

C--   End of X direction ; start Y direction case.

       ELSEIF ( fill4dir .EQ. 2 ) THEN
C--    Internal exchange for calculations in Y

C-     For cube face corners we need to duplicate the
C-     j-1 and j+1 values into the null space as follows:
C
C      o SW corner: copy T(0,1) into T(0,0) e.g.
C                      |
C                      |  x T(1,1)
C                      |
C      ----------------|-----------
C                      |
C         x T(0,0)<====== x T(1,0)
C                      |
C
C      o NW corner: copy T(    0,sNy  ) into T(    0,sNy+1) e.g.
C                      |
C         x T(0,sNy+1)<=== x T(1,sNy+1)
C                      |
C      ----------------|-----------
C                      |
C                      |   x T(1,sNy)
C                      |
C
C      o NE corner: copy T(sNx+1,sNy  ) into T(sNx+1,sNy+1) e.g.
C                      |
C      x T(sNx,sNy+1)=====>x T(sNx+1,sNy+1)
C                      |
C      ----------------|-----------
C                      |
C      x T(sNx,sNy)    |
C                      |
C      o SE corner: copy T(sNx+1,1    ) into T(sNx+1,0    ) e.g.
C                      |
C         x T(sNx,1)   |
C                      |
C      ----------------|-----------
C                      |
C         x T(sNx,0) =====>x T(sNx+1,    0)
         IF ( southWestCorner ) THEN
          DO j=1,Oly
           DO i=1,Olx
            trFld( 1-i , 1-j ) = negOne*trFld(   j   , 1-i )
           ENDDO
          ENDDO
         ENDIF
         IF ( southEastCorner ) THEN
          DO j=1,Oly
           DO i=1,Olx
            trFld(sNx+i, 1-j ) = negOne*trFld(sNx+1-j, 1-i )
           ENDDO
          ENDDO
         ENDIF
         IF ( northWestCorner ) THEN
          DO j=1,Oly
           DO i=1,Olx
            trFld( 1-i ,sNy+j) = negOne*trFld(   j   ,sNy+i)
           ENDDO
          ENDDO
         ENDIF
         IF ( northEastCorner ) THEN
          DO j=1,Oly
           DO i=1,Olx
            trFld(sNx+i,sNy+j) = negOne*trFld(sNx+1-j,sNy+i)
           ENDDO
          ENDDO
         ENDIF

C-     End of Y direction case.
       ELSE
         STOP 'FILL_CS_CORNER_TR_RL: fill4dir has illegal value'
       ENDIF

C--   End useCubedSphereExchange
      ENDIF

      RETURN
      END
