C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_som_prep_cs_corner.F,v 1.2 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_SOM_PREP_CS_CORNER
C     !INTERFACE:
      SUBROUTINE GAD_SOM_PREP_CS_CORNER(
     U           smVol, smTr0, smTr, smCorners,
     I           prep4dirX, overlapOnly, interiorOnly,
     I           N_edge, S_edge, E_edge, W_edge,
     I           iPass, k, myNz, bi, bj, myThid )


C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_SOM_PREP_CS_CORNER
C     | o Prepare for Horizontal SOM Advection :
C     |   when using Cubed-Sphere Grid, fill corner-halo regions
C     |   of all Tracer-moments with proper values
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     smVol      :: grid-cell volume
C     smTr0      :: tracer Zero Order moment
C     smTr       :: tracer 1rst & 2nd Order moments
C     smCorners  :: Temporary storage of Corner-halo-regions values
C                   ( 3rd dim = Number of corners = 4 : SW, SE, NE, NW )
C     prep4dirX  :: True = prepare for X direction advection
C                   otherwise, prepare for Y direction advection.
C  overlapOnly   :: only update the edges of myTile, but not the interior
C  interiorOnly  :: only update the interior of myTile, but not the edges
C [N,S,E,W]_edge :: true if N,S,E,W edge of myTile is an Edge of the cube
C     iPass      :: current passage index in SOM_ADVECT
C     k          :: current level index
C     myNz       :: 3rd dimension of array to exchange
C     bi,bj      :: current tile indices
C     myThid     :: my Thread Id number
      INTEGER myNz
      _RL     smVol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz)
      _RL     smTr0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz)
      _RL     smTr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,myNz,nSx,nSy,nSOM)
      _RL     smCorners(OLx,OLy,4,-1:nSOM)
      LOGICAL prep4dirX, overlapOnly, interiorOnly
      LOGICAL N_edge, S_edge, E_edge, W_edge
      INTEGER iPass, k, bi, bj
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j, jPass, n
      LOGICAL southWestCorner
      LOGICAL southEastCorner
      LOGICAL northEastCorner
      LOGICAL northWestCorner

      southWestCorner = S_edge .AND. W_edge
      southEastCorner = S_edge .AND. E_edge
      northEastCorner = N_edge .AND. E_edge
      northWestCorner = N_edge .AND. W_edge

      IF ( overlapOnly ) THEN
C--   to avoid repeating 2 times the full sequence of FILL_CS_CORNER calls,
C     add a loop on jPass (from iPass to 2) to reproduce the following logic:
C     1 ) overlapOnly & iPass = 1  (face 3 & 6)
C         - fill corners for the other direction
C         - then store the corner values
C     2 ) IF overlapOnly           :: fill corners for the current direction
C         ELSEIF .NOT.interiorOnly :: get the corner values back from storage
C         ENDIF
       DO jPass = iPass,2

        IF ( ( jPass.EQ.2 .AND. prep4dirX ) .OR.
     &       ( jPass.EQ.1 .AND. .NOT.prep4dirX ) ) THEN
C--   Fill corners to prepare for calculations in X
          CALL GAD_SOM_FILL_CS_CORNER( .TRUE.,
     U                                 smVol(1-OLx,1-OLy,k),
     U                                 smTr0(1-OLx,1-OLy,k),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,1),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,2),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,3),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,4),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,5),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,6),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,7),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,8),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,9),
     I                                 bi, bj, myThid )
C--   End of filling for X dir
c       ENDIF

        ELSE
C Note: the 2 IF tests are equivalent to just 1 if/else test;
C       use this later option and leave the former commented.

c       IF ( ( jPass.EQ.1 .AND. prep4dirX ) .OR.
c    &       ( jPass.EQ.2 .AND. .NOT.prep4dirX ) ) THEN
C--   Fill corners to prepare for calculations in Y
          CALL GAD_SOM_FILL_CS_CORNER( .FALSE.,
     U                                 smVol(1-OLx,1-OLy,k),
     U                                 smTr0(1-OLx,1-OLy,k),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,1),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,2),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,3),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,4),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,5),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,6),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,7),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,8),
     U                                 smTr(1-OLx,1-OLy,k,bi,bj,9),
     I                                 bi, bj, myThid )
C--   End of filling for Y dir
        ENDIF

        IF ( jPass.EQ.1 ) THEN
C--   Store corner values (to be used on the next iPass)
         IF ( southWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,1,-1) = smVol(i-OLx,j-OLy,k)
            smCorners(i,j,1, 0) = smTr0(i-OLx,j-OLy,k)
            DO n=1,nSOM
             smCorners(i,j,1,n) = smTr (i-OLx,j-OLy,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF ( southEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,2,-1) = smVol(sNx+i,j-OLy,k)
            smCorners(i,j,2, 0) = smTr0(sNx+i,j-OLy,k)
            DO n=1,nSOM
             smCorners(i,j,2,n) = smTr (sNx+i,j-OLy,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF ( northEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,3,-1) = smVol(sNx+i,sNy+j,k)
            smCorners(i,j,3, 0) = smTr0(sNx+i,sNy+j,k)
            DO n=1,nSOM
             smCorners(i,j,3,n) = smTr (sNx+i,sNy+j,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
         IF ( northWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smCorners(i,j,4,-1) = smVol(i-OLx,sNy+j,k)
            smCorners(i,j,4, 0) = smTr0(i-OLx,sNy+j,k)
            DO n=1,nSOM
             smCorners(i,j,4,n) = smTr (i-OLx,sNy+j,k,bi,bj,n)
            ENDDO
           ENDDO
          ENDDO
         ENDIF
C--   End storing block
        ENDIF

C--   End of loop on jPass
       ENDDO

      ELSEIF ( .NOT.interiorOnly ) THEN

C--   Get back corner values from storage
        IF ( southWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(i-OLx,j-OLy,k )        = smCorners(i,j,1,-1)
            smTr0(i-OLx,j-OLy,k )        = smCorners(i,j,1, 0)
            DO n=1,nSOM
             smTr(i-OLx,j-OLy,k,bi,bj,n) = smCorners(i,j,1, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
        IF ( southEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(sNx+i,j-OLy,k )        = smCorners(i,j,2,-1)
            smTr0(sNx+i,j-OLy,k )        = smCorners(i,j,2, 0)
            DO n=1,nSOM
             smTr(sNx+i,j-OLy,k,bi,bj,n) = smCorners(i,j,2, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
        IF ( northEastCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(sNx+i,sNy+j,k )        = smCorners(i,j,3,-1)
            smTr0(sNx+i,sNy+j,k )        = smCorners(i,j,3, 0)
            DO n=1,nSOM
             smTr(sNx+i,sNy+j,k,bi,bj,n) = smCorners(i,j,3, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
        IF ( northWestCorner ) THEN
          DO j=1,OLy
           DO i=1,OLx
            smVol(i-OLx,sNy+j,k )        = smCorners(i,j,4,-1)
            smTr0(i-OLx,sNy+j,k )        = smCorners(i,j,4, 0)
            DO n=1,nSOM
             smTr(i-OLx,sNy+j,k,bi,bj,n) = smCorners(i,j,4, n)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
C--   End getting back corner values from storage

C---  End if/else - overlapOnly - block
      ENDIF

      RETURN
      END
