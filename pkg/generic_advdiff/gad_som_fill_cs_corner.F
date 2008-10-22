C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_som_fill_cs_corner.F,v 1.3 2008/10/22 00:28:47 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_SOM_FILL_CS_CORNER
C     !INTERFACE:
      SUBROUTINE GAD_SOM_FILL_CS_CORNER(
     I           fill4dirX,
     U           sm_v,  sm_o,  sm_x,  sm_y,  sm_z,
     U           sm_xx, sm_yy, sm_zz, sm_xy, sm_xz, sm_yz,
     I           bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_SOM_FILL_CS_CORNER
C     | o Wraper S/R to fill the corner-halo region of CS-grid,
C     |   for all moments of a 1 tracer field
C     *==========================================================*
C     | o the corner halo region is filled with valid values
C     |   in order to compute (later on) gradient in X or Y
C     |   direction, on a wide stencil.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     fill4dirX :: True = prepare for X direction calculations
C                  otherwise, prepare for Y direction
C     sm_v      :: volume of grid cell
C     sm_o      :: tracer content of grid cell (zero order moment)
C     sm_x,y,z  :: 1rst order moment of tracer distribution, in x,y,z direction
C   sm_xx,yy,zz ::  2nd order moment of tracer distribution, in x,y,z direction
C   sm_xy,xz,yz ::  2nd order moment of tracer distr., in cross direction xy,xz,yz
C     bi,bj     :: tile indices
C     myThid    :: my Thread Id. number
      LOGICAL fill4dirX
      _RL sm_v  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_o  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_x  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_y  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_z  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_xx (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_yy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_zz (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_xy (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_xz (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sm_yz (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER selectDir
CEOP

      IF (useCubedSphereExchange) THEN
          IF ( fill4dirX ) THEN
            selectDir = 1
          ELSE
            selectDir = 2
          ENDIF
          CALL FILL_CS_CORNER_TR_RL( selectDir, .FALSE.,
     &                               sm_v , bi,bj, myThid )
          CALL FILL_CS_CORNER_TR_RL( selectDir, .FALSE.,
     &                               sm_o , bi,bj, myThid )
          CALL FILL_CS_CORNER_AG_RL( fill4dirX, .TRUE.,
     &                               sm_x , sm_y , bi,bj, myThid )
          CALL FILL_CS_CORNER_TR_RL( selectDir, .FALSE.,
     &                               sm_z , bi,bj, myThid )
          CALL FILL_CS_CORNER_AG_RL( fill4dirX, .FALSE.,
     &                               sm_xx, sm_yy, bi,bj, myThid )
          CALL FILL_CS_CORNER_TR_RL( selectDir, .FALSE.,
     &                               sm_zz, bi,bj, myThid )
          CALL FILL_CS_CORNER_TR_RL( selectDir, .TRUE.,
     &                               sm_xy, bi,bj, myThid )
          CALL FILL_CS_CORNER_AG_RL( fill4dirX, .TRUE.,
     &                               sm_xz, sm_yz, bi,bj, myThid )
      ENDIF

      RETURN
      END
