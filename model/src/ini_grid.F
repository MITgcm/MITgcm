C $Header: /u/gcmpack/MITgcm/model/src/ini_grid.F,v 1.6 1998/12/09 16:11:52 adcroft Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE INI_GRID( myThid )
C     /==========================================================\
C     | SUBROUTINE INI_GRID                                      |
C     | o Initialise model grid                                  |
C     |==========================================================|
C     | These arrays are used throughout the code in evaluating  |
C     | gradients, integrals and spatial avarages. This routine  |
C     | is called separately by each thread and initialise only  |
C     | the region of the domain it is "responsible" for.        |
C     | Notes:                                                   |
C     | Two examples are shown in this code. One illustrates the |
C     | initialisation of a cartesian grid. The other shows the  |
C     | inialisation of a spherical polar grid. Other orthonormal|
C     | grids can be fitted into this design. In this case       |
C     | custom metric terms also need adding to account for the  |
C     | projections of velocity vectors onto these grids.        |
C     | The structure used here also makes it possible to        |
C     | implement less regular grid mappings. In particular      |
C     | o Schemes which leave out blocks of the domain that are  |
C     |   all land could be supported.                           |
C     | o Multi-level schemes such as icosohedral or cubic       |
C     |   grid projectedions onto a sphere can also be fitted    |
C     |   within the strategy we use.                            |
C     |   Both of the above also require modifying the support   |
C     |   routines that map computational blocks to simulation   |
C     |   domain blocks.                                         |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_GRID
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     msgBuf - Used for informational I/O.
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Set up vertical grid and coordinate system
      CALL INI_VERTICAL_GRID( myThid )

C--   Set up horizontal grid and coordinate system
      IF ( usingCartesianGrid ) THEN
       CALL INI_CARTESIAN_GRID( myThid )
      ELSEIF ( usingSphericalPolarGrid ) THEN
       CALL INI_SPHERICAL_POLAR_GRID( myThid )
      ELSE
       _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A)') 
     &   'S/R INI_GRID: No grid coordinate system has been selected'
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R INI_GRID'
       _END_MASTER(myThid)
      ENDIF
C
      RETURN
      END
