C $Header: /u/gcmpack/MITgcm/model/src/ini_grid.F,v 1.33 2010/03/16 00:08:27 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_GRID

C     !INTERFACE:
      SUBROUTINE INI_GRID( myThid )
C     !DESCRIPTION:
C     These arrays are used throughout the code in evaluating gradients,
C     integrals and spatial avarages. This routine is called separately
C     by each thread and initializes only the region of the domain it is
C     "responsible" for.

C     !CALLING SEQUENCE:
C     INI_GRID
C      |   -- LOAD_GRID_SPACING
C      |   -- INI_VERTICAL_GRID
C      |    / INI_CARTESIAN_GRID
C      |   /  INI_SPHERICAL_POLAR_GRID
C      |   \  INI_CURVILINEAR_GRID
C      |    \ INI_CYLINDER_GRID

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif
#ifdef ALLOW_MONITOR
#include "MONITOR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: my Thread Id number
      INTEGER myThid
CEOP

C     === Functions ====
      LOGICAL  MASTER_CPU_IO
      EXTERNAL MASTER_CPU_IO

C     !LOCAL VARIABLES:
C     msgBuf  :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_MNC
      INTEGER i
#endif

C     load grid spacing (vector) from files
      CALL LOAD_GRID_SPACING( myThid )

C     Set up vertical grid and coordinate system
      CALL INI_VERTICAL_GRID( myThid )

C     Two examples are shown in this code. One illustrates the
C     initialization of a cartesian grid. The other shows the
C     inialization of a spherical polar grid. Other orthonormal grids
C     can be fitted into this design. In this case custom metric terms
C     also need adding to account for the projections of velocity
C     vectors onto these grids.  The structure used here also makes it
C     possible to implement less regular grid mappings. In particular:
C      o Schemes which leave out blocks of the domain that are
C        all land could be supported.
C      o Multi-level schemes such as icosohedral or cubic
C        grid projectedions onto a sphere can also be fitted
C       within the strategy we use.
C        Both of the above also require modifying the support
C        routines that map computational blocks to simulation
C        domain blocks.

C     Set up horizontal grid and coordinate system
      IF ( usingCartesianGrid ) THEN
        CALL INI_CARTESIAN_GRID( myThid )
      ELSEIF ( usingSphericalPolarGrid ) THEN
        CALL INI_SPHERICAL_POLAR_GRID( myThid )
      ELSEIF ( usingCurvilinearGrid ) THEN
        CALL INI_CURVILINEAR_GRID( myThid )
      ELSEIF ( usingCylindricalGrid ) THEN
        CALL INI_CYLINDER_GRID( myThid )
      ELSE
        _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(2A)') 'S/R INI_GRID: ',
     &       'No grid coordinate system has been selected'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R INI_GRID'
        _END_MASTER(myThid)
      ENDIF

#ifdef ALLOW_MONITOR
      IF ( MASTER_CPU_IO(myThid) ) THEN
C--   only the master thread is allowed to switch On/Off mon_write_stdout
C     & mon_write_mnc (since it is the only thread that uses those flags):

        IF (monitor_stdio) THEN
          mon_write_stdout = .TRUE.
        ELSE
          mon_write_stdout = .FALSE.
        ENDIF
        mon_write_mnc = .FALSE.
#ifdef ALLOW_MNC
        IF (useMNC .AND. monitor_mnc) THEN
          DO i = 1,MAX_LEN_MBUF
            mon_fname(i:i) = ' '
          ENDDO
          mon_fname(1:12) = 'monitor_grid'
          CALL MNC_CW_SET_UDIM(mon_fname, 1, myThid)
          mon_write_mnc = .TRUE.
        ENDIF
#endif /*  ALLOW_MNC  */

      ENDIF

C     Print out statistics of each horizontal grid array (helps when
C     debugging)
      CALL MON_PRINTSTATS_RS(1,xC,'XC',myThid)
      CALL MON_PRINTSTATS_RS(1,xG,'XG',myThid)
      CALL MON_PRINTSTATS_RS(1,dxC,'DXC',myThid)
      CALL MON_PRINTSTATS_RS(1,dxF,'DXF',myThid)
      CALL MON_PRINTSTATS_RS(1,dxG,'DXG',myThid)
      CALL MON_PRINTSTATS_RS(1,dxV,'DXV',myThid)
      CALL MON_PRINTSTATS_RS(1,yC,'YC',myThid)
      CALL MON_PRINTSTATS_RS(1,yG,'YG',myThid)
      CALL MON_PRINTSTATS_RS(1,dyC,'DYC',myThid)
      CALL MON_PRINTSTATS_RS(1,dyF,'DYF',myThid)
      CALL MON_PRINTSTATS_RS(1,dyG,'DYG',myThid)
      CALL MON_PRINTSTATS_RS(1,dyU,'DYU',myThid)
      CALL MON_PRINTSTATS_RS(1,rA,'RA',myThid)
      CALL MON_PRINTSTATS_RS(1,rAw,'RAW',myThid)
      CALL MON_PRINTSTATS_RS(1,rAs,'RAS',myThid)
      CALL MON_PRINTSTATS_RS(1,rAz,'RAZ',myThid)
      CALL MON_PRINTSTATS_RS(1,angleCosC,'AngleCS',myThid)
      CALL MON_PRINTSTATS_RS(1,angleSinC,'AngleSN',myThid)

      IF ( MASTER_CPU_IO(myThid) ) THEN
        mon_write_stdout = .FALSE.
        mon_write_mnc    = .FALSE.
      ENDIF
#endif /* ALLOW_MONITOR */

C--   Everyone else must wait for the grid to be set
      _BARRIER

      RETURN
      END
