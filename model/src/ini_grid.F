C $Header: /u/gcmpack/MITgcm/model/src/ini_grid.F,v 1.16 2004/05/13 15:40:53 adcroft Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_GRID
C     !INTERFACE:
      SUBROUTINE INI_GRID( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_GRID                                       
C     | o Initialise model grid                                   
C     *==========================================================*
C     | These arrays are used throughout the code in evaluating   
C     | gradients, integrals and spatial avarages. This routine   
C     | is called separately by each thread and initialise only   
C     | the region of the domain it is "responsible" for.         
C     | Notes:                                                    
C     | Two examples are shown in this code. One illustrates the  
C     | initialisation of a cartesian grid. The other shows the   
C     | inialisation of a spherical polar grid. Other orthonormal 
C     | grids can be fitted into this design. In this case        
C     | custom metric terms also need adding to account for the   
C     | projections of velocity vectors onto these grids.         
C     | The structure used here also makes it possible to         
C     | implement less regular grid mappings. In particular       
C     | o Schemes which leave out blocks of the domain that are   
C     |   all land could be supported.                            
C     | o Multi-level schemes such as icosohedral or cubic        
C     |   grid projectedions onto a sphere can also be fitted     
C     |   within the strategy we use.                             
C     |   Both of the above also require modifying the support    
C     |   routines that map computational blocks to simulation    
C     |   domain blocks.                                          
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_GRID
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     msgBuf - Used for informational I/O.
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_EXF
      INTEGER i, j, bi, bj
#endif /* ALLOW_EXF */
CEOP

C--   Set up vertical grid and coordinate system
      CALL INI_VERTICAL_GRID( myThid )

C--   Set up horizontal grid and coordinate system
      IF ( usingCartesianGrid ) THEN
       CALL INI_CARTESIAN_GRID( myThid )
      ELSEIF ( usingSphericalPolarGrid ) THEN
       CALL INI_SPHERICAL_POLAR_GRID( myThid )
      ELSEIF ( usingCurvilinearGrid ) THEN
       CALL INI_CURVILINEAR_GRID( myThid )
      ELSE
       _BEGIN_MASTER(myThid)
        WRITE(msgBuf,'(A)') 
     &   'S/R INI_GRID: No grid coordinate system has been selected'
         CALL PRINT_ERROR( msgBuf , myThid)
         STOP 'ABNORMAL END: S/R INI_GRID'
       _END_MASTER(myThid)
      ENDIF

#ifdef ALLOW_EXF
C--   exf_interp assumes that 0 <= xG, xC <= 360
C     This is a quick fix until this assumption is relaxed
C     and the interpolation weights are pre-computed.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          IF ( xG(I,J,bi,bj) .LT. 0 )
     &           xG(I,J,bi,bj) = xG(I,J,bi,bj) + 360
          IF ( xC(I,J,bi,bj) .LT. 0 )
     &           xC(I,J,bi,bj) = xC(I,J,bi,bj) + 360
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_EXF */

C--   Write certain grid data to files (useful for creating netCDF
C     and general post-analysis)
C     This I/O is now done in write_grid.F
c     CALL WRITE_FLD_XY_RS( 'XC',' ',XC,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'YC',' ',YC,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'XG',' ',XG,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'YG',' ',YG,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'RAC',' ',rA,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'RAW',' ',rAw,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'RAS',' ',rAs,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'DXG',' ',DXG,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'DYG',' ',DYG,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'DXC',' ',DXC,0,myThid)
c     CALL WRITE_FLD_XY_RS( 'DYC',' ',DYC,0,myThid)

C--   Print out statistics of each horizontal grid array (helps when debugging)

#ifdef ALLOW_MONITOR
      CALL MON_PRINTSTATS_RS(1,XC,'XC',myThid)
      CALL MON_PRINTSTATS_RS(1,XG,'XG',myThid)
      CALL MON_PRINTSTATS_RS(1,DXC,'DXC',myThid)
      CALL MON_PRINTSTATS_RS(1,DXF,'DXF',myThid)
      CALL MON_PRINTSTATS_RS(1,DXG,'DXG',myThid)
      CALL MON_PRINTSTATS_RS(1,DXV,'DXV',myThid)
      CALL MON_PRINTSTATS_RS(1,YC,'YC',myThid)
      CALL MON_PRINTSTATS_RS(1,YG,'YG',myThid)
      CALL MON_PRINTSTATS_RS(1,DYC,'DYC',myThid)
      CALL MON_PRINTSTATS_RS(1,DYF,'DYF',myThid)
      CALL MON_PRINTSTATS_RS(1,DYG,'DYG',myThid)
      CALL MON_PRINTSTATS_RS(1,DYU,'DYU',myThid)
      CALL MON_PRINTSTATS_RS(1,RA,'RA',myThid)
      CALL MON_PRINTSTATS_RS(1,RAW,'RAW',myThid)
      CALL MON_PRINTSTATS_RS(1,RAS,'RAS',myThid)
      CALL MON_PRINTSTATS_RS(1,RAZ,'RAZ',myThid)
#endif

      RETURN
      END
