C $Header: /u/gcmpack/MITgcm/model/src/write_grid.F,v 1.1 2004/05/13 15:40:53 adcroft Exp $
C $Name:  $
      
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: WRITE_GRID
C     !INTERFACE:
      SUBROUTINE WRITE_GRID(
     I     myThid )

C     !DESCRIPTION: \bv
C     write_grid() writes the model geometry/grid arrays to file(s) using
C     which ever I/O package is available/active.
C      - this includes horizontal/vertical grid
C      - and finite volume modulating factors (hFacs)
C     To all intents and purposes it dumps the contents of GRID.h to disk.
C     \ev

C     !CALLING SEQUENCE:
C     INITIALIZE_FIXED
C       |
C       |-- WRITE_GRID

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "mnc_common.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid -  Number of this instances
      INTEGER myThid

C     !LOCAL VARIABLES:
      character*(4) bfname
C     tmpfld  - Temporary array used to compute & write Total Depth
C               has to be in common for multi threading
C     ?aja: not sure why the COMMON block is necessary - should ask JMC?
      COMMON / LOCAL_INI_MASKS_ETC / tmpfld
      _RS tmpfld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER i,j,bi,bj
CEOP

C-- Calculate fluid thickness in R coordinates as seen by model
C   (This may differ from what the user specified due to partial cells etc.)
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
C         Total fluid column thickness (r_unit) :
          tmpfld(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _BEGIN_MASTER( myThid )
C     Write horizontal grid arrays
      CALL WRITE_FLD_XY_RS( 'XC',' ',XC,0,myThid)
      CALL WRITE_FLD_XY_RS( 'YC',' ',YC,0,myThid)
      CALL WRITE_FLD_XY_RS( 'XG',' ',XG,0,myThid)
      CALL WRITE_FLD_XY_RS( 'YG',' ',YG,0,myThid)
      CALL WRITE_FLD_XY_RS( 'RAC',' ',rA,0,myThid)
      CALL WRITE_FLD_XY_RS( 'RAW',' ',rAw,0,myThid)
      CALL WRITE_FLD_XY_RS( 'RAS',' ',rAs,0,myThid)
      CALL WRITE_FLD_XY_RS( 'DXG',' ',DXG,0,myThid)
      CALL WRITE_FLD_XY_RS( 'DYG',' ',DYG,0,myThid)
      CALL WRITE_FLD_XY_RS( 'DXC',' ',DXC,0,myThid)
      CALL WRITE_FLD_XY_RS( 'DYC',' ',DYC,0,myThid)
C     Write 3D geometry arrays
      CALL WRITE_FLD_XY_RS( 'Depth',' ',tmpfld,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacC',' ',hFacC,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacW',' ',hFacW,0,myThid)
      CALL WRITE_FLD_XYZ_RS( 'hFacS',' ',hFacS,0,myThid)
      IF (buoyancyRelation .EQ. 'ATMOSPHERIC')
     &   CALL WRITE_FLD_XY_RS( 'topo_P',' ',Ro_surf,0,myThid)
      _END_MASTER(myThid)

#ifdef ALLOW_MNC
      IF (useMNC) THEN
      bfname='grid';

C     Create MNC definitions for GRID.h variables

C     Coordinate arrays
C begin block
c     these definitions are made in ini_model_io() since they will be
c     the same in all MNC files
c     CALL MNC_CW_ADD_VNAME('RC', '-_-_--__C__-', 0,0, myThid)
c     CALL MNC_CW_ADD_VNAME('RF', '-_-_--__I__-', 0,0, myThid)
c     CALL MNC_CW_ADD_VNAME('XC', 'Cen_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('YC', 'Cen_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('XU', 'U_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('YU', 'U_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('XV', 'V_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('YV', 'V_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('XG', 'Cor_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VNAME('YG', 'Cor_xy_Hn__-__-', 3,4, myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('RC',1,'description',
c    &     'r of cell center',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('RF',1,'description',
c    &     'r of cell face',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('XC',1,'description',
c    &     'x of cell center',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('YC',1,'description',
c    &     'y of cell center',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('XU',1,'description',
c    &     'x of U point',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('YU',1,'description',
c    &     'y of U point',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('XV',1,'description',
c    &     'x of V point',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('YV',1,'description',
c    &     'y of V point',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('XG',1,'description',
c    &     'x of cell corner',myThid)
c     CALL MNC_CW_ADD_VATTR_TEXT('YG',1,'description',
c    &     'y of cell corner',myThid)
C end block

C     Grid spacing, areas and hFacs
      CALL MNC_CW_ADD_VNAME('drC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drF', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('saFac', '-_-_--__C__-', 0,0, myThid)

      CALL MNC_CW_ADD_VNAME('dxC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxF', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyF', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxG', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyG', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxV', 'U_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyU', 'V_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rA', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAw', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAs', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAz', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('fCori', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('fCoriG', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('R_low', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('Ro_surf', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('Depth', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('HFacC', 'Cen_xy_Hn__C__-', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('HFacW', 'Cen_xy_Hn__C__-', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('HFacS', 'Cen_xy_Hn__C__-', 4,5, myThid)

      CALL MNC_CW_ADD_VATTR_TEXT('drC',1,'description',
     &     'r cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('drF',1,'description',
     &     'r cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('saFac',1,'description',
     &     'shallow atmosphere factor',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxC',1,'description',
     &     'x cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyC',1,'description',
     &     'y cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxF',1,'description',
     &     'x cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyF',1,'description',
     &     'y cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxG',1,'description',
     &     'x cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyG',1,'description',
     &     'y cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxV',1,'description',
     &     'x v-velocity separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyU',1,'description',
     &     'y u-velocity separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rA',1,'description',
     &     'r-face area',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAw',1,'description',
     &     '',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAs',1,'description',
     &     '',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAz',1,'description',
     &     '',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCori',1,'description',
     &     'Coriolis f',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCoriG',1,'description',
     &     'Coriolis f',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('R_low',1,'description',
     &     'base of fluid in r-units',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ro_surf',1,'description',
     &     'surface reference (at rest) position',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Depth',1,'description',
     &     'fluid thickness in r coordinates (at rest)',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacC',1,'description',
     &     'C-facing vertical fraction of open cell',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacW',1,'description',
     &     'W-facing vertical fraction of open cell',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacS',1,'description',
     &     'S-facing vertical fraction of open cell',myThid)

C     Write the GRID.h variables to a file
      CALL MNC_CW_SET_UDIM(bfname, 0, myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'RC',rC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'RF',rF,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'drC',drC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'drF',drF,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'saFac',saFac,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'XC',xC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'YC',yC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'XU',xG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'YU',yC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'XV',xC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'YV',yG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'XG',xG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'YG',yG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dxC',dxC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dyC',dyC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dxF',dxF,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dyF',dyF,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dxG',dxG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dyG',dyG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dxV',dxV,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'dyU',dyU,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'rA',rA,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'rAw',rAw,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'rAs',rAs,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'rAz',rAz,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'fCori',fCori,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'fCoriG',fCoriG,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'R_low',R_low,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'Ro_surf',Ro_surf,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'Depth',tmpfld,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'HFacC',HFacC,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'HFacW',HFacW,myThid)
      CALL MNC_CW_RL_W('D',bfname,0,0,'HFacS',HFacS,myThid)
      ENDIF
#endif /* ALLOW_MNC */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

