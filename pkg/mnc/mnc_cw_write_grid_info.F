C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_write_grid_info.F,v 1.15 2013/07/26 14:23:53 jmc Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C !ROUTINE: MNC_CW_WRITE_GRID_INFO

C !INTERFACE:
      SUBROUTINE MNC_CW_WRITE_GRID_INFO(
     I     bfname,
     I     myThid )

C     !DESCRIPTION:
C     This subroutine writes all the grid information (\textit{eg.}
C     locations, spacings, areas, \textit{etc.}) to one grid file
C     (\texttt{grid.xxxxx.nc}) per tile.

C===================================================================
C  WARNING: This routine is not maintained since it is never called
C===================================================================

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) bfname
CEOP

C     !LOCAL VARIABLES:
      integer nf,nl

C     Functions
      integer IFNBLNK, ILNBLNK

      nf = IFNBLNK(bfname)
      nl = ILNBLNK(bfname)

C===================================================================
C  WARNING: This routine is not maintained since it is never called
C===================================================================
      STOP 'MNC_CW_WRITE_GRID_INFO is not maintained'

C     Create MNC definitions for GRID.h variables
      CALL MNC_CW_ADD_VNAME('rC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('rF', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drF', '-_-_--__C__-', 0,0, myThid)

      CALL MNC_CW_ADD_VNAME('xC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('yC', 'Cen_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('xG', 'Cor_xy_Hn__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('yG', 'Cor_xy_Hn__-__-', 3,4, myThid)
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
      CALL MNC_CW_ADD_VNAME('HFacC', 'Cen_xy_Hn__C__-', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('HFacW', 'Cen_xy_Hn__C__-', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('HFacS', 'Cen_xy_Hn__C__-', 4,5, myThid)

      CALL MNC_CW_ADD_VATTR_TEXT('rC','description',
     &     'r of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rF','description',
     &     'r of cell face',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('drC','description',
     &     'r cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('drF','description',
     &     'r cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xC','description',
     &     'x of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yC','description',
     &     'y of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xG','description',
     &     'x of cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yG','description',
     &     'y of cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxC','description',
     &     'x cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyC','description',
     &     'y cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxF','description',
     &     'x cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyF','description',
     &     'y cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxG','description',
     &     'x cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyG','description',
     &     'y cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dxV','description',
     &     'x v-velocity separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('dyU','description',
     &     'y u-velocity separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rA','description',
     &     'r-face area',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAw','description',
     &     '',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAs','description',
     &     '',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rAz','description',
     &     '',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCori','description',
     &     'Coriolis f',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('fCoriG','description',
     &     'Coriolis f',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('R_low','description',
     &     'base of fluid in r-units',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('Ro_surf','description',
     &     'surface reference (at rest,myThid) position',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacC','description',
     &     'C-facing vertical fraction of open cell',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacW','description',
     &     'W-facing vertical fraction of open cell',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacS','description',
     &     'S-facing vertical fraction of open cell',myThid)

C     Write the GRID.h variables
      CALL MNC_CW_SET_UDIM(bfname(nf:nl), 0, myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'rC',rC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'rF',rF,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'drC',drC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'drF',drF,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'xC',xC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'yC',yC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'xG',xG,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'yG',yG,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dxC',dxC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dyC',dyC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dxF',dxF,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dyF',dyF,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dxG',dxG,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dyG',dyG,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dxV',dxV,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'dyU',dyU,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'rA',rA,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'rAw',rAw,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'rAs',rAs,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'rAz',rAz,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'fCori',fCori,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'fCoriG',fCoriG,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'R_low',R_low,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'Ro_surf',Ro_surf,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'HFacC',HFacC,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'HFacW',HFacW,myThid)
      CALL MNC_CW_RS_W('D',bfname(nf:nl),0,0,'HFacS',HFacS,myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C !ROUTINE: MNC_CW_WRITE_GRID_COORD

C !INTERFACE:
      SUBROUTINE MNC_CW_WRITE_GRID_COORD(
     I     fname,
     I     myThid )

C     !DESCRIPTION:
C     This subroutine writes coordinates to one netcdf file
C     (\texttt{fname.xxxxx.nc}) per tile.

C     !USES:
      implicit none
#include "SIZE.h"
#include "GRID.h"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP
C     Write coordinates to file "fname"
      _BEGIN_MASTER( myThid )

      CALL MNC_CW_SET_UDIM(fname, 0, myThid)
      CALL MNC_CW_RS_W('R',fname,0,0,'XC',xC, myThid)
      CALL MNC_CW_RS_W('R',fname,0,0,'YC',yC, myThid)
      CALL MNC_CW_RS_W('R',fname,0,0,'XG',xG, myThid)
      CALL MNC_CW_RS_W('R',fname,0,0,'YG',yG, myThid)
C--   Here we can add as much grid information as we like, but this
C--   will increase the size of each of the affected files. It is
C--   a matter of taste, but here are some candidates:
C--   Probably it is enough to write the coordinates of the
C--   center points and the corners.
C     CALL MNC_CW_RS_W('R',fname,0,0,'XU',xG, myThid)
C     CALL MNC_CW_RS_W('R',fname,0,0,'YU',yC, myThid)
C     CALL MNC_CW_RS_W('R',fname,0,0,'XV',xC, myThid)
C     CALL MNC_CW_RS_W('R',fname,0,0,'YV',yG, myThid)
C--   Only required for diagnostics files, but unclear what the vertical
C--   coordinate means in this case, so do not write anything for now
C     CALL MNC_CW_RS_W('R',fname,0,0,'RC',rC, myThid)
C     CALL MNC_CW_RS_W('R',fname,0,0,'RF',rF, myThid)
C--   If required write mask information.
C     CALL MNC_CW_RS_W('R',fname,0,0,'HFacC',HFacC,myThid)

      _END_MASTER(myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
