C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_write_grid_info.F,v 1.5 2004/03/21 03:44:23 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_WRITE_GRID_INFO(
     I     bfname,  
     I     myThid )

      implicit none
#include "mnc_common.h"

#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"

C     Arguments
      integer myThid
      character*(*) bfname

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer nf,nl

      nf = IFNBLNK(bfname)
      nl = ILNBLNK(bfname)

C     Create MNC definitions for GRID.h variables
      CALL MNC_CW_ADD_VNAME('rC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('rF', '-_-_--__I__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drC', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('drF', '-_-_--__C__-', 0,0, myThid)
      CALL MNC_CW_ADD_VNAME('saFac', '-_-_--__C__-', 0,0, myThid)

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

      CALL MNC_CW_ADD_VATTR_TEXT('rC',1,'description',
     &     'r of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('rF',1,'description',
     &     'r of cell face',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('drC',1,'description',
     &     'r cell center separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('drF',1,'description',
     &     'r cell face separation',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('saFac',1,'description',
     &     'shallow atmosphere factor',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xC',1,'description',
     &     'x of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yC',1,'description',
     &     'y of cell center',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('xG',1,'description',
     &     'x of cell corner',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('yG',1,'description',
     &     'y of cell corner',myThid)
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
     &     'surface reference (at rest,myThid) position',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacC',1,'description',
     &     'C-facing vertical fraction of open cell',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacW',1,'description',
     &     'W-facing vertical fraction of open cell',myThid)
      CALL MNC_CW_ADD_VATTR_TEXT('HFacS',1,'description',
     &     'S-facing vertical fraction of open cell',myThid)

C     Write the GRID.h variables
      CALL MNC_CW_SET_UDIM(bfname(nf:nl), 0, myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rC',rC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rF',rF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'drC',drC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'drF',drF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'saFac',saFac,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'xC',xC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'yC',yC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'xG',xG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'yG',yG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxC',dxC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyC',dyC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxF',dxF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyF',dyF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxG',dxG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyG',dyG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxV',dxV,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyU',dyU,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rA',rA,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rAw',rAw,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rAs',rAs,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rAz',rAz,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'fCori',fCori,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'fCoriG',fCoriG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'R_low',R_low,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'Ro_surf',Ro_surf,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'HFacC',HFacC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'HFacW',HFacW,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'HFacS',HFacS,myThid)


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

