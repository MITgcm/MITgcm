C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_write_grid_info.F,v 1.3 2004/03/19 03:28:36 edhill Exp $
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

      CALL MNC_CW_ADD_VNAME('xC', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('yC', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('xG', 'Cor_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('yG', 'Cor_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxC', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyC', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxF', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyF', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxG', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyG', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dxV', 'U_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('dyU', 'V_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rA', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAw', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAs', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('rAz', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('fCori', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('fCoriG', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('R_low', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('Ro_surf', 'Cen_xy_Hy__-__-', 3,4, myThid)
      CALL MNC_CW_ADD_VNAME('HFacC', 'Cen_xy_Hy__C__-', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('HFacW', 'Cen_xy_Hy__C__-', 4,5, myThid)
      CALL MNC_CW_ADD_VNAME('HFacS', 'Cen_xy_Hy__C__-', 4,5, myThid)

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
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rC',0,rC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rF',0,rF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'drC',0,drC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'drF',0,drF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'saFac',0,saFac,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'xC',0,xC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'yC',0,yC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'xG',0,xG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'yG',0,yG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxC',0,dxC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyC',0,dyC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxF',0,dxF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyF',0,dyF,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxG',0,dxG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyG',0,dyG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dxV',0,dxV,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'dyU',0,dyU,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rA',0,rA,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rAw',0,rAw,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rAs',0,rAs,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'rAz',0,rAz,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'fCori',0,fCori,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'fCoriG',0,fCoriG,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'R_low',0,R_low,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'Ro_surf',0,Ro_surf,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'HFacC',0,HFacC,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'HFacW',0,HFacW,myThid)
      CALL MNC_CW_RL_W_D(bfname(nf:nl),0,0,'HFacS',0,HFacS,myThid)


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

