C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_write_grid_info.F,v 1.1 2004/02/05 19:38:55 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_WRITE_GRID_INFO(
     I     myThid, 
     I     bfname ) 

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
      CALL MNC_CW_ADD_VNAME(myThid, 'rC', '-_-_--__C__-', 0,0)
      CALL MNC_CW_ADD_VNAME(myThid, 'rF', '-_-_--__I__-', 0,0)
      CALL MNC_CW_ADD_VNAME(myThid, 'drC', '-_-_--__C__-', 0,0)
      CALL MNC_CW_ADD_VNAME(myThid, 'drF', '-_-_--__C__-', 0,0)
      CALL MNC_CW_ADD_VNAME(myThid, 'saFac', '-_-_--__C__-', 0,0)

      CALL MNC_CW_ADD_VNAME(myThid, 'xC', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'yC', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'xG', 'Cor_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'yG', 'Cor_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dxC', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dyC', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dxF', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dyF', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dxG', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dyG', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dxV', 'U_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'dyU', 'V_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'rA', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'rAw', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'rAs', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'rAz', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'fCori', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'fCoriG', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'R_low', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'Ro_surf', 'Cen_xy_Hy__-__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'HFacC', 'Cen_xy_Hy__C__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'HFacW', 'Cen_xy_Hy__C__-', 3,4)
      CALL MNC_CW_ADD_VNAME(myThid, 'HFacS', 'Cen_xy_Hy__C__-', 3,4)

      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'rC',1,'description',
     &     'r of cell center')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'rF',1,'description',
     &     'r of cell face')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'drC',1,'description',
     &     'r cell center separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'drF',1,'description',
     &     'r cell face separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'saFac',1,'description',
     &     'shallow atmosphere factor')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'xC',1,'description',
     &     'x of cell center')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'yC',1,'description',
     &     'y of cell center')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'xG',1,'description',
     &     'x of cell corner')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'yG',1,'description',
     &     'y of cell corner')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dxC',1,'description',
     &     'x cell center separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dyC',1,'description',
     &     'y cell center separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dxF',1,'description',
     &     'x cell face separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dyF',1,'description',
     &     'y cell face separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dxG',1,'description',
     &     'x cell face separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dyG',1,'description',
     &     'y cell face separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dxV',1,'description',
     &     'x v-velocity separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'dyU',1,'description',
     &     'y u-velocity separation')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'rA',1,'description',
     &     'r-face area')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'rAw',1,'description',
     &     '')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'rAs',1,'description',
     &     '')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'rAz',1,'description',
     &     '')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'fCori',1,'description',
     &     'Coriolis f')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'fCoriG',1,'description',
     &     'Coriolis f')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'R_low',1,'description',
     &     'base of fluid in r-units')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'Ro_surf',1,'description',
     &     'surface reference (at rest) position')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'HFacC',1,'description',
     &     'C-facing vertical fraction of open cell')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'HFacW',1,'description',
     &     'W-facing vertical fraction of open cell')
      CALL MNC_CW_ADD_VATTR_TEXT(myThid,'HFacS',1,'description',
     &     'S-facing vertical fraction of open cell')

C     Write the GRID.h variables
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'rC',0,rC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'rF',0,rF)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'drC',0,drC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'drF',0,drF)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'saFac',0,saFac)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'xC',0,xC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'yC',0,yC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'xG',0,xG)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'yG',0,yG)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dxC',0,dxC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dyC',0,dyC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dxF',0,dxF)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dyF',0,dyF)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dxG',0,dxG)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dyG',0,dyG)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dxV',0,dxV)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'dyU',0,dyU)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'rA',0,rA)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'rAw',0,rAw)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'rAs',0,rAs)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'rAz',0,rAz)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'fCori',0,fCori)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'fCoriG',0,fCoriG)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'R_low',0,R_low)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'Ro_surf',0,Ro_surf)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'HFacC',0,HFacC)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'HFacW',0,HFacW)
      CALL MNC_CW_RL_W_D(myThid,bfname(nf:nl),0,0,'HFacS',0,HFacS)


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

