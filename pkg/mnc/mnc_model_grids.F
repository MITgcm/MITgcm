C $Header: /u/gcmpack/MITgcm/pkg/mnc/Attic/mnc_model_grids.F,v 1.2 2004/01/15 04:31:24 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_W_GRIDINFO( 
     I     myThid, 
     I     RefName )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"

#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"

C     Arguments
      integer myThid
      character*(*) RefName

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k,ii, n, bi,bj, iG,jG, rnX,rnY
      integer istart(3), iend(3)
      character*(MNC_MAX_CHAR) fname, gnames(20),dnames(20)
      _RS trs((sNx+2*OLx)*(sNy+2*OLy)*(Nr+1))
      _RL trl((sNx+2*OLx)*(sNy+2*OLy)*(Nr+1))

C     Initialize strings
      fname(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      DO i = 1,20
        gnames(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        dnames(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      ENDDO

C     Create the dimensions
      dnames(11)(1:9) = 'X        '
      dnames(12)(1:9) = 'Y        '
      dnames(13)(1:9) = 'R        '
      dnames(14)(1:9) = 'Xp1      '
      dnames(15)(1:9) = 'Yp1      '
      dnames(16)(1:9) = 'Rp1      '
      dnames(17)(1:9) = 'Xm1      '
      dnames(18)(1:9) = 'Ym1      '
      dnames(19)(1:9) = 'Rm1      '
      CALL MNC_DIM_INIT(myThid, dnames(11), dnames(11), sNx)
      CALL MNC_DIM_INIT(myThid, dnames(12), dnames(12), sNy)
      CALL MNC_DIM_INIT(myThid, dnames(13), dnames(13), Nr)
      CALL MNC_DIM_INIT(myThid, dnames(14), dnames(14), sNx+1)
      CALL MNC_DIM_INIT(myThid, dnames(15), dnames(15), sNy+1)
      CALL MNC_DIM_INIT(myThid, dnames(16), dnames(16), Nr+1)
      CALL MNC_DIM_INIT(myThid, dnames(17), dnames(14), sNx-1)
      CALL MNC_DIM_INIT(myThid, dnames(18), dnames(15), sNy-1)
      CALL MNC_DIM_INIT(myThid, dnames(19), dnames(16), Nr-1)
      
C     grid names         123456789
      gnames( 1)(1:9) = 'center   '
      gnames( 2)(1:9) = 'u        '
      gnames( 3)(1:9) = 'v        '
      gnames( 4)(1:9) = 'w        '
      gnames( 5)(1:9) = 'zeta     '
      gnames( 6)(1:9) = 'center_r '
      gnames( 7)(1:9) = 'u_r      '
      gnames( 8)(1:9) = 'v_r      '
      gnames( 9)(1:9) = 'w_r      '
      gnames(10)(1:9) = 'zeta_r   '
      gnames(11)(1:9) = 'r        '
      gnames(12)(1:9) = 'rp1      '

C     Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C     Output a grid file for all tiles owned by this thread or process
      DO bj = 1,nSy
        DO bi = 1,nSx

C         Kludge until unstructered tiles -- mimics mdsio tile numbering
          iG = bi+(myXGlobalLo-1)/sNx
          jG = bj+(myYGlobalLo-1)/sNy
          
          n = ILNBLNK(RefName)
          write(fname(1:100),'(3a,i3.3,a,i3.3,a)') "grid_",
     &         RefName(1:n), '-',iG,'-',jG,'.nc '
          CALL MNC_FILE_CREATE(myThid, fname)

C         Attributes
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'mitgcm_ver',
     &         'Put CVS version info here.' )
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'mitgcm_grid_ver', 
     &         1, 0.1D0 )
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'reference',
     &         'Put reference string here.' )
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'author',
     &         'Put author string here.' )
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'date',
     &         'Put date string here.' )
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'xC0', 1, xC0)
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'yC0', 1, yC0)
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'gravitySign', 
     &         1, gravitySign )
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'rkFac', 1, rkFac)

C         Create the grids
C         center
          dnames(1)(1:MNC_MAX_CHAR) = dnames(11)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(12)(1:MNC_MAX_CHAR)
          dnames(3)(1:MNC_MAX_CHAR) = dnames(13)(1:MNC_MAX_CHAR)
          CALL MNC_GRID_INIT(myThid, fname, gnames(1), 2, dnames)
          CALL MNC_GRID_INIT(myThid, fname, gnames(6), 3, dnames)
C         u
          dnames(1)(1:MNC_MAX_CHAR) = dnames(14)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(12)(1:MNC_MAX_CHAR)
          CALL MNC_GRID_INIT(myThid, fname, gnames(2), 2, dnames)
          CALL MNC_GRID_INIT(myThid, fname, gnames(7), 3, dnames)
C         v
          dnames(1)(1:MNC_MAX_CHAR) = dnames(11)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(15)(1:MNC_MAX_CHAR)
          CALL MNC_GRID_INIT(myThid, fname, gnames(3), 2, dnames)
          CALL MNC_GRID_INIT(myThid, fname, gnames(8), 3, dnames)
C         w
          dnames(1)(1:MNC_MAX_CHAR) = dnames(11)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(12)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(16)(1:MNC_MAX_CHAR)
          CALL MNC_GRID_INIT(myThid, fname, gnames(4), 2, dnames)
          CALL MNC_GRID_INIT(myThid, fname, gnames(9), 3, dnames)
C         zeta (vorticity)
          dnames(1)(1:MNC_MAX_CHAR) = dnames(11)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(12)(1:MNC_MAX_CHAR)
          dnames(2)(1:MNC_MAX_CHAR) = dnames(16)(1:MNC_MAX_CHAR)
          CALL MNC_GRID_INIT(myThid, fname, gnames(5), 2, dnames)
          CALL MNC_GRID_INIT(myThid, fname, gnames(10), 3, dnames)
C         r, rp1
          CALL MNC_GRID_INIT(myThid, fname, gnames(11),1,dnames(13))
          CALL MNC_GRID_INIT(myThid, fname, gnames(12),1,dnames(16))

C         Indicies and sizes for extracting the data -- we need to skip
C         over the exchange regions
          rnX = sNx + 2*OLx
          rnY = sNy + 2*OLy
          istart(1) = OLx + 1
          istart(2) = OLy + 1
          istart(3) = 1
          iend(1) = sNx + OLx
          iend(2) = sNy + OLy
          istart(3) = Nr

C         Create and write grid parameters
C         rC, rF
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(11),'rC','--')
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'rC', rC)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(12),'rF','--')
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'rF', rF)
C         drC, drF
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(11),'drC','--')
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'drC', drC)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(11),'drF','--')
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'drF', drF)
C         saFac
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(11),'saFac','--')
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'saFac', saFac)

C         xC, yC
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'xC','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         xC(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'xC', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'yC','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         yC(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'yC', trl)
C         xG, yG
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'xG','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         xG(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'xG', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'yG','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         yG(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'yG', trl)
C         dxC, dyC
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dxC','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dxC(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dxC', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dyC','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dyC(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dyC', trl)
C         dxF, dyF
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dxF','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dxF(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dxF', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dyF','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dyF(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dyF', trl)
C         dxG, dyG
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dxG','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dxG(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dxG', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dyG','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dyG(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dyG', trl)
C         dxV, dyU
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dxV','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dxV(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dxV', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'dyU','m')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         dyU(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'dyU', trl)
C         R_low, Ro_surf
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'R_low','--')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         R_low(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'R_low', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(1),'Ro_surf','--')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         Ro_surf(1-OLx,1-OLy,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'Ro_surf', trl)
C         HFacC, HFacW, HFacS
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(6),'HFacC','--')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         HFacC(1-OLx,1-OLy,1,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'HFacC', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(6),'HFacW','--')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         HFacW(1-OLx,1-OLy,1,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'HFacW', trl)
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnames(6),'HFacS','--')
          CALL MNC_RESHAPE_2DD(rnX, rnY, istart, iend, 
     &         HFacS(1-OLx,1-OLy,1,bi,bj), trl)
          CALL MNC_VAR_WRITE_DBL(myThid, fname, 'HFacS', trl)

C         Close the grid file
          CALL MNC_FILE_CLOSE(myThid, fname)

        ENDDO
      ENDDO

      _END_MASTER( myThid )
      
      RETURN
      END


C==================================================================

      SUBROUTINE MNC_RESHAPE_2DD( ni,nj, istart,iend, din,dout ) 

      implicit none

C     Arguments
      integer ni,nj, istart(2), iend(2)
      _RL din(ni,nj)
      _RL dout(*)

C     Locals
      integer i,j, n

      n = 0
      DO i = istart(1),iend(1)
        DO j = istart(2),iend(2)
          n = n + 1
          dout(n) = din(i,j)
        ENDDO
      ENDDO

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_RESHAPE_2DR( ni,nj, istart,iend, din,dout ) 

      implicit none

C     Arguments
      integer ni,nj, istart(2), iend(2)
      _RS din(ni,nj)
      _RS dout(*)

C     Locals
      integer i,j, n

      n = 0
      DO i = istart(1),iend(1)
        DO j = istart(2),iend(2)
          n = n + 1
          dout(n) = din(i,j)
        ENDDO
      ENDDO

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_RESHAPE_3DD( ni,nj,nk, istart,iend, din,dout ) 

      implicit none

C     Arguments
      integer ni,nj,nk, istart(3), iend(3)
      _RL din(ni,nj,nk)
      _RL dout(*)

C     Locals
      integer i,j,k, n

      n = 0
      DO i = istart(1),iend(1)
        DO j = istart(2),iend(2)
          DO k = istart(3),iend(3)
            n = n + 1
            dout(n) = din(i,j,k)
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_RESHAPE_3DR( ni,nj,nk, istart,iend, din,dout ) 

      implicit none

C     Arguments
      integer ni,nj,nk, istart(3), iend(3)
      _RS din(ni,nj,nk)
      _RS dout(*)

C     Locals
      integer i,j,k, n

      n = 0
      DO i = istart(1),iend(1)
        DO j = istart(2),iend(2)
          DO k = istart(3),iend(3)
            n = n + 1
            dout(n) = din(i,j,k)
          ENDDO
        ENDDO
      ENDDO

      RETURN
      END

