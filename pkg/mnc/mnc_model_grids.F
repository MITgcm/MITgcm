C $Header: /u/gcmpack/MITgcm/pkg/mnc/Attic/mnc_model_grids.F,v 1.6 2004/01/25 00:22:57 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C==================================================================

      SUBROUTINE MNC_W_GRIDINFO( myThid, RefName )

      implicit none
      integer myThid
      character*(*) RefName

      CALL MNC_W_GRIDINFO_ALL( myThid, RefName, 'N', 'D' )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_W_GRIDINFO_ALL( 
     I     myThid, 
     I     RefName, 
     I     withHalo, 
     I     RorD )

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
      character*(1) withHalo, RorD

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, n, bi,bj, iG,jG, rnX,rnY,rnZ, mt
      integer istart(3), iend(3)
      character*(MNC_MAX_CHAR) fnm, atnm, gnms(20),dnms(20)
      character*(MAX_LEN_MBUF) msgbuf
      REAL*4 t_R((sNx+2*OLx)*(sNy+2*OLy)*(Nr+1))
      REAL*8 t_D((sNx+2*OLx)*(sNy+2*OLy)*(Nr+1))

C     Initialize strings
      fnm(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      atnm(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      DO i = 1,20
        gnms(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        dnms(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      ENDDO

C     dim names
      dnms(11)(1:9) = 'X        '
      dnms(12)(1:9) = 'Y        '
      dnms(13)(1:9) = 'R        '
      dnms(14)(1:9) = 'Rp1      '
      
C     grid names       123456789
      gnms( 1)(1:9) = 'center   '
      gnms( 2)(1:9) = 'u        '
      gnms( 3)(1:9) = 'v        '
      gnms( 4)(1:9) = 'w        '

C     Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C     Output a grid file for all tiles owned by this thread or process
      DO bj = 1,nSy
        DO bi = 1,nSx

C         Kludge until unstructered tiles -- mimics mdsio tile numbering
          iG = bi+(myXGlobalLo-1)/sNx
          jG = bj+(myYGlobalLo-1)/sNy
          
          n = ILNBLNK(RefName)
          IF (n .GT. (100-5-11))  n = 100-5-11
          fnm(1:5) = 'grid_'
          fnm(6:(6+n-1)) = RefName(1:n)
          n = n + 6
          write(fnm(n:(n+11-1)),'(a1,i3.3,a1,i3.3,a3)') 
     &         '-',iG,'-',jG,'.nc'
          CALL MNC_FILE_CREATE(myThid, fnm)

C         Create the dimensions
          IF (withHalo(1:1) .EQ. 'N') THEN
            CALL MNC_DIM_INIT(myThid,fnm,dnms(11), sNx  )
            CALL MNC_DIM_INIT(myThid,fnm,dnms(12), sNy  )
            CALL MNC_DIM_INIT(myThid,fnm,dnms(13), Nr   )
            CALL MNC_DIM_INIT(myThid,fnm,dnms(14), Nr+1 )
          ELSEIF (withHalo(1:1) .EQ. 'Y') THEN
            CALL MNC_DIM_INIT(myThid,fnm,dnms(11), sNx+2*OLx )
            CALL MNC_DIM_INIT(myThid,fnm,dnms(12), sNy+2*OLy )
            CALL MNC_DIM_INIT(myThid,fnm,dnms(13), Nr        )
            CALL MNC_DIM_INIT(myThid,fnm,dnms(14), Nr+1)
          ELSE
            write(msgbuf,'(a)') 'MNC ERROR: withHalo must be [Y|N]'
            CALL print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MNC_W_GRIDINFO_ALL'
          ENDIF

C         Attributes
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fnm, 'mitgcm_tag_index', 
     &         MNC_TAG_ID )
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fnm, 'mitgcm_grid_ver', 
     &         1, 0.1D0 )
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fnm, 'reference',
     &         'Put reference string here.' )
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fnm, 'author',
     &         'Put author string here.' )
          CALL MNC_FILE_ADD_ATTR_STR(myThid,fnm, 'date',
     &         'Put date string here.' )

C         Model parameters (SIZE.h) included as attributes
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'sNx', 1, sNx)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'sNy', 1, sNy)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'OLx', 1, OLx)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'OLy', 1, OLy)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'nSx', 1, nSx)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'nSy', 1, nSy)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'nPx', 1, nPx)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'nPy', 1, nPy)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'Nx', 1, Nx)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'Ny', 1, Ny)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'Nr', 1, Nr)

C         Grid info included as attributes
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'bi', 1, bi)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'bj', 1, bj)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'iG', 1, iG)
          CALL MNC_FILE_ADD_ATTR_INT(myThid,fnm, 'jG', 1, jG)
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fnm, 'xC0', 1, xC0)
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fnm, 'yC0', 1, yC0)
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fnm, 'gravitySign', 
     &         1, gravitySign )
          CALL MNC_FILE_ADD_ATTR_DBL(myThid,fnm, 'rkFac', 1, rkFac)

C         Create the grids
C         center
          dnms(1)(1:MNC_MAX_CHAR) = dnms(11)(1:MNC_MAX_CHAR)
          dnms(2)(1:MNC_MAX_CHAR) = dnms(12)(1:MNC_MAX_CHAR)
          dnms(3)(1:MNC_MAX_CHAR) = dnms(13)(1:MNC_MAX_CHAR)
          CALL MNC_GRID_INIT(myThid, fnm, gnms(1), 2, dnms)
          CALL MNC_GRID_INIT(myThid, fnm, gnms(2), 3, dnms)
C         r, rp1
          CALL MNC_GRID_INIT(myThid, fnm, gnms(3),1,dnms(13))
          CALL MNC_GRID_INIT(myThid, fnm, gnms(4),1,dnms(14))

C         Indicies and sizes for extracting the data -- we need to skip
C         over the exchange regions
          rnX = sNx + 2*OLx
          rnY = sNy + 2*OLy
          rnZ = Nr
          IF (withHalo(1:1) .EQ. 'N') THEN
            istart(1) = OLx + 1
            istart(2) = OLy + 1
            istart(3) = 1
            iend(1) = sNx + OLx
            iend(2) = sNy + OLy
            iend(3) = Nr
          ELSE
            istart(1) = 1
            istart(2) = 1
            istart(3) = 1
            iend(1) = sNx + 2*OLx
            iend(2) = sNy + 2*OLy
            iend(3) = Nr
          ENDIF

C         Create and write grid parameters
          IF ( RorD(1:1) .EQ. 'D' ) THEN

C           rC, rF
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(3),'rC','--')
            CALL MNC_RESHAPE_RS_D(rnZ,1,1, 1,Nr, rC, t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'rC', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(4),'rF','--')
            CALL MNC_RESHAPE_RS_D(rnZ+1,1,1, 1,Nr+1, rF, t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'rF', t_D)
C           drC, drF
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(3),'drC','--')
            CALL MNC_RESHAPE_RS_D(rnZ,1,1, 1,Nr, drC, t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'drC', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(3),'drF','--')
            CALL MNC_RESHAPE_RS_D(rnZ,1,1, 1,Nr, drF, t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'drF', t_D)
C           saFac
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(3),'saFac','--')
            CALL MNC_RESHAPE_RS_D(rnZ,1,1, 1,Nr, saFac, t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'saFac', t_D)
            
C           xC, yC
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'xC','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           xC(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'xC', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'yC','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           yC(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'yC', t_D)
C           xG, yG
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'xG','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           xG(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'xG', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'yG','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           yG(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'yG', t_D)
C           dxC, dyC
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dxC','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dxC(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dxC', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dyC','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dyC(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dyC', t_D)
C           dxF, dyF
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dxF','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dxF(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dxF', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dyF','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dyF(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dyF', t_D)
C           dxG, dyG
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dxG','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dxG(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dxG', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dyG','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dyG(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dyG', t_D)
C           dxV, dyU
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dxV','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dxV(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dxV', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'dyU','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           dyU(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'dyU', t_D)
C           rA, rAw, rAs, rAz
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'rA','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           rA(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'rA', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'rAw','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           rAw(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'rAw', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'rAs','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           rAs(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'rAs', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'rAz','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           rAz(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'rAz', t_D)
C           fCori, fCoriG
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'fCori','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           fCori(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'fCori', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'fCoriG','m')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           fCoriG(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'fCoriG', t_D)
C           R_low, Ro_surf
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'R_low','--')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           R_low(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'R_low', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(1),'Ro_surf','--')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, 1, istart, iend, 
     &           Ro_surf(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'Ro_surf', t_D)
C           HFacC, HFacW, HFacS
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(2),'HFacC','--')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, rnZ, istart, iend, 
     &           HFacC(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'HFacC', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(2),'HFacW','--')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, rnZ, istart, iend, 
     &           HFacW(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'HFacW', t_D)
            CALL MNC_VAR_INIT_DBL(myThid,fnm,gnms(2),'HFacS','--')
            CALL MNC_RESHAPE_RS_D(rnX, rnY, rnZ, istart, iend, 
     &           HFacS(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fnm, 'HFacS', t_D)

          ELSEIF ( RorD(1:1) .EQ. 'R' ) THEN

C           rC, rF
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(3),'rC','--')
            CALL MNC_RESHAPE_RS_R(rnZ,1,1, 1,Nr, rC, t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'rC', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(4),'rF','--')
            CALL MNC_RESHAPE_RS_R(rnZ+1,1,1, 1,Nr+1, rF, t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'rF', t_R)
C           drC, drF
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(3),'drC','--')
            CALL MNC_RESHAPE_RS_R(rnZ,1,1, 1,Nr, drC, t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'drC', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(3),'drF','--')
            CALL MNC_RESHAPE_RS_R(rnZ,1,1, 1,Nr, drF, t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'drF', t_R)
C           saFac
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(3),'saFac','--')
            CALL MNC_RESHAPE_RS_R(rnZ,1,1, 1,Nr, saFac, t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'saFac', t_R)
            
C           xC, yC
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'xC','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           xC(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'xC', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'yC','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           yC(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'yC', t_R)
C           xG, yG
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'xG','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           xG(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'xG', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'yG','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           yG(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'yG', t_R)
C           dxC, dyC
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dxC','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dxC(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dxC', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dyC','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dyC(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dyC', t_R)
C           dxF, dyF
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dxF','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dxF(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dxF', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dyF','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dyF(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dyF', t_R)
C           dxG, dyG
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dxG','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dxG(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dxG', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dyG','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dyG(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dyG', t_R)
C           dxV, dyU
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dxV','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dxV(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dxV', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'dyU','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           dyU(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'dyU', t_R)
C           rA, rAw, rAs, rAz
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'rA','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           rA(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'rA', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'rAw','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           rAw(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'rAw', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'rAs','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           rAs(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'rAs', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'rAz','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           rAz(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'rAz', t_R)
C           fCori, fCoriG
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'fCori','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           fCori(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'fCori', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'fCoriG','m')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           fCoriG(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'fCoriG', t_R)
C           R_low, Ro_surf
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'R_low','--')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           R_low(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'R_low', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(1),'Ro_surf','--')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, 1, istart, iend, 
     &           Ro_surf(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'Ro_surf', t_R)
C           HFacC, HFacW, HFacS
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(2),'HFacC','--')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, rnZ, istart, iend, 
     &           HFacC(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'HFacC', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(2),'HFacW','--')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, rnZ, istart, iend, 
     &           HFacW(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'HFacW', t_R)
            CALL MNC_VAR_INIT_REAL(myThid,fnm,gnms(2),'HFacS','--')
            CALL MNC_RESHAPE_RS_R(rnX, rnY, rnZ, istart, iend, 
     &           HFacS(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fnm, 'HFacS', t_R)

          ELSE
            write(msgbuf,'(a)') 'MNC ERROR: RodD must be [R|D]'
            CALL print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MNC_W_GRIDINFO_ALL'
          ENDIF

C         Add long_name attributes to the variables
          mt = myThid
          atnm(1:12) = 'description '
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'rC',atnm,
     &         'r of cell center')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'rF',atnm,
     &         'r of cell face')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'drC',atnm,
     &         'r cell center separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'drF',atnm,
     &         'r cell face separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'saFac',atnm,
     &         'shallow atmosphere factor')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'xC',atnm,
     &         'x of cell center')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'yC',atnm,
     &         'y of cell center')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'xG',atnm,
     &         'x of cell corner')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'yG',atnm,
     &         'y of cell corner')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dxC',atnm,
     &         'x cell center separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dyC',atnm,
     &         'y cell center separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dxF',atnm,
     &         'x cell face separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dyF',atnm,
     &         'y cell face separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dxG',atnm,
     &         'x cell face separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dyG',atnm,
     &         'y cell face separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dxV',atnm,
     &         'x v-velocity separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'dyU',atnm,
     &         'y u-velocity separation')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'rA',atnm,
     &         'r-face area')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'rAw',atnm,
     &         '')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'rAs',atnm,
     &         '')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'rAz',atnm,
     &         '')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'fCori',atnm,
     &         'Coriolis f')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'fCoriG',atnm,
     &         'Coriolis f')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'R_low',atnm,
     &         'base of fluid in r-units')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'Ro_surf',atnm,
     &         'surface reference (at rest) position')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'HFacC',atnm,
     &         'C-facing vertical fraction of open cell')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'HFacW',atnm,
     &         'W-facing vertical fraction of open cell')
          CALL MNC_VAR_ADD_ATTR_STR(mt,fnm,'HFacS',atnm,
     &         'S-facing vertical fraction of open cell')

C          CALL MNC_DUMP()

C         Close the grid file
          CALL MNC_FILE_CLOSE(myThid, fnm)

        ENDDO
      ENDDO

      _END_MASTER( myThid )

      RETURN
      END


