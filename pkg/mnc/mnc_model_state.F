C $Header: /u/gcmpack/MITgcm/pkg/mnc/Attic/mnc_model_state.F,v 1.3 2004/01/27 05:47:32 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_W_STATE( myThid, myIter, RefName )

      implicit none
      integer myThid, myIter
      character*(*) RefName

      CALL MNC_W_STATE_ALL( myThid, myIter, RefName, 'N', 'D' )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_W_STATE_ALL( 
     I     myThid, 
     I     myIter, 
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
#include "DYNVARS.h"

C     Arguments
      integer myThid, myIter
      character*(*) RefName
      character*(1) withHalo, RorD

C     Functions
      integer ILNBLNK, IFNBLNK

C     Local Variables
      integer MAX_GRIDS_LOC
      parameter ( MAX_GRIDS_LOC = 20 )
      integer i, n, bi,bj, iG,jG, rnX,rnY,rnZ, bf,bl
      integer istart(3,MAX_GRIDS_LOC), iend(3,MAX_GRIDS_LOC)
      character*(MNC_MAX_CHAR) fname, gnms(20),dnms(20)
      REAL*4 t_R((sNx+2*OLx)*(sNy+2*OLy)*(Nr+1))
      REAL*8 t_D((sNx+2*OLx)*(sNy+2*OLy)*(Nr+1))

C     Initialize strings
      fname(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      DO i = 1,20
        gnms(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        dnms(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      ENDDO

C     Only do I/O if I am the master thread
      _BEGIN_MASTER( myThid )

C     Output a grid file for all tiles owned by this thread or process
      DO bj = 1,nSy
        DO bi = 1,nSx

C         Kludge until unstructered tiles -- mimics mdsio tile numbering
          iG = bi+(myXGlobalLo-1)/sNx
          jG = bj+(myYGlobalLo-1)/sNy
          
          n = ILNBLNK(RefName)
          write(fname(1:100),'(3a,i3.3,a,i3.3,a)') "state_",
     &         RefName(1:n), '-',iG,'-',jG,'.nc '

          bf = IFNBLNK(fname)
          bl = ILNBLNK(fname)

C         Create the file (do nothing if it already exists)
          CALL MNC_W_STATE_INIT(myThid,myIter,fname(bf:bl), 
     &         bi,bj, iG,jG, withHalo, RorD)

C         Indicies and sizes for extracting the data -- we need to skip
C         over the exchange regions
          rnX = sNx + 2*OLx
          rnY = sNy + 2*OLy
          rnZ = Nr
          IF (withHalo(1:1) .EQ. 'N') THEN
C           center = 1
            istart(1,1) = OLx + 1
            istart(2,1) = OLy + 1
            istart(3,1) = 1
            iend(1,1)   = sNx + OLx
            iend(2,1)   = sNy + OLy
            iend(3,1)   = Nr
C           u = 2
            istart(1,2) = OLx + 1
            istart(2,2) = OLy + 1
            istart(3,2) = 1
            iend(1,2)   = sNx + OLx + 1
            iend(2,2)   = sNy + OLy
            iend(3,2)   = Nr
C           v = 3
            istart(1,3) = OLx + 1
            istart(2,3) = OLy + 1
            istart(3,3) = 1
            iend(1,3)   = sNx + OLx
            iend(2,3)   = sNy + OLy + 1
            iend(3,3)   = Nr
          ELSE
C           center = 1
            istart(1,1) = 1
            istart(2,1) = 1
            istart(3,1) = 1
            iend(1,1)   = sNx + 2*OLx
            iend(2,1)   = sNy + 2*OLy
            iend(3,1)   = Nr
C           u = 2
            istart(1,2) = 1
            istart(2,2) = 1
            istart(3,2) = 1
            iend(1,2)   = sNx + 2*OLx
            iend(2,2)   = sNy + 2*OLy
            iend(3,2)   = Nr
C           v = 3
            istart(1,3) = 1
            istart(2,3) = 1
            istart(3,3) = 1
            iend(1,3)   = sNx + 2*OLx
            iend(2,3)   = sNy + 2*OLy
            iend(3,3)   = Nr
          ENDIF
        
C         Increment (by 1) the current length of the NetCDF UNLIMITED
C         dimension by increasing the length of the iteration variable
          CALL MNC_VAR_APPEND_INT(myThid,fname, 'iter', myIter, 1)

C         With the NetCDF UNLIMITED dimension now incremented, write the
C         variables at this iteration
          IF ( RorD(1:1) .EQ. 'D' ) THEN

C           Salinity
            CALL MNC_RESHAPE_RL_D(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           salt(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'salinity', t_D)
C           Temperature
            CALL MNC_RESHAPE_RL_D(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           theta(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'temp', t_D)
C           uVel
            CALL MNC_RESHAPE_RL_D(rnX,rnY,rnZ, istart(1,2),iend(1,2), 
     &           uVel(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'u', t_D)
C           vVel
            CALL MNC_RESHAPE_RL_D(rnX,rnY,rnZ, istart(1,3),iend(1,3), 
     &           vVel(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'v', t_D)
C           wVel
            CALL MNC_RESHAPE_RL_D(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           wVel(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'w', t_D)
C           etaN
            CALL MNC_RESHAPE_RL_D(rnX,rnY,1, istart(1,1),iend(1,1), 
     &           etaN(1-OLx,1-OLy,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'etaN', t_D)
C           phiHydLow
            IF (buoyancyRelation.NE.'ATMOSPHERIC') THEN
              CALL MNC_RESHAPE_RL_D(rnX,rnY,1, istart(1,1),iend(1,1), 
     &             phiHydLow(1-OLx,1-OLy,bi,bj), t_D)
              CALL MNC_VAR_WRITE_DBL(myThid, fname, 'phl', t_D)
            ENDIF
C           totPhiHyd
            CALL MNC_RESHAPE_RL_D(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           totPhiHyd(1-OLx,1-OLy,1,bi,bj), t_D)
            CALL MNC_VAR_WRITE_DBL(myThid, fname, 'ph', t_D)

        ELSEIF ( RorD(1:1) .EQ. 'R' ) THEN

C           Salinity
            CALL MNC_RESHAPE_RL_R(rnX,rnY,rnZ, istart(1,1), iend(1,1), 
     &           salt(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'salinity', t_R)
C           Temperature
            CALL MNC_RESHAPE_RL_R(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           theta(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'temp', t_R)
C           uVel
            CALL MNC_RESHAPE_RL_R(rnX,rnY,rnZ, istart(1,2),iend(1,2), 
     &           uVel(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'u', t_R)
C           vVel
            CALL MNC_RESHAPE_RL_R(rnX,rnY,rnZ, istart(1,3),iend(1,3), 
     &           vVel(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'v', t_R)
C           wVel
            CALL MNC_RESHAPE_RL_R(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           wVel(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'w', t_R)
C           etaN
            CALL MNC_RESHAPE_RL_R(rnX,rnY,1, istart(1,1),iend(1,1), 
     &           etaN(1-OLx,1-OLy,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'etaN', t_R)
C           phiHydLow
            IF (buoyancyRelation.NE.'ATMOSPHERIC') THEN
              CALL MNC_RESHAPE_RL_R(rnX,rnY,1, istart(1,1),iend(1,1), 
     &             phiHydLow(1-OLx,1-OLy,bi,bj), t_R)
              CALL MNC_VAR_WRITE_REAL(myThid, fname, 'phl', t_R)
            ENDIF
C           totPhiHyd
            CALL MNC_RESHAPE_RL_R(rnX,rnY,rnZ, istart(1,1),iend(1,1), 
     &           totPhiHyd(1-OLx,1-OLy,1,bi,bj), t_R)
            CALL MNC_VAR_WRITE_REAL(myThid, fname, 'ph', t_R)

          ENDIF

        ENDDO
      ENDDO

      _END_MASTER( myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_W_STATE_INIT( 
     I     myThid, 
     I     myIter, 
     I     fname, 
     I     bi,bj, iG,jG, 
     I     withHalo, 
     I     RorD )

      implicit none
#include "mnc_common.h"

#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     Arguments
      integer myThid, myIter, bi,bj, iG,jG
      character*(*) fname
      character*(1) withHalo, RorD

C     Functions
      integer ILNBLNK

C     Local Variables
      integer MAX_GRIDS_LOC
      parameter ( MAX_GRIDS_LOC = 20 )
      integer i, indf, n, rnX,rnY,rnZ
      character*(MNC_MAX_CHAR) atnm
      character*(MNC_MAX_CHAR) gnms(MAX_GRIDS_LOC)
      character*(MNC_MAX_CHAR) dnms(MAX_GRIDS_LOC)
      character*(MAX_LEN_MBUF) msgbuf

C     Initialize strings
      atnm(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      DO i = 1,20
        gnms(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        dnms(i)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      ENDDO
      
C     If necessary, create the new file and if not exit
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF (indf .LT. 1) THEN
        
        n = ILNBLNK(fname)
        CALL MNC_FILE_CREATE(myThid, fname(1:n))
        
C       dim names        123456789
        dnms(11)(1:9) = 'X        '
        dnms(12)(1:9) = 'Y        '
        dnms(13)(1:9) = 'R        '
        dnms(14)(1:9) = 'Xp1      '
        dnms(15)(1:9) = 'Yp1      '
        dnms(16)(1:9) = 'Rp1      '
        dnms(20)(1:9) = 'T        '

C       Create the dimensions
        CALL MNC_DIM_INIT(myThid,fname,dnms(13), Nr   )
        CALL MNC_DIM_INIT(myThid,fname,dnms(16), Nr+1 )
        CALL MNC_DIM_INIT(myThid,fname,dnms(20), -1   )
        IF (withHalo(1:1) .EQ. 'N') THEN
          CALL MNC_DIM_INIT(myThid,fname,dnms(11), sNx   )
          CALL MNC_DIM_INIT(myThid,fname,dnms(12), sNy   )
          CALL MNC_DIM_INIT(myThid,fname,dnms(14), sNx+1 )
          CALL MNC_DIM_INIT(myThid,fname,dnms(15), sNy+1 )
        ELSEIF (withHalo(1:1) .EQ. 'Y') THEN
          CALL MNC_DIM_INIT(myThid,fname,dnms(11), sNx+2*OLx )
          CALL MNC_DIM_INIT(myThid,fname,dnms(12), sNy+2*OLy )
          CALL MNC_DIM_INIT(myThid,fname,dnms(14), sNx+2*OLx )
          CALL MNC_DIM_INIT(myThid,fname,dnms(15), sNy+2*OLy )
        ELSE
          write(msgbuf,'(a)') 'MNC ERROR: withHalo must be [Y|N]'
          CALL print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MNC_W_GRIDINFO_ALL'
        ENDIF

C       grid names       123456789
        gnms( 1)(1:9) = 'center   '
        gnms( 2)(1:9) = 'u        '
        gnms( 3)(1:9) = 'v        '
        gnms( 4)(1:9) = 'w        '
        gnms( 5)(1:9) = 'zeta     '
        gnms( 6)(1:9) = 'time     '
        gnms( 7)(1:9) = 'center2d '

C       Indicies and sizes for extracting the data -- we need to skip
C       over the exchange regions
        rnX = sNx + 2*OLx
        rnY = sNy + 2*OLy
        rnZ = Nr
        
C       Create the grids
C       time
        CALL MNC_GRID_INIT(myThid, fname, gnms(6), 1, dnms(20))
C       center
        dnms(1)(1:MNC_MAX_CHAR) = dnms(11)(1:MNC_MAX_CHAR)
        dnms(2)(1:MNC_MAX_CHAR) = dnms(12)(1:MNC_MAX_CHAR)
        dnms(3)(1:MNC_MAX_CHAR) = dnms(13)(1:MNC_MAX_CHAR)
        dnms(4)(1:MNC_MAX_CHAR) = dnms(20)(1:MNC_MAX_CHAR)
        CALL MNC_GRID_INIT(myThid, fname, gnms(1), 4, dnms)
C       u
        dnms(1)(1:MNC_MAX_CHAR) = dnms(14)(1:MNC_MAX_CHAR)
        CALL MNC_GRID_INIT(myThid, fname, gnms(2), 4, dnms)
C       v
        dnms(1)(1:MNC_MAX_CHAR) = dnms(11)(1:MNC_MAX_CHAR)
        dnms(2)(1:MNC_MAX_CHAR) = dnms(15)(1:MNC_MAX_CHAR)
        CALL MNC_GRID_INIT(myThid, fname, gnms(3), 4, dnms)
C       w
C        dnms(2)(1:MNC_MAX_CHAR) = dnms(12)(1:MNC_MAX_CHAR)
C        dnms(3)(1:MNC_MAX_CHAR) = dnms(16)(1:MNC_MAX_CHAR)
C        CALL MNC_GRID_INIT(myThid, fname, gnms(4), 4, dnms)
C       zeta (vorticity)
        dnms(1)(1:MNC_MAX_CHAR) = dnms(14)(1:MNC_MAX_CHAR)
        dnms(2)(1:MNC_MAX_CHAR) = dnms(15)(1:MNC_MAX_CHAR)
        dnms(3)(1:MNC_MAX_CHAR) = dnms(13)(1:MNC_MAX_CHAR)
        CALL MNC_GRID_INIT(myThid, fname, gnms(5), 4, dnms)
C       center2d
        dnms(1)(1:MNC_MAX_CHAR) = dnms(11)(1:MNC_MAX_CHAR)
        dnms(2)(1:MNC_MAX_CHAR) = dnms(12)(1:MNC_MAX_CHAR)
        dnms(3)(1:MNC_MAX_CHAR) = dnms(20)(1:MNC_MAX_CHAR)
        CALL MNC_GRID_INIT(myThid, fname, gnms(7), 3, dnms)

C       Attributes
        CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'mitgcm_tag_index', 
     &       MNC_TAG_ID )
        CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'mitgcm_grid_ver', 
     &       1, 0.1D0 )
        CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'reference',
     &       'Put reference string here.' )
        CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'author',
     &       'Put author string here.' )
        CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'date',
     &       'Put date string here.' )
        
C       Model parameters (SIZE.h) included as attributes
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'sNx', 1, sNx)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'sNy', 1, sNy)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'OLx', 1, OLx)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'OLy', 1, OLy)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nSx', 1, nSx)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nSy', 1, nSy)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nPx', 1, nPx)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nPy', 1, nPy)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'Nx', 1, Nx)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'Ny', 1, Ny)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'Nr', 1, Nr)
        
C       Grid info included as attributes
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'bi', 1, bi)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'bj', 1, bj)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'iG', 1, iG)
        CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'jG', 1, jG)
        CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'xC0', 1, xC0)
        CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'yC0', 1, yC0)
        CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'gravitySign', 
     &       1, gravitySign )
        CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'rkFac', 1, rkFac)
        
C       Create time dimension
        CALL MNC_VAR_INIT_INT(myThid,fname,gnms(6),'iter','--')
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'iter','description',
     &       'iteration number')

C       Create variables
        IF ( RorD(1:1) .EQ. 'D' ) THEN
          
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(1),'temp','deg C')
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(1),'salinity','ppt')
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(2),'u','m/s')
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(3),'v','m/s')
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(1),'w','m/s')
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(7),'etaN','m')
          IF (buoyancyRelation.NE.'ATMOSPHERIC')
     &         CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(7),'phl','m')
          CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(1),'ph','m')
#ifdef ALLOW_NONHYDROSTATIC
          IF (nonHydroStatic)
     &      CALL MNC_VAR_INIT_DBL(myThid,fname,gnms(1),'pnh','m')
#endif

        ELSEIF ( RorD(1:1) .EQ. 'R' ) THEN
          
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(1),'temp','deg C')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(1),'salinity','ppt')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(2),'u','m/s')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(3),'v','m/s')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(1),'w','m/s')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(7),'etaN','m')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(7),'phl','m')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(1),'ph','m')
          IF (buoyancyRelation.NE.'ATMOSPHERIC')
     &         CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(7),'phl','m')
          CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(1),'ph','m')
#ifdef ALLOW_NONHYDROSTATIC
          IF (nonHydroStatic)
     &      CALL MNC_VAR_INIT_REAL(myThid,fname,gnms(1),'pnh','m')
#endif

        ELSE
          write(msgbuf,'(a)') 'MNC ERROR: RodD must be [R|D]'
          CALL print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MNC_W_GRIDINFO_ALL'
        ENDIF
        
C       Add attributes to the variables
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'temp','long_name',
     &       'potential_temperature')
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'u','description',
     &       'grid-local zonal velocity')
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'v','description',
     &       'grid-local meridional velocity')
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'w','description',
     &       'grid-local vertical velocity')
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'etaN','description',
     &       'grid-local free-surface r-anomaly')
        IF (buoyancyRelation.NE.'ATMOSPHERIC')
     &       CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'phl',
     &       'description',
     &       'grid-local phi-hydrostatic at r-lower boundary')
        CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'ph','description',
     &       'grid-local total hydrostatic potential')
#ifdef ALLOW_NONHYDROSTATIC
          IF (nonHydroStatic)
     &           CALL MNC_VAR_ADD_ATTR_STR(myThid,fname,'pnh',
     &           'description',
     &           'grid-local non-hydrostatic potential')
#endif

      ENDIF
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
