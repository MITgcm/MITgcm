C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_grid.F,v 1.3 2004/01/07 07:29:13 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_GRID_INIT( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     ndim, 
     I     dnames )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      character*(*) gname
      integer ndim
      character*(*) dnames(ndim)

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k, n, indf,indg,indd, id, fid, ngrid, ng_ind, ds_last
      integer idd, err, dn,dnpn
      character*(MAX_LEN_MBUF) msgbuf
      character*(MNC_MAX_CHAR) name
      character*(3*MNC_MAX_CHAR) dim_names
      character*(MNC_MAX_CHAR) file_name
      character*(MNC_MAX_CHAR) grid_name

C     Get the file ID and indicies
      file_name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      n = ILNBLNK(fname)
      IF (n .GT. MNC_MAX_CHAR) n = MNC_MAX_CHAR
      file_name(1:n) = fname(1:n)
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, file_name, mnc_f_names, indf)
      IF (indf .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', file_name, 
     &       '''does not exist--must be previously initialized'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_GRID_INIT'
      ENDIF
      fid = mnc_f_info(indf,2)
      ngrid = mnc_f_info(indf,3)
      ng_ind = 4 + 3*ngrid
      IF (ngrid .EQ. 0) THEN
        ds_last = 0
      ELSE
        ds_last = mnc_f_info(indf,(ng_ind+2))
      ENDIF

C     Check for sufficient space in memory
      i = ds_last + ndim
      j = 3 + 3*(ngrid + 1)
      IF ((i .GE. MNC_MAX_INFO) .OR. (j .GE. MNC_MAX_INFO)) THEN
        write(msgbuf,'(2a)') 'MNC ERROR: insufficient space--',
     &       'please increase MNC_MAX_INFO'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_GRID_INIT'
      ENDIF

C     Get the grid index [check for re-definition]
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, gname, mnc_g_names, indg)
      IF ( indg .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: grid ''',
     &       gname, '''was previously defined'
      ELSE
        CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_g_names,
     &       indg)
        mnc_g_names(indg)(1:MNC_MAX_CHAR) =
     &       mnc_blank_name(1:MNC_MAX_CHAR)
        n = ILNBLNK(gname)
        mnc_g_names(indg)(1:n) = gname(1:n)
        grid_name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        grid_name(1:9) = 'mncgrid__'
        k = 10 + n - 1
        grid_name(10:k) = gname(1:n)
      ENDIF

C     Add the dimensions
      dn = 1
      DO i = 1,3*MNC_MAX_CHAR
        dim_names(i:i) = ' '
      ENDDO
      DO i = 1,ndim

        j = ds_last + i
        name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        n = ILNBLNK(dnames(i))
        name(1:n) = dnames(i)(1:n)
        CALL MNC_GET_IND(myThid, MNC_MAX_ID, name, mnc_d_names, indd)
        IF (indd .LT. 1) THEN
          write(msgbuf,'(3a)') 'MNC ERROR: dimension ''',
     &         name, ''' does not exist'
          CALL print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MNC_GRID_INIT'
        ENDIF
        name(1:MNC_MAX_CHAR) = mnc_d_names(indd)(1:MNC_MAX_CHAR)
        k = mnc_d_size(indd)
        IF ( k .LT. 1 ) THEN
          IF (i .NE. ndim) THEN
            write(msgbuf,'(3a)') 'MNC ERROR: in file ''', fname,
     &           ''' grid ''', gname, 
     &           ''': only the LAST dim can be unlimited (len<0)'
            CALL print_error( msgbuf, mythid )
            stop 'ABNORMAL END: S/R MNC_GRID_INIT'
          ENDIF
          k = NF_UNLIMITED
        ENDIF
        err = NF_DEF_DIM(fid, name, k, idd)
        write(msgbuf,'(5a)') 'creating dim in grid ''', gname,
     &       ''' in file ''', fname, ''''
        CALL MNC_HANDLE_ERR(myThid, err, msgbuf)
        mnc_fg_ids(indf,j) = idd
        mnc_fd_ind(indf,j) = indd

C       Accumulate the dimension names
        n = ILNBLNK(name)
        dnpn = dn + n - 1
        dim_names(dn:dnpn) = name(1:n)
        dn = dn + 2
        
      ENDDO

C     Add attributes to aid in later parsing
      CALL MNC_FILE_ADD_ATTR_STR(myThid, file_name, grid_name, 
     &     dim_names)
      write(msgbuf,'(5a)') 'creating attribute for grid ''', gname,
     &     ''' in file ''', fname, ''''
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

C     Grid successfully added, so update file table
      mnc_f_info(indf,ng_ind) = indg
      mnc_f_info(indf,ng_ind+1) = ds_last + 1
      mnc_f_info(indf,ng_ind+2) = ds_last + ndim
      mnc_f_info(indf,3) = ngrid + 1
      
      RETURN
      END
      
C==================================================================

      SUBROUTINE MNC_GRID_SET_LL( 
     I     myThid, 
     I     fname, 
     I     gname, 
     I     type, 
     I     lats, 
     I     lons )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, type
C     type ==> [ 1=1D , 2=2D ]
      character*(*) fname
      character*(*) gname
      _RL lats(*)
      _RL lons(*)

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, indf,indg, fid
      integer err
      character*(MAX_LEN_MBUF) msgbuf

C     Get the file ID
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF ( indf .LT. 1 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, '''does not exist'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_GRID_SET_LL'
      ENDIF

C     Check that the grid exists
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, gname, mnc_g_names, indg)
      IF ( indg .LT. 1 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: grid ''',
     &       gname, '''does not exist'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_GRID_SET_LL'
      ENDIF

      RETURN
      END

