C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_grid.F,v 1.11 2004/03/19 03:28:37 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_GRID_INIT( 
     I     fname, 
     I     gname, 
     I     ndim, 
     I     dnames, 
     I     myThid )

      implicit none

C     Arguments
      integer myThid, ndim
      character*(*) fname,gname
      character*(*) dnames(ndim)

C     Local Variables
      integer ind

      CALL MNC_GRID_INIT_ALL(fname, gname, ndim, dnames, ind, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_GRID_INIT_ALL( 
     I     fname, 
     I     gname, 
     I     ndim, 
     I     dnames, 
     O     ind, 
     I     myThid ) 

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, ndim, ind
      character*(*) fname,gname
      character*(*) dnames(ndim)

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k,ii,jj,kk, n,nf, indf,indg,indd, fid, ngrid
      integer ng_ind,lg_ind, ds_last, ndim_file, igr,ig1,ig2
      character*(MAX_LEN_MBUF) msgbuf
      character*(MNC_MAX_CHAR) name
      character*(MNC_MAX_CHAR) file_name

C     Get the file ID and indicies
      file_name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      nf = ILNBLNK(fname)
      IF (nf .GT. MNC_MAX_CHAR) nf = MNC_MAX_CHAR
      file_name(1:nf) = fname(1:nf)
      CALL MNC_GET_IND(MNC_MAX_ID,file_name,mnc_f_names,indf, myThid)
      IF (indf .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''', file_name(1:nf), 
     &       ''' does not exist'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_GRID_INIT'
      ENDIF
      fid = mnc_f_info(indf,2)
      ngrid = mnc_f_info(indf,3)
      ng_ind = 4 + 3*ngrid
      IF (ngrid .EQ. 0) THEN
        ds_last = 0
      ELSE
        lg_ind = 4 + 3*(ngrid - 1)
        ds_last = mnc_f_info(indf,(lg_ind+2))
      ENDIF

C     Check for sufficient space in memory
      i = ds_last + ndim
      j = 3 + 3*(ngrid + 1)
      IF ((i .GE. MNC_MAX_INFO) .OR. (j .GE. MNC_MAX_INFO)) THEN
        write(msgbuf,'(2a)') 'MNC_GRID_INIT_ALL ERROR: insufficient',
     &       ' space--please increase MNC_MAX_INFO'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_GRID_INIT_ALL'
      ENDIF

C     Enter DEFINE mode
      CALL MNC_FILE_REDEF(fname, myThid)

C     Check for grid re-definition
      DO igr = 1,mnc_f_info(indf,3)
        ii = 4 + 3*igr
        IF (mnc_g_names(mnc_f_info(indf,ii)) .EQ. gname) THEN
          ig1 = mnc_f_info(indf,ii+1)
          ig2 = mnc_f_info(indf,ii+2)

C         Check if different number of dims
          IF (ndim .NE. (ig2-ig1+1)) THEN
            write(msgbuf,'(6a)') 'MNC ERROR: grid ''', gname,
     &           ''' was previously defined for file ''', 
     &           mnc_f_names(indf), ''' with a different ',
     &           'number of dimensions'
            CALL print_error(msgbuf, mythid)
            stop 'ABNORMAL END: S/R MNC_GRID_INIT'
          ENDIF

C         Check if same number of dims but different dim names
          k = 0
          DO jj = ig1,ig2
            k = k + 1
            IF (mnc_d_names(mnc_fd_ind(indf,jj)) .NE. dnames(k)) THEN
              write(msgbuf,'(6a)') 'MNC ERROR: grid ''', gname,
     &             ''' was previously defined for file ''', 
     &             mnc_f_names(indf), ''' with a different ',
     &             'combination of dimensions'
              CALL print_error(msgbuf, mythid)
              stop 'ABNORMAL END: S/R MNC_GRID_INIT'
            ENDIF
          ENDDO
          GOTO 10

        ENDIF
      ENDDO
 10   CONTINUE

C     Reaching this point means the grid was not previously defined
      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID, mnc_g_names,
     &     indg, myThid)
      mnc_g_names(indg)(1:MNC_MAX_CHAR) =
     &     mnc_blank_name(1:MNC_MAX_CHAR)
      n = ILNBLNK(gname)
      mnc_g_names(indg)(1:n) = gname(1:n)
      
C     Add the dimensions
      DO i = 1,ndim

        j = ds_last + i
        name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        n = ILNBLNK(dnames(i))

C       Search for the dimension ID within the list of dimensions
C       defined for this file
        ndim_file = mnc_f_alld(indf,1)
        indd = 0
        DO ii = 1,ndim_file
          jj = mnc_f_alld(indf,ii+1)
          kk = ILNBLNK(mnc_d_names(jj))
          IF ((n .EQ. kk) 
     &         .AND. (dnames(i)(1:n) .EQ. mnc_d_names(jj)(1:kk))) THEN
            indd = jj
            GOTO 20
          ENDIF
        ENDDO
 20     CONTINUE
        IF (indd .LT. 1) THEN
          write(msgbuf,'(5a)') 'MNC ERROR: dimension ''',
     &         dnames(i)(1:n), ''' does not exist for file ''', 
     &         fname(1:nf), ''''
          CALL print_error( msgbuf, mythid )
          stop 'ABNORMAL END: S/R MNC_GRID_INIT'
        ENDIF

        mnc_fd_ind(indf,j) = indd
        
      ENDDO

C     Grid successfully added, so update file table
      mnc_f_info(indf,ng_ind) = indg
      mnc_f_info(indf,ng_ind+1) = ds_last + 1
      mnc_f_info(indf,ng_ind+2) = ds_last + ndim
      mnc_f_info(indf,3) = ngrid + 1
      ind = indg
      
      RETURN
      END
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_GRID_GET_DIMIND( 
     I     indf, 
     I     dname, 
     O     ind_fg_ids, 
     I     myThid )

      implicit none
#include "mnc_common.h"

C     Arguments
      integer indf, ind_fg_ids, myThid
      character*(*) dname

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,j,k,l, n,n1, ngrid, ds,de

      ind_fg_ids = -1
      n = ILNBLNK(dname)
      ngrid = mnc_f_info(indf,3)
      DO i = 1,ngrid
        j = 4 + 3*(i - 1)
        ds = mnc_f_info(indf,j+1)
        de = mnc_f_info(indf,j+2)
        DO k = ds,de
          l = mnc_fd_ind(indf,k)
          n1 = ILNBLNK(mnc_d_names(l))
          IF ((n .EQ. n1) 
     &         .AND. (mnc_d_names(l)(1:n1) .EQ. dname(1:n))) THEN
            ind_fg_ids = k
            RETURN
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
