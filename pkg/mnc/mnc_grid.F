C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_grid.F,v 1.20 2008/06/20 20:36:58 utke Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_GRID_INIT

C     !INTERFACE:
      SUBROUTINE MNC_GRID_INIT( 
     I     fname, 
     I     gname, 
     I     ndim, 
     I     dnames, 
     I     myThid )

C     !DESCRIPTION:
C     Create an MNC grid within a NetCDF file context.
      
C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid, ndim
      character*(*) fname,gname
      character*(*) dnames(ndim)
CEOP

C     !LOCAL VARIABLES:
      integer ind

      CALL MNC_GRID_INIT_ALL(fname, gname, ndim, dnames, ind, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_GRID_INIT_ALL

C     !INTERFACE:
      SUBROUTINE MNC_GRID_INIT_ALL( 
     I     fname, 
     I     gname, 
     I     ndim, 
     I     dnames, 
     O     ind, 
     I     myThid ) 

C     !DESCRIPTION:
C     Initialize a new conceptual (MNC inner layer) grid within a NetCDF
C     file context.  If the requested grid name already exists, then
C     verify that it has exactly the same number of dimensions, each
C     with exactly the same size and report a fatal error if not.  This
C     is a necessary check since the MNC inner layer does not support
C     grid name re--definition.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid, ndim, ind
      character*(*) fname,gname
      character*(*) dnames(ndim)
CEOP

C     !LOCAL VARIABLES:
      integer i,j,k,ii,jj,kk, n,nf, indf,indg,indd, fid, ngrid
      integer ng_ind,lg_ind, ds_last, ndim_file, igr,ig1,ig2
      integer ngt, ngn
      character*(MAX_LEN_MBUF) msgbuf
      character*(MNC_MAX_PATH) file_name

C     Functions
      integer ILNBLNK

C     Get the file ID and indicies
      DO i =1,MNC_MAX_PATH
        file_name(i:i) = ' '
      ENDDO
      nf = ILNBLNK(fname)
      IF (nf .GT. MNC_MAX_PATH) nf = MNC_MAX_PATH
      file_name(1:nf) = fname(1:nf)
      CALL MNC_GET_IND(MNC_MAX_FID,file_name,mnc_f_names,indf,myThid)
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

      ngn = ILNBLNK(gname)

C     Check for grid re-definition
      DO igr = 1,mnc_f_info(indf,3)
        ii = 4 + 3*(igr - 1)
        ngt = ILNBLNK(mnc_g_names(mnc_f_info(indf,ii)))
        IF ( (ngt .EQ. ngn) 
     &       .AND. (mnc_g_names(mnc_f_info(indf,ii))(1:ngt) 
     &       .EQ. gname(1:ngn)) ) THEN

          ig1 = mnc_f_info(indf,ii+1)
          ig2 = mnc_f_info(indf,ii+2)

C         Check if different number of dims
          IF (ndim .NE. (ig2-ig1+1)) THEN
            kk = ILNBLNK( mnc_f_names(indf) )
            write(msgbuf,'(6a)') 'MNC ERROR: grid ''', gname(1:ngn),
     &           ''' was previously defined for file ''', 
     &           mnc_f_names(indf)(1:kk), ''' with a different ',
     &           'number of dimensions'
            CALL print_error(msgbuf, mythid)
            stop 'ABNORMAL END: S/R MNC_GRID_INIT'
          ENDIF

C         Check if same number of dims but different dim names
          k = 0
          DO jj = ig1,ig2
            k = k + 1
            IF (mnc_d_names(mnc_fd_ind(indf,jj)) .NE. dnames(k)) THEN
              kk = ILNBLNK( mnc_f_names(indf) )
              write(msgbuf,'(6a)') 'MNC ERROR: grid ''', gname(1:ngn),
     &             ''' was previously defined for file ''', 
     &             mnc_f_names(indf)(1:kk), ''' with a different ',
     &             'combination of dimensions'
              CALL print_error(msgbuf, mythid)
              stop 'ABNORMAL END: S/R MNC_GRID_INIT'
            ENDIF
          ENDDO

C         Reaching this point means that the grid name WAS previously
C         defined and the number and sizes of the associated dimensions
C         exactly match so everything is OK and we do not need to create
C         a new definition for this grid.
          RETURN

        ENDIF
      ENDDO

C     Reaching this point means the grid was NOT previously defined and
C     we must therefore create a new definition.
      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID, mnc_g_names,
     &     'mnc_g_names', indg, myThid)
      mnc_g_names(indg)(1:MNC_MAX_CHAR) =
     &     mnc_blank_name(1:MNC_MAX_CHAR)
      n = ILNBLNK(gname)
      mnc_g_names(indg)(1:n) = gname(1:n)
      
C     Add the dimensions
      DO i = 1,ndim

        j = ds_last + i
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
CBOP 1
C     !ROUTINE: MNC_GRID_GET_DIMIND

C     !INTERFACE:
      SUBROUTINE MNC_GRID_GET_DIMIND( 
     I     indf, 
     I     dname, 
     O     ind_fg_ids, 
     I     myThid )

C     !DESCRIPTION:
C     Get the dimension ID (index) for the named dimension.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"

C     !INPUT PARAMETERS:
      integer indf, ind_fg_ids, myThid
      character*(*) dname
CEOP

C     !LOCAL VARIABLES:
      integer i,j,k,l, n,n1, ngrid, ds,de

C     Functions
      integer ILNBLNK


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
