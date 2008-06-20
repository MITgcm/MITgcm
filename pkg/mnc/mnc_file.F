C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_file.F,v 1.24 2008/06/20 20:36:58 utke Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_CREATE

C     !INTERFACE:
      SUBROUTINE MNC_FILE_CREATE( 
     I     fname, 
     I     myThid )

C     !DESCRIPTION:
C     Create a NetCDF file.
      
C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer indf

      CALL MNC_FILE_OPEN(fname, 0, indf, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_OPEN

C     !INTERFACE:
      SUBROUTINE MNC_FILE_OPEN( 
     I     fname, 
     I     itype, 
     O     indf, 
     I     myThid )

C     !DESCRIPTION:
C     Open or create a NetCDF file.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid,indf
      character*(*) fname
      integer itype
C     itype => [ 0=new | 1=append | 2=read-only ]
CEOP

C     !LOCAL VARIABLES:
      integer n, err, fid, nf
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

C     Is the file already open?
      nf = ILNBLNK(fname)
      CALL MNC_GET_IND(MNC_MAX_FID, fname,mnc_f_names,indf,myThid)
      IF (indf .GT. 0) THEN
        write(msgbuf,'(3a)') 'MNC_FILE_OPEN ERROR: ''', fname(1:nf), 
     &       ''' is already open -- cannot open twice'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: package MNC'
      ENDIF

      write(msgbuf,'(3a)') 'opening ''', fname(1:nf), ''''
      IF (itype .EQ. 0) THEN

C       Create new file
        err = NF_CREATE(fname, NF_CLOBBER, fid)
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

      ELSEIF (itype .EQ. 1) THEN

C       Append to existing file
        CALL MNC_FILE_READALL(fname, myThid)

      ELSE
C       Error
        write(msgbuf,'(a,i5,a)') 'MNC_FILE_OPEN ERROR: ''', itype, 
     &       ''' is not defined--should be: [0|1]'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_STR'
      ENDIF

      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_FID, mnc_f_names,
     &     'mnc_f_names', indf, myThid)
      n = ILNBLNK(fname)
      mnc_f_names(indf)(1:n) = fname(1:n)
      mnc_f_info(indf,1) = 1
      mnc_f_info(indf,2) = fid
      mnc_f_info(indf,3) = 0
      mnc_fv_ids(indf,1) = 0
      mnc_f_alld(indf,1) = 0

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_ADD_ATTR_STR

C     !INTERFACE:
      SUBROUTINE MNC_FILE_ADD_ATTR_STR( 
     I     fname, 
     I     atname, 
     I     sval, 
     I     myThid )

C     !DESCRIPTION:
C     Add a character string attribute to a NetCDF file.

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname, atname, sval
CEOP
      real*4 sZero
      sZero = 0.

      CALL MNC_FILE_ADD_ATTR_ANY(fname,atname, 1, 
     &     sval, 0, 0.0D0, sZero, 0, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_ADD_ATTR_DBL

C     !INTERFACE:
      SUBROUTINE MNC_FILE_ADD_ATTR_DBL( 
     I     fname, 
     I     atname, 
     I     len, 
     I     dval, 
     I     myThid )

C     !DESCRIPTION:
C     Add a double-precision real attribute to a NetCDF file.

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid, len
      character*(*) fname, atname
      REAL*8 dval
CEOP
      real*4 sZero
      sZero = 0.

      CALL MNC_FILE_ADD_ATTR_ANY(fname,atname, 2, 
     &     ' ', len, dval, sZero, 0, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_ADD_ATTR_REAL

C     !INTERFACE:
      SUBROUTINE MNC_FILE_ADD_ATTR_REAL( 
     I     fname, 
     I     atname, 
     I     len, 
     I     rval, 
     I     myThid )

C     !DESCRIPTION:
C     Add a single-precision real attribute to a NetCDF file.

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid, len
      character*(*) fname, atname
      REAL*4 rval
CEOP

      CALL MNC_FILE_ADD_ATTR_ANY(fname,atname, 3, 
     &     ' ', len, 0.0D0, rval, 0, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_ADD_ATTR_INT

C     !INTERFACE:
      SUBROUTINE MNC_FILE_ADD_ATTR_INT( 
     I     fname, 
     I     atname, 
     I     len, 
     I     ival, 
     I     myThid )

C     !DESCRIPTION:
C     Add an integer attribute to a NetCDF file.

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid, len, ival
      character*(*) fname, atname
CEOP
      real*4 sZero
      sZero = 0.

      CALL MNC_FILE_ADD_ATTR_ANY(fname,atname, 4, 
     &     ' ', len, 0.0D0, sZero, ival, myThid )
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_FILE_ADD_ATTR_ANY

C     !INTERFACE:
      SUBROUTINE MNC_FILE_ADD_ATTR_ANY( 
     I     fname, 
     I     atname, 
     I     atype, sv, len,dv,rv,iv, 
     I     myThid )

C     !DESCRIPTION:
C     Add all attributes to a NetCDF file.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid, atype, len, iv
      character*(*) fname, atname, sv
      REAL*8 dv
      REAL*4 rv
CEOP

C     !LOCAL VARIABLES:
      integer n, nf, err, fid, ind, n1, lens
      character*(MNC_MAX_CHAR) s1
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

C     Verify that the file is open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, ind, myThid)
      IF (ind .LT. 0) THEN
        nf = ILNBLNK( fname )
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname(1:nf), ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_INT'
      ENDIF
      fid = mnc_f_info(ind,2)

C     Enter define mode
      CALL MNC_FILE_REDEF(fname, myThid)

      s1(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      n1 = ILNBLNK(atname)
      s1(1:n1) = atname(1:n1)

      IF (atype .EQ. 1) THEN
        lens = ILNBLNK(sv)
        err = NF_PUT_ATT_TEXT(fid, NF_GLOBAL, s1, lens, sv)
        CALL MNC_HANDLE_ERR(err,
     &       'adding TEXT attribute in S/R MNC_FILE_ADD_ATTR_ANY', 
     &       myThid)
      ELSEIF (atype .EQ. 2) THEN
        err = NF_PUT_ATT_DOUBLE(fid, NF_GLOBAL, s1, NF_DOUBLE, len, dv)
        CALL MNC_HANDLE_ERR(err,
     &       'adding DOUBLE attribute in S/R MNC_FILE_ADD_ATTR_ANY',
     &       myThid)
      ELSEIF (atype .EQ. 3) THEN
        err = NF_PUT_ATT_REAL(fid, NF_GLOBAL, s1, NF_FLOAT, len, rv)
        CALL MNC_HANDLE_ERR(err,
     &       'adding REAL attribute in S/R MNC_FILE_ADD_ATTR_ANY',
     &       myThid)
      ELSEIF (atype .EQ. 4) THEN
        err = NF_PUT_ATT_INT(fid, NF_GLOBAL, s1, NF_INT, len, iv)
        CALL MNC_HANDLE_ERR(err,
     &       'adding INT attribute in S/R MNC_FILE_ADD_ATTR_ANY',
     &       myThid)
      ELSE
        write(msgbuf,'(a,i10,a)') 'MNC ERROR: atype = ''', atype, 
     &       ''' is invalid--must be: [1-4]'
        n = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:n), mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_ANY'
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_FILE_CLOSE

C     !INTERFACE:
      SUBROUTINE MNC_FILE_CLOSE( 
     I     fname, 
     I     myThid )

C     !DESCRIPTION:
C     Close a NetCDF file.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer i,j,k,n, err, fid, indf, nf
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

      nf = ILNBLNK(fname)

C     Check that the file is open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
      IF (indf .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC Warning: file ''', fname(1:nf), 
     &       ''' is already closed'
        CALL print_error( msgbuf, mythid )
        RETURN
      ENDIF
      fid = mnc_f_info(indf,2)
      err = NF_CLOSE(fid)
      write(msgbuf,'(3a)') ' cannot close file ''', fname(1:nf), ''''
      CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

C     Clear all the info associated with this file
C     variables
      n = mnc_fv_ids(indf,1)
      IF (n .GE. 1) THEN
        DO i = 1,n
          j = 2 + 3*(i - 1)
          k = mnc_fv_ids(indf,j)
          mnc_v_names(k)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
        ENDDO
        DO i = 1,MNC_MAX_INFO
          mnc_fv_ids(indf,i) = 0
        ENDDO
      ENDIF
C     dims
      n = mnc_f_alld(indf,1)
      mnc_f_alld(indf,1) = 0
      DO i = 1,n
        j = mnc_f_alld(indf,i+1)
        mnc_d_ids(j)  = 0
        mnc_d_size(j) = 0
        mnc_d_names(j)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        mnc_f_alld(indf,i+1) = 0
      ENDDO
C     grids
      n = mnc_f_info(indf,3)
      IF (n .GT. 0) THEN
        DO i = 1,n
          j = 4 + 3*(i - 1)
          k = mnc_f_info(indf,j)
          mnc_g_names(k)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
        ENDDO
        DO i = 1,MNC_MAX_INFO
          mnc_fd_ind(indf,i) = 0
          mnc_f_info(indf,i) = 0
        ENDDO
      ENDIF
C     file name
      DO i = 1,MNC_MAX_PATH
        mnc_f_names(indf)(i:i) = ' '
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_CLOSE_ALL_MATCHING

C     !INTERFACE:
      SUBROUTINE MNC_FILE_CLOSE_ALL_MATCHING( 
     I     fname, 
     I     myThid ) 

C     !DESCRIPTION:
C     Close all files matching a character string.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer i,n

C     Functions
      integer ILNBLNK

      n = ILNBLNK(fname)
      DO i = 1,MNC_MAX_FID

C       Check that the file is open
        IF (fname(1:n) .EQ. mnc_f_names(i)(1:n)) THEN
          CALL MNC_FILE_CLOSE(mnc_f_names(i), myThid)
        ENDIF

      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_CLOSE_ALL

C     !INTERFACE:
      SUBROUTINE MNC_FILE_CLOSE_ALL( 
     I     myThid ) 

C     !DESCRIPTION:
C     Close all NetCDF files.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i
      character*(MNC_MAX_PATH) bpath

      DO i = 1,MNC_MAX_PATH
        bpath(i:i) = ' '
      ENDDO

      DO i = 1,MNC_MAX_FID

C       Check that the file is open
        IF (mnc_f_names(i)(1:MNC_MAX_PATH) 
     &       .NE. bpath(1:MNC_MAX_PATH)) THEN
          CALL MNC_FILE_CLOSE(mnc_f_names(i), myThid)
        ENDIF

      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_REDEF

C     !INTERFACE:
      SUBROUTINE MNC_FILE_REDEF( 
     I     fname, 
     I     myThid )

C     !DESCRIPTION:
C     Set the NetCDF file to DEFINE mode.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer ind, fid, def, err, n
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

C     Verify that the file is open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, ind, myThid)
      IF (ind .LT. 0) THEN
        n = ILNBLNK(fname)
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname(1:n), ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_REDEF'
      ENDIF
      def = mnc_f_info(ind,1)
      fid = mnc_f_info(ind,2)

      IF (def .NE. 1) THEN
C       Enter define mode
        err = NF_REDEF(fid)
        CALL MNC_HANDLE_ERR(err, 
     &       'entering define mode in S/R MNC_FILE_REDEF', myThid)
        mnc_f_info(ind,1) = 1
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_FILE_ENDDEF

C     !INTERFACE:
      SUBROUTINE MNC_FILE_ENDDEF( 
     I     fname, 
     I     myThid )

C     !DESCRIPTION:
C     End DEFINE mode for a NetCDF file.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer ind, fid, def, err, n
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

C     Verify that the file is open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, ind, myThid)
      IF (ind .LT. 0) THEN
        n = ILNBLNK(fname)
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname(1:n), ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_REDEF'
      ENDIF
      def = mnc_f_info(ind,1)
      fid = mnc_f_info(ind,2)

      IF (def .NE. 2) THEN
C       Enter define mode
        err = NF_ENDDEF(fid)
        CALL MNC_HANDLE_ERR(err, 
     &       'ending define mode in S/R MNC_FILE_ENDDEF', myThid)
        mnc_f_info(ind,1) = 2
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_FILE_READALL

C     !INTERFACE:
      SUBROUTINE MNC_FILE_READALL( 
     I     fname, 
     I     myThid ) 

C     !DESCRIPTION:
C     Try to open and read a NetCDF file.
      
C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) fname
CEOP

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer ierr, nff,nlf, indf
      character*(MAX_LEN_MBUF) msgbuf

      nff = IFNBLNK(fname)
      nlf = ILNBLNK(fname)
      CALL MNC_FILE_TRY_READ(fname, ierr, indf, myThid)
      write(msgbuf,'(3a)') 'MNC ERROR: cannot open file ''', 
     &     fname(nff:nlf), ''' for read/write access'
      CALL MNC_HANDLE_ERR(ierr, msgbuf, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_FILE_TRY_READ

C     !INTERFACE:
      SUBROUTINE MNC_FILE_TRY_READ( 
     I     fname, 
     O     ierr, 
     O     indf, 
     I     myThid ) 

C     !DESCRIPTION:
C     Try to open and read a NetCDF file.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid, ierr, indf
      character*(*) fname

C     !LOCAL VARIABLES:
      integer i,j,k, fid, err, ndim,nvar,ngat,unlimid
      integer dlen, id, xtype, nat, nff,nlf, iv
      integer ndv, did, ns,ne, n1,n2, indg, indv
      character*(MAX_LEN_MBUF) msgbuf
      character*(NF_MAX_NAME) name
      integer idlist(NF_MAX_VAR_DIMS)
      character*(MNC_MAX_CHAR) dnames(20)
CEOP
C     Functions
      integer IFNBLNK, ILNBLNK

C     Open and save the filename and fID
      nff = IFNBLNK(fname)
      nlf = ILNBLNK(fname)
      err = NF_OPEN(fname, NF_WRITE, fid)
      ierr = NF_NOERR
      IF (err .NE. NF_NOERR) THEN
        ierr = err
        RETURN
      ENDIF
      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_FID, mnc_f_names,
     &     'mnc_f_names', indf, myThid)
      mnc_f_names(indf)(1:(nlf-nff+1)) = fname(nff:nlf)
      mnc_f_info(indf,2) = fid

C     Get the overall number of entities
      err = NF_INQ(fid, ndim, nvar, ngat, unlimid)
      write(msgbuf,'(4a)') 'MNC ERROR: cannot read number of dims',
     &     ' in file ''', fname(nff:nlf), ''''
      CALL MNC_HANDLE_ERR(err, msgbuf, myThid)

C     Read each dimension and save the information
      DO id = 1,ndim
        err = NF_INQ_DIM(fid, id, name, dlen)
        write(msgbuf,'(2a,i5,3a)') 'MNC ERROR: cannot read dimension',
     &       ' info for dim ''', id, ''' in file ''', 
     &       fname(nff:nlf), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        IF (id .EQ. unlimid) THEN
          dlen = -1
        ENDIF
        ns = IFNBLNK(name)
        ne = ILNBLNK(name)

        CALL MNC_DIM_INIT_ALL(fname,name(ns:ne),dlen,'N', myThid)
        DO i = 1,mnc_f_alld(indf,1)
          j = mnc_f_alld(indf,i+1)
          n1 = IFNBLNK(mnc_d_names(j))
          n2 = ILNBLNK(mnc_d_names(j))
          IF (((ne-ns) .EQ. (n2-n1)) 
     &         .AND. (mnc_d_names(j)(ns:ne) .EQ. name(ns:ne))) THEN
            mnc_d_ids(j) = id
            goto 10
          ENDIF
        ENDDO
 10     CONTINUE
      ENDDO

C     Read and save each variable
      DO id = 1,nvar
        err = NF_INQ_VAR(fid, id, name, xtype, ndv, idlist, nat)
        write(msgbuf,'(2a,i5,3a)') 'MNC ERROR: cannot read variable',
     &       ' info for variable ''', id, ''' in file ''', 
     &       fname(nff:nlf), ''''
        CALL MNC_HANDLE_ERR(err, msgbuf, myThid)
        n1 = IFNBLNK(name)
        n2 = ILNBLNK(name)

C       Create a grid for this variable
        DO i = 1,ndv
          did = idlist(i)
          dnames(i)(1:MNC_MAX_CHAR) = mnc_d_names(did)(1:MNC_MAX_CHAR)
        ENDDO

        CALL MNC_GRID_INIT_ALL(fname, name, ndv, dnames, indg, myThid)

C       Update the tables
        CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID,mnc_v_names,
     &       'mnc_v_names', indv,myThid)
        mnc_v_names(indv)(1:(n2-n1+1)) = name(n1:n2)
        iv = 2 + 3*mnc_fv_ids(indf,1)
        mnc_fv_ids(indf,iv)   = indv
        mnc_fv_ids(indf,iv+1) = id
        DO i = 1,mnc_f_info(indf,3)
          j = 4 + 3*(i-1)
          k = mnc_f_info(indf,j)
          IF (k .EQ. indg) THEN
            mnc_fv_ids(indf,iv+2) = j
            GOTO 20
          ENDIF
        ENDDO
 20     CONTINUE
        mnc_fv_ids(indf,1) = mnc_fv_ids(indf,1) + 1
        
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
