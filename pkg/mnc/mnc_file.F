C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_file.F,v 1.9 2004/01/25 00:22:57 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C==================================================================

      SUBROUTINE MNC_FILE_CREATE( 
     I     myThid,
     I     fname )

      implicit none

C     Arguments
      integer myThid
      character*(*) fname

      CALL MNC_FILE_OPEN(myThid, fname, 0)

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_OPEN( 
     I     myThid,
     I     fname, 
     I     itype )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      integer itype
C     itype => [ 0=new | 1=append ]

C     Functions
      integer ILNBLNK

C     Local Variables
      integer n, err, fid, ind
      character*(MAX_LEN_MBUF) msgbuf

C     Is the file already open?
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC_FILE_OPEN ERROR: ''', fname, 
     &       ''' is already open -- cannot open twice'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: package MNC'
      ENDIF

      write(msgbuf,'(3a)') 'opening ''', fname, ''''
      IF ( itype .EQ. 0 ) THEN

C       Create new file
        err = NF_CREATE(fname, NF_CLOBBER, fid)
        CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

      ELSEIF ( itype .EQ. 1 ) THEN

C       Append to existing file
        CALL MNC_FILE_READALL(myThid, fname)

      ELSE
C       Error
        write(msgbuf,'(a,i5,a)') 'MNC_FILE_OPEN ERROR: ''', itype, 
     &       ''' is not defined--should be: [0|1]'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_STR'
      ENDIF

      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_f_names, ind)
      n = ILNBLNK(fname)
      mnc_f_names(ind)(1:n) = fname(1:n)
      mnc_f_info(ind,1) = 1
      mnc_f_info(ind,2) = fid
      mnc_f_info(ind,3) = 0
      mnc_fv_ids(ind,1) = 0
      mnc_f_alld(ind,1) = 0

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_STR( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     sval )

      implicit none
C     Arguments
      integer myThid
      character*(*) fname, atname, sval

      CALL MNC_FILE_ADD_ATTR_ANY(myThid,fname,atname, 1, 
     &     sval, 0, 0.0D0, 0.0, 0 )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_DBL( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     len, 
     I     dval )

      implicit none
C     Arguments
      integer myThid, len
      character*(*) fname, atname
      REAL*8 dval

      CALL MNC_FILE_ADD_ATTR_ANY(myThid,fname,atname, 2, 
     &     ' ', len, dval, 0.0, 0 )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_REAL( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     len, 
     I     rval )

      implicit none
C     Arguments
      integer myThid, len
      character*(*) fname, atname
      REAL*4 rval

      CALL MNC_FILE_ADD_ATTR_ANY(myThid,fname,atname, 3, 
     &     ' ', len, 0.0D0, rval, 0 )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_INT( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     len, 
     I     ival )

      implicit none
C     Arguments
      integer myThid, len, ival
      character*(*) fname, atname

      CALL MNC_FILE_ADD_ATTR_ANY(myThid,fname,atname, 4, 
     &     ' ', len, 0.0D0, 0.0, ival )
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_ANY( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     atype, sv, len,dv,rv,iv )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, atype, len, iv
      character*(*) fname, atname, sv
      REAL*8 dv
      REAL*4 rv
      
C     Functions
      integer ILNBLNK

C     Local Variables
      integer n, err, fid, ind, n1, lens
      character*(MNC_MAX_CHAR) s1
      character*(MAX_LEN_MBUF) msgbuf
      
C     Verify that the file is open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF (ind .LT. 0) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_INT'
      ENDIF
      fid = mnc_f_info(ind,2)

C     Enter define mode
      CALL MNC_FILE_REDEF(myThid, fname)

      s1(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      n1 = ILNBLNK(atname)
      s1(1:n1) = atname(1:n1)

      IF (atype .EQ. 1) THEN
        lens = ILNBLNK(sv)
        err = NF_PUT_ATT_TEXT(fid, NF_GLOBAL, s1, lens, sv)
        CALL MNC_HANDLE_ERR(myThid, err,
     &       'adding TEXT attribute in S/R MNC_FILE_ADD_ATTR_ANY')
      ELSEIF (atype .EQ. 2) THEN
        err = NF_PUT_ATT_DOUBLE(fid, NF_GLOBAL, s1, NF_DOUBLE, len, dv)
        CALL MNC_HANDLE_ERR(myThid, err,
     &       'adding DOUBLE attribute in S/R MNC_FILE_ADD_ATTR_ANY')
      ELSEIF (atype .EQ. 3) THEN
        err = NF_PUT_ATT_REAL(fid, NF_GLOBAL, s1, NF_FLOAT, len, rv)
        CALL MNC_HANDLE_ERR(myThid, err,
     &       'adding REAL attribute in S/R MNC_FILE_ADD_ATTR_ANY')
      ELSEIF (atype .EQ. 4) THEN
        err = NF_PUT_ATT_INT(fid, NF_GLOBAL, s1, NF_INT, len, iv)
        CALL MNC_HANDLE_ERR(myThid, err,
     &       'adding INT attribute in S/R MNC_FILE_ADD_ATTR_ANY')
      ELSE
        write(msgbuf,'(a,i10,a)') 'MNC ERROR: atype = ''', atype, 
     &       ''' is invalid--must be: [1-4]'
        n = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:n), mythid)
        stop 'ABNORMAL END: S/R MNC_VAR_ADD_ATTR_ANY'
      ENDIF

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_CLOSE( 
     I     myThid,
     I     fname )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname

C     Local Variables
      integer i,j,k,n, err, fid, ind
      character*(MAX_LEN_MBUF) msgbuf

C     Check that the file is open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF (ind .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC Warning: file ''', fname, 
     &       ''' is already closed'
        CALL print_error( msgbuf, mythid )
        RETURN
      ENDIF
      fid = mnc_f_info(ind,2)
      err = NF_CLOSE(fid)
      write(msgbuf,'(3a)') ' cannot close file ''', fname, ''''
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

C     Clear all the info associated with this file
C     variables
      n = mnc_fv_ids(ind,1)
      IF (n .GE. 1) THEN
        DO i = 1,n
          j = 2 + 3*(i - 1)
          k = mnc_fv_ids(ind,j)
          mnc_v_names(k)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
        ENDDO
        DO i = 1,(1 + 3*n)
          mnc_fv_ids(ind,i) = 0
        ENDDO
      ENDIF
C     dims
      n = mnc_f_alld(ind,1)
      mnc_f_alld(ind,1) = 0
      DO i = 1,n
        j = mnc_f_alld(ind,i+1)
        mnc_d_ids(j)  = 0
        mnc_d_size(j) = 0
        mnc_d_names(j)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
        mnc_f_alld(ind,i+1) = 0
      ENDDO
C     grids
      n = mnc_f_info(ind,3)
      IF (n .GT. 0) THEN
        DO i = 1,n
          j = 4 + 3*(i - 1)
          k = mnc_f_info(ind,j)
          mnc_g_names(k)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
        ENDDO
        DO i = 1,MNC_MAX_INFO
          mnc_fd_ind(ind,i) = 0
          mnc_f_info(ind,i) = 0
        ENDDO
      ENDIF
C     file name
      mnc_f_names(ind)(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_CLOSE_ALL_MATCHING( 
     I     myThid, 
     I     fname ) 

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i,n

      n = ILNBLNK(fname)
      DO i = 1,MNC_MAX_ID

C       Check that the file is open
        IF (fname(1:n) .EQ. mnc_f_names(i)(1:n)) THEN
          CALL MNC_FILE_CLOSE(myThid,mnc_f_names(i))
        ENDIF

      ENDDO

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_CLOSE_ALL( 
     I     myThid ) 

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid

C     Local Variables
      integer i

      DO i = 1,MNC_MAX_ID

C       Check that the file is open
        IF (mnc_f_names(i)(1:MNC_MAX_CHAR) 
     &       .NE. mnc_blank_name(1:MNC_MAX_CHAR)) THEN
          CALL MNC_FILE_CLOSE(myThid,mnc_f_names(i))
        ENDIF

      ENDDO

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_REDEF( 
     I     myThid,
     I     fname )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname

C     Functions
      integer ILNBLNK

C     Local Variables
      integer ind, fid, def, err, n
      character*(MAX_LEN_MBUF) msgbuf

C     Verify that the file is open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
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
        CALL MNC_HANDLE_ERR(myThid, err, 
     &       'entering define mode in S/R MNC_FILE_REDEF')
        mnc_f_info(ind,1) = 1
      ENDIF

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_ENDDEF( 
     I     myThid,
     I     fname )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname

C     Functions
      integer ILNBLNK

C     Local Variables
      integer ind, fid, def, err, n
      character*(MAX_LEN_MBUF) msgbuf

C     Verify that the file is open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
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
        CALL MNC_HANDLE_ERR(myThid, err, 
     &       'ending define mode in S/R MNC_FILE_ENDDEF')
        mnc_f_info(ind,1) = 2
      ENDIF

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_FILE_READALL( 
     I     myThid, 
     I     fname ) 

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer i,j,k, fid, err, ndim,nvar,ngat,unlimid
      integer dlen, id, indf, xtype, nat, nff,nlf, iv
      integer ndv, did, ns,ne, n1,n2, indg, indv
      character*(MAX_LEN_MBUF) msgbuf
      character*(NF_MAX_NAME) name
      integer idlist(NF_MAX_VAR_DIMS)
      character*(MNC_MAX_CHAR) dnames(20)

C     Open and save the filename and fID
      nff = IFNBLNK(fname)
      nlf = ILNBLNK(fname)
      err = NF_OPEN(fname, NF_WRITE, fid)
      write(msgbuf,'(3a)') 'MNC ERROR: cannot open file ''', 
     &     fname(nff:nlf), ''' for read/write access'
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)
      CALL MNC_GET_NEXT_EMPTY_IND(myThid,MNC_MAX_ID,mnc_f_names,indf) 
      mnc_f_names(indf)(1:(nlf-nff+1)) = fname(nff:nlf)
      mnc_f_info(indf,2) = fid

C     Get the overall number of entities
      err = NF_INQ(fid, ndim, nvar, ngat, unlimid)
      write(msgbuf,'(4a)') 'MNC ERROR: cannot read number of dims',
     &     ' in file ''', fname(nff:nlf), ''''
      CALL MNC_HANDLE_ERR(myThid, err, msgbuf)

C     Read each dimension and save the information
      DO id = 1,ndim
        err = NF_INQ_DIM(fid, id, name, dlen)
        write(msgbuf,'(2a,i5,3a)') 'MNC ERROR: cannot read dimension',
     &       ' info for dim ''', id, ''' in file ''', 
     &       fname(nff:nlf), ''''
        CALL MNC_HANDLE_ERR(myThid, err, msgbuf)
        IF (id .EQ. unlimid) THEN
          dlen = -1
        ENDIF
        ns = IFNBLNK(name)
        ne = ILNBLNK(name)
        CALL MNC_DIM_INIT_ALL(myThid,fname,name(ns:ne),dlen,'N')
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
        CALL MNC_HANDLE_ERR(myThid, err, msgbuf)
        n1 = IFNBLNK(name)
        n2 = ILNBLNK(name)
        
C       Create a grid for this variable
        DO i = 1,ndv
          did = idlist(i)
          dnames(i)(1:MNC_MAX_CHAR) = mnc_d_names(did)(1:MNC_MAX_CHAR)
        ENDDO
        CALL MNC_GRID_INIT_ALL(myThid, fname, name, ndv, dnames, indg)

C       Update the tables
        CALL MNC_GET_NEXT_EMPTY_IND(myThid,MNC_MAX_ID,mnc_v_names,indv)
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

