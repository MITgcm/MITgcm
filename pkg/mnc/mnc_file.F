C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_file.F,v 1.2 2004/01/05 21:38:27 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

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

C     Local Variables
      integer i, err, fid, ind
      character*(MNC_MAX_CHAR) aname
      character*(MAX_LEN_MBUF) msgbuf

C     Is the file already open?
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC_FILE_OPEN ERROR: ''', fname, 
     &       ''' is already open -- cannot open twice'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: package MNC'
      ENDIF

      IF ( itype .EQ. 0 ) THEN
C       Create new file
        err = NF_CREATE( fname, NF_CLOBBER, fid )
        IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)
      ELSEIF ( itype .EQ. 1 ) THEN
C       Append to existing file
        err = NF_OPEN( fname, NF_WRITE, fid )
        IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)
      ELSE
C       Error
        write(msgbuf,'(a,i5,a)') 'MNC_FILE_OPEN ERROR: ''', itype, 
     &       ''' is not defined--should be: [0|1]'
      ENDIF

      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_f_names, ind)
      mnc_f_names(ind) = fname
      mnc_f_info(ind,1) = 1
      mnc_f_info(ind,1) = fid

      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_STR( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     sval )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname
      character*(*) atname
      character*(*) sval

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, err, fid, ind, n1,n2
      character*(MNC_MAX_CHAR) s1, s2
      character*(MAX_LEN_MBUF) msgbuf

C     Is the file open?
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_STR'
      ENDIF
      fid = mnc_f_info(ind,2)

C     Enter define mode
      err = NF_REDEF(fid)
      IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)

      s1(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      s2(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      n1 = ILNBLNK(atname)
      n2 = ILNBLNK(sval)
      s1(1:n1) = atname(1:n1)
      s2(1:n2) = sval(1:n2)

      err = NF_PUT_ATT_TEXT(fid, NF_GLOBAL, s1, n2, s2)
      IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)

      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_INT( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     ival )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname, atname
      integer ival

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, err, fid, ind, n1,n2
      character*(MNC_MAX_CHAR) s1
      character*(MAX_LEN_MBUF) msgbuf

C     Is the file open?
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_INT'
      ENDIF
      fid = mnc_f_info(ind,2)

C     Enter define mode
      err = NF_REDEF(fid)
      IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)

      s1(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      n1 = ILNBLNK(atname)
      s1(1:n1) = atname(1:n1)

      err = NF_PUT_ATT_INT(fid, NF_GLOBAL, s1, NF_INT, ival)
      IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)

      END

C==================================================================

      SUBROUTINE MNC_FILE_ADD_ATTR_DBL( 
     I     myThid, 
     I     fname, 
     I     atname, 
     I     dval )

#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      character*(*) fname, atname
      _RL dval

C     Functions
      integer ILNBLNK

C     Local Variables
      integer i, err, fid, ind, n1,n2
      character*(MNC_MAX_CHAR) s1
      character*(MAX_LEN_MBUF) msgbuf
      

C     Is the file open?
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, ind)
      IF ( ind .GT. 0 ) THEN
        write(msgbuf,'(3a)') 'MNC ERROR: file ''',
     &       fname, ''' must be opened first'
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: S/R MNC_FILE_ADD_ATTR_INT'
      ENDIF
      fid = mnc_f_info(ind,2)

C     Enter define mode
      err = NF_REDEF(fid)
      IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)

      s1(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
      n1 = ILNBLNK(atname)
      s1(1:n1) = atname(1:n1)

      err = NF_PUT_ATT_DOUBLE(fid, NF_GLOBAL, s1, NF_DOUBLE, ival)
      IF (err .NE. NF_NOERR) CALL MNC_HANDLE_ERR(myThid, err)

      END

