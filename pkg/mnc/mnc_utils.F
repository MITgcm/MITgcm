C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_utils.F,v 1.3 2004/01/07 19:50:52 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C==================================================================

      SUBROUTINE MNC_HANDLE_ERR( myThid, status, msg )

      implicit none
#include "netcdf.inc"
#include "EEPARAMS.h"

C     Arguments
      INTEGER  myThid, status
      character*(*) msg

C     Functions
      integer ILNBLNK

C     Locals
      integer lenm
      character*(MAX_LEN_MBUF) msgbuf

      IF ( status .NE. NF_NOERR ) THEN
        write(msgbuf,'(2a)') 'NetCDF ERROR: ', NF_STRERROR(status)
        lenm = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:lenm), mythid)
        write(msgbuf,'(2a)') 'MNC ERROR: ', msg
        lenm = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:lenm), mythid)
        stop 'ABNORMAL END: package MNC'
      ENDIF
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_GET_IND(
     I     myThid,
     I     NT,
     I     aname,
     I     name_list,
     O     ind )

      implicit none
#include "EEPARAMS.h"

C     Arguments
      integer myThid, nt, id
      character*(*) aname
      character*(*) name_list(NT)

C     Functions
      integer ILNBLNK

C     Locals
      integer n, i, nf, ind
      character*(MAX_LEN_MBUF) msgbuf

C     Check that aname contains a valid name
      n = ILNBLNK( aname )
      IF ( n .LT. 1 ) THEN
        write(msgbuf,'(a)')
     &       'MNC_GET_IND: an invalid (empty) name was specified'
        CALL print_error( msgbuf, myThid )
        stop 'ABNORMAL END: S/R MNC_GET_ID'
      ENDIF

C     Search for the index
      DO i=1,NT
        nf = ILNBLNK( name_list(i) )
        IF ( nf .EQ. n ) THEN
          IF ( name_list(i)(1:n) .EQ. aname(1:n) ) THEN
            ind = i
            GOTO 10
          ENDIF
        ENDIF
      ENDDO
      ind = -1
 10   CONTINUE
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_GET_NEXT_EMPTY_IND(
     I     myThid,
     I     NT,
     I     name_list,
     O     ind )

      implicit none
#include "EEPARAMS.h"

C     Arguments
      integer myThid, nt, id
      character*(*) name_list(NT)

C     Functions
      integer ILNBLNK

C     Locals
      integer n, i, ind
      character*(MAX_LEN_MBUF) msgbuf

C     Search for the index
      DO i=1,NT
        n = ILNBLNK( name_list(i) )
        IF ( n .EQ. 0 ) THEN
          ind = i
          GOTO 10
        ENDIF
      ENDDO

C     If this is code is reached, we have exceeded the array size
      write(msgbuf,'(a,i6,a)')
     &     'MNC_GET_NEXT_EMPTY_IND: array size ', nt, 
     &     ' exceeded--try increasing MNC_MAX_ID'
      CALL print_error( msgbuf, myThid )
      stop 'ABNORMAL END: S/R MNC_GET_NEXT_EMPTY_IND'

 10   CONTINUE
      RETURN
      END

C==================================================================

      SUBROUTINE MNC_GET_FVINDS( 
     I     myThid, 
     I     fname, 
     I     vname, 
     O     indf, 
     O     ind_fv_ids )

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"

C     Arguments
      INTEGER  myThid, fid, indf, ind_fv_ids
      character*(*) fname
      character*(*) vname

C     Functions
      integer ILNBLNK

C     Locals
      integer i,j,k, n, nv, lenf,lenv

C     Strip trailing spaces
      lenf = ILNBLNK(fname)
      lenv = ILNBLNK(vname)

C     Check that the file exists
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF (indf .LT. 1) THEN
        ind_fv_ids = -1
        RETURN
      ENDIF
      fid = mnc_f_info(indf,2)

C     Find the vID
      nv = mnc_fv_ids(indf,1)
      DO i = 1,nv
        k = 2 + (i - 1)*3
        j = mnc_fv_ids(indf,k)
        n = ILNBLNK(mnc_v_names(j))
        IF ((n .EQ. lenv) 
     &       .AND. (mnc_v_names(j)(1:n) .EQ. vname(1:n))) THEN
          ind_fv_ids = k
          GOTO 10
        ENDIF
      ENDDO
      ind_fv_ids = -1
 10   CONTINUE

      RETURN
      END

