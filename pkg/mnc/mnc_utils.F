C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_utils.F,v 1.1 2004/01/05 06:20:08 edhill Exp $
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

C     Locals
      character*(MAX_LEN_MBUF) msgbuf

      IF ( status .NE. NF_NOERR ) THEN
        write(msgbuf,'(2a)') 'NetCDF ERROR: ', NF_STRERROR(status)
        CALL print_error( msgbuf, mythid )
        write(msgbuf,'(2a)') 'MNC ERROR: ', msg
        CALL print_error( msgbuf, mythid )
        stop 'ABNORMAL END: package MNC'
      ENDIF
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

