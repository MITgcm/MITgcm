C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_utils.F,v 1.8 2004/02/26 22:31:58 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


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
      integer i,lenm
      character*(MAX_LEN_MBUF) msgbuf
      DO i = 1,MAX_LEN_MBUF
        msgbuf(i:i) = ' '
      ENDDO

      IF ( status .NE. NF_NOERR ) THEN
        write(msgbuf,'(2a)') 'NetCDF ERROR: ', NF_STRERROR(status)
        lenm = ILNBLNK(msgbuf)
        print *, msgbuf(1:lenm)
        CALL print_error(msgbuf(1:lenm), mythid)
        write(msgbuf,'(2a)') 'MNC ERROR: ', msg
        lenm = ILNBLNK(msgbuf)
        print *, msgbuf(1:lenm)
        CALL print_error(msgbuf(1:lenm), mythid)
        stop 'ABNORMAL END: package MNC'
      ENDIF
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


      SUBROUTINE MNC_GET_IND(
     I     myThid,
     I     NT,
     I     aname,
     I     name_list,
     O     ind )

      implicit none
#include "EEPARAMS.h"

C     Arguments
      integer myThid, nt
      character*(*) aname
      character*(*) name_list(NT)

C     Functions
      integer ILNBLNK

C     Locals
      integer n, i, nf, ind, lenm
      character*(MAX_LEN_MBUF) msgbuf

C     Check that aname contains a valid name
      n = ILNBLNK( aname )
      IF ( n .LT. 1 ) THEN
        write(msgbuf,'(a)')
     &       'MNC_GET_IND: an invalid (empty) name was specified'
        lenm = ILNBLNK(msgbuf)
        CALL print_error(msgbuf(1:lenm), myThid)
        stop 'ABNORMAL END: S/R MNC_GET_IND'
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


      SUBROUTINE MNC_GET_NEXT_EMPTY_IND(
     I     myThid,
     I     NT,
     I     name_list,
     O     ind )

      implicit none
#include "EEPARAMS.h"

C     Arguments
      integer myThid, nt
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

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
      integer i,j,k, n, lenv

C     Strip trailing spaces
      lenv = ILNBLNK(vname)

C     Check that the file exists
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF (indf .LT. 1) THEN
        ind_fv_ids = -1
        RETURN
      ENDIF
      fid = mnc_f_info(indf,2)

C     Find the vID
      DO i = 1,mnc_fv_ids(indf,1)
        k = 2 + 3*(i - 1)
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     The purpose of this function is to determine whether the
C     dimensions (sizes) of a specific variable within the MNC low-level
C     look-up tables matches the dimensions of a Variable Type defined
C     within the upper-level CW layer.
C
C     YES  ==>  ires > 0
C     NO   ==>  ires < 0

      SUBROUTINE MNC_COMP_VTYPE_VAR( 
     I     myThid, 
     I     ind_vt, 
     I     indf,
     I     ind_fv_ids, 
     O     ires )

      implicit none
#include "mnc_common.h"

C     Arguments
      INTEGER  myThid, ind_vt, indf, ind_fv_ids, ires

C     Locals
      integer      i,k, ind_cw_g, ig,ids,ide,nint
      character*1  ch

      ires = -1

C     grid index for the internal  representation
      ig = mnc_fv_ids(indf,ind_fv_ids+2)
      ids = mnc_f_info(indf,ig+1)
      ide = mnc_f_info(indf,ig+2)
      nint = ids - ide + 1

C     GType index for CW
C       ind_cw_g = mnc_cw_vgind(ind_vt)
C       IF (nint .NE. mnc_cw_ndim(ind_cw_g)) THEN
C         RETURN
C       ENDIF
C       k = 0
C       DO i = ids,ide
C         k = k + 1
C         indd = mnc_fd_ind(indf,i)
C         ch = mnc_d_names(indd)(1:1)
C         cw_len = mnc_cw_ie(k,ind_cw_g) - mnc_cw_is(k,ind_cw_g) + 1
C         IF (ch .EQ. 'X') THEN
          
C         ELSEIF (ch .EQ. 'Y') THEN
          
C         ELSE
          
C         ENDIF
        
C         IF (mnc_fd_ind(indf,i) .NE. cw_len) THEN
C           RETURN
C         ENDIF
C       ENDDO

C     Reaching this point means all tests passed
      ires = 1

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
