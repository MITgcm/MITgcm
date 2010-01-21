C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_utils.F,v 1.23 2010/01/21 01:48:05 jmc Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C--  File mnc_utils.F:
C--   Contents
C--   o MNC_HANDLE_ERR
C--   o MNC_GET_IND
C--   o MNC_GET_NEXT_EMPTY_IND
C--   o MNC_GET_FVINDS
C--   o MNC_CHK_VTYP_R_NCVAR
C--   o MNC_PSNCM

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_HANDLE_ERR

C     !INTERFACE:
      SUBROUTINE MNC_HANDLE_ERR( status, msg, myThid )

C     !DESCRIPTION:
C     Convenience function for handling all MNC and NetCDF library
C     errors.

C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !DESCRIPTION:
C     Create an MNC grid within a NetCDF file context.

C     !USES:
      INTEGER  myThid, status
      character*(*) msg
CEOP

C     !LOCAL VARIABLES:
      integer i,lenm
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

      DO i = 1,MAX_LEN_MBUF
        msgbuf(i:i) = ' '
      ENDDO

      IF ( status .NE. NF_NOERR ) THEN
        write(msgbuf,'(2a)') 'NetCDF ERROR: '
        lenm = ILNBLNK(msgbuf)
        print *, msgbuf(1:lenm)
        CALL print_error(msgbuf(1:lenm), mythid)
        print *, '==='
        print *, NF_STRERROR(status)
        print *, '==='
        lenm = ILNBLNK(msg)
        lenm = MIN(lenm,MAX_LEN_MBUF-11)
        write(msgbuf,'(2a)') 'MNC ERROR: ', msg(1:lenm)
        lenm = ILNBLNK(msgbuf)
        print *, msgbuf(1:lenm)
        CALL print_error(msgbuf(1:lenm), mythid)
        stop 'ABNORMAL END: package MNC'
      ENDIF
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_GET_IND

C     !INTERFACE:
      SUBROUTINE MNC_GET_IND(
     I     NT,
     I     aname,
     I     name_list,
     O     ind,
     I     myThid )

C     !DESCRIPTION:
C     Get the index of the specified name.

C     !USES:
      implicit none
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid, nt
      character*(*) aname
      character*(*) name_list(NT)
CEOP

C     !LOCAL VARIABLES:
      integer n, i, nf, ind, lenm
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK

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
CBOP 1
C     !ROUTINE: MNC_GET_NEXT_EMPTY_IND

C     !INTERFACE:
      SUBROUTINE MNC_GET_NEXT_EMPTY_IND(
     I     NT,
     I     name_list,
     I     var_symb,
     O     ind,
     I     myThid )

C     !DESCRIPTION:
C     Get the index of the next empty entry.

C     !USES:
      implicit none
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid, nt
      character*(*) name_list(NT)
      character*(*) var_symb
CEOP

C     !LOCAL VARIABLES:
      integer n, i, ind
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer ILNBLNK


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
     &     ' exceeded'
      CALL print_error( msgbuf, myThid )
      n = ILNBLNK( var_symb )
      write(msgbuf,'(a,a,a)')
     &     'MNC_GET_NEXT_EMPTY_IND: occurred within the ''',
     &     var_symb(1:n), ''' array'
      CALL print_error( msgbuf, myThid )
      stop 'ABNORMAL END: S/R MNC_GET_NEXT_EMPTY_IND'

 10   CONTINUE
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_GET_FVINDS

C     !INTERFACE:
      SUBROUTINE MNC_GET_FVINDS(
     I     fname,
     I     vname,
     O     indf,
     O     ind_fv_ids,
     I     myThid )

C     !DESCRIPTION:
C     Get the variable indicies.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      INTEGER  myThid, fid, indf, ind_fv_ids
      character*(*) fname
      character*(*) vname
CEOP

C     !LOCAL VARIABLES:
      integer i,j,k, n, lenv

C     Functions
      integer ILNBLNK

C     Strip trailing spaces
      lenv = ILNBLNK(vname)

C     Check that the file exists
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
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
        IF ( n.EQ.lenv ) THEN
         IF ( mnc_v_names(j)(1:n).EQ.vname(1:n) ) THEN
          ind_fv_ids = k
          GOTO 10
         ENDIF
        ENDIF
      ENDDO
      ind_fv_ids = -1
 10   CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Here, we determine whether the dimensions (sizes) of a specific
C     variable within the MNC low-level look-up tables matches the
C     dimensions of a Variable Type defined within the upper-level CW
C     layer.
C
C     Return values:
C     .  YES  ==>  ires > 0
C     .  NO   ==>  ires < 0

      SUBROUTINE MNC_CHK_VTYP_R_NCVAR(
     I     ind_vt,
     I     indf,
     I     ind_fv_ids,
     I     indu,
     O     ires,
     I     myThid )

      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     Arguments
      INTEGER  myThid, ind_vt, indf, ind_fv_ids, indu, ires

C     Functions
      integer ILNBLNK

C     Locals
      integer  ii,k, ind_cw_g, ig,ids,ide,nint, indd, nk
      integer  ndim_vt, ncgt,ncvr,ncvf, npb, sz_min
      character*(MAX_LEN_MBUF) pbuf, msgbuf

      ires = -1

C     grid indicies for the internal (as-read-from-the-file) data
      ig = mnc_fv_ids(indf,ind_fv_ids+2)
      ids = mnc_f_info(indf,ig+1)
      ide = mnc_f_info(indf,ig+2)
      nint = ids - ide + 1

      ind_cw_g = mnc_cw_vgind(ind_vt)
      ncgt = ILNBLNK(mnc_cw_gname(ind_cw_g))
      ncvr = ILNBLNK(mnc_v_names(mnc_fv_ids(indf,ind_fv_ids)))
      ncvf = ILNBLNK(mnc_f_names(indf))
      write(pbuf,'(7a)') 'MNC_CHK_VTYP_R_NCVAR WARNING: var ''',
     &     mnc_v_names(mnc_fv_ids(indf,ind_fv_ids))(1:ncvr),
     &     ''' within file ''', mnc_f_names(indf)(1:ncvf),
     &     ''' does not satisy the size needed by GType ''',
     &     mnc_cw_gname(ind_cw_g)(1:ncgt), ''''
      npb = ILNBLNK(pbuf)
      ndim_vt = mnc_cw_ndim(ind_cw_g)
      nk = nint
      IF (ndim_vt .LT. nk)  nk = ndim_vt
      IF (nint .NE. ndim_vt) THEN
        write(msgbuf,'(2a)') pbuf(1:npb), ' -- too few dims'
        CALL print_error(msgbuf, myThid)
      ENDIF

C     Check that the necessary size exists along each dimension
      DO k = 1,nk
        ii = ids + (k - 1)
        sz_min = mnc_cw_dims(k,ind_cw_g)
        IF (sz_min .EQ. -1)  sz_min = indu
        indd = mnc_fd_ind(indf,ii)
        IF (mnc_d_size(indd) .LT. sz_min) THEN
          write(msgbuf,'(2a,i3,a,i3,a,i3)') pbuf(1:npb), ': dim #',
     &         k, ' is too small: ', mnc_d_size(indd), ' vs ',
     &         mnc_cw_ie(k,ind_cw_g)
          CALL print_error(msgbuf, myThid)
          RETURN
        ENDIF
      ENDDO

C     Reaching this point means all tests passed
      ires = 1

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_PSNCM

C     !INTERFACE:
      SUBROUTINE MNC_PSNCM(
     O     ostring,
     I     ival, n )

C     !DESCRIPTION:
C     Print a zero-padded integer to a String with an N-Character
C     Minimum length

      IMPLICIT NONE

C     !INPUT PARAMETERS:
C     ostring :: String to contain formatted output
      CHARACTER*(*) ostring
      INTEGER ival, n
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, lens, nmin
      CHARACTER*(25) tmp

      lens = LEN(ostring)
      DO i = 1,lens
        ostring(i:i) = ' '
      ENDDO
      WRITE(tmp,'(I25.25)') ival
      DO i = 1,25
        IF (tmp(i:i) .NE. '0') THEN
          nmin = 26 - i
          GOTO 200
        ENDIF
      ENDDO
 200  CONTINUE
      IF (nmin .LT. n)  nmin = n
      ostring(1:nmin) = tmp((26-nmin):25)

C
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

