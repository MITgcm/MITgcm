C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cwrapper.F,v 1.35 2010/01/21 01:48:05 jmc Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C--  File mnc_cwrapper.F:
C--   Contents
C--   o MNC_CW_ADD_GNAME
C--   o MNC_CW_DEL_GNAME
C--   o MNC_CW_DUMP
C--   o MNC_CW_APPEND_VNAME
C--   o MNC_CW_ADD_VNAME
C--   o MNC_CW_DEL_VNAME
C--   o MNC_CW_ADD_VATTR_TEXT
C--   o MNC_CW_ADD_VATTR_INT
C--   o MNC_CW_ADD_VATTR_DBL
C--   o MNC_CW_ADD_VATTR_ANY
C--   o MNC_CW_GET_TILE_NUM
C--   o MNC_CW_GET_FACE_NUM
C--   o MNC_CW_GET_XYFO
C--   o MNC_CW_FILE_AORC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_ADD_GNAME

C     !INTERFACE:
      SUBROUTINE MNC_CW_ADD_GNAME(
     I     name,
     I     ndim,
     I     dlens,
     I     dnames,
     I     inds_beg, inds_end,
     I     myThid )

C     !DESCRIPTION:
C     Add a grid name to the MNC convenience wrapper layer.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid, ndim
      character*(*) name
      integer dlens(*), inds_beg(*), inds_end(*)
      character*(*) dnames(*)
CEOP

C     !LOCAL VARIABLES:
      integer i, nnf,nnl, indg
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer IFNBLNK, ILNBLNK

      nnf = IFNBLNK(name)
      nnl = ILNBLNK(name)

C     Check that this name is not already defined
      CALL MNC_GET_IND(MNC_MAX_ID, name, mnc_cw_gname, indg, myThid)
      IF (indg .GT. 0) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_GNAME ERROR: ''', name,
     &       ''' is already defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_GNAME'
      ENDIF
      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID, mnc_cw_gname,
     &     'mnc_cw_gname', indg, myThid)

      mnc_cw_gname(indg)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
      mnc_cw_gname(indg)(1:(nnl-nnf+1)) = name(nnf:nnl)
      mnc_cw_ndim(indg) = ndim

      DO i = 1,ndim
        mnc_cw_dn(i,indg)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
        nnf = IFNBLNK(dnames(i))
        nnl = ILNBLNK(dnames(i))
        mnc_cw_dn(i,indg)(1:(nnl-nnf+1)) = dnames(i)(nnf:nnl)
        mnc_cw_dims(i,indg) = dlens(i)
        mnc_cw_is(i,indg)   = inds_beg(i)
        mnc_cw_ie(i,indg)   = inds_end(i)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_DEL_GNAME

C     !INTERFACE:
      SUBROUTINE MNC_CW_DEL_GNAME(
     I     name,
     I     myThid )

C     !DESCRIPTION:
C     Delete a grid name from the MNC convenience wrapper layer.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) name
CEOP

C     !LOCAL VARIABLES:
      integer nnf,nnl, indg

C     Functions
      integer IFNBLNK, ILNBLNK

      nnf = IFNBLNK(name)
      nnl = ILNBLNK(name)

C     Check that this name is not already defined
      CALL MNC_GET_IND(MNC_MAX_ID, name, mnc_cw_gname, indg, myThid)
      IF (indg .LT. 1) THEN
        RETURN
      ENDIF

      mnc_cw_gname(indg)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
      mnc_cw_ndim(indg) = 0

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_DUMP

C     !INTERFACE:
      SUBROUTINE MNC_CW_DUMP( myThid )

C     !DESCRIPTION:
C     Write a condensed view of the current state of the MNC look-up
C     tables for the convenience wrapper section.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j, ntot
      integer NBLNK
      parameter ( NBLNK = 150 )
      character s1*(NBLNK), blnk*(NBLNK)

      _BEGIN_MASTER(myThid)

      DO i = 1,NBLNK
        blnk(i:i) = ' '
      ENDDO

      s1(1:NBLNK) = blnk(1:NBLNK)
      write(s1,'(a5,a)') 'MNC: ',
     &     'The currently defined Grid Types are:'
      CALL PRINT_MESSAGE(
     &     s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)
      ntot = 0
      DO j = 1,MNC_MAX_ID
        IF (mnc_cw_gname(j)(1:MNC_MAX_CHAR)
     &       .NE. mnc_blank_name(1:MNC_MAX_CHAR)) THEN

          ntot = ntot + 1
          s1(1:NBLNK) = blnk(1:NBLNK)
          write(s1,'(a5,2i5,a3,a20,i3,a3,5i4,a4,5i4,a4,5i4,6a8)')
     &         'MNC: ',
     &         j, ntot, ' : ', mnc_cw_gname(j)(1:20), mnc_cw_ndim(j),
     &         ' : ', (mnc_cw_dims(i,j), i=1,5),
     &         '  | ', (mnc_cw_is(i,j), i=1,5),
     &         '  | ', (mnc_cw_ie(i,j), i=1,5),
     &         '  | ', (mnc_cw_dn(i,j)(1:7), i=1,5)
          CALL PRINT_MESSAGE(
     &         s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)

        ENDIF
      ENDDO

      s1(1:NBLNK) = blnk(1:NBLNK)
      write(s1,'(a5,a)') 'MNC: ',
     &     'The currently defined Variable Types are:'
      CALL PRINT_MESSAGE(
     &     s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)
      ntot = 0
      DO j = 1,MNC_MAX_ID
        IF (mnc_cw_vname(j)(1:MNC_MAX_CHAR)
     &       .NE. mnc_blank_name(1:MNC_MAX_CHAR)) THEN

          ntot = ntot + 1
          s1(1:NBLNK) = blnk(1:NBLNK)
          write(s1,'(a5,2i5,a3,a25,a3,i4)') 'MNC: ',
     &         j, ntot, ' | ',
     &         mnc_cw_vname(j)(1:20), ' | ', mnc_cw_vgind(j)
          CALL PRINT_MESSAGE(
     &         s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)

          DO i = 1,mnc_cw_vnat(1,j)
            s1(1:NBLNK) = blnk(1:NBLNK)
            write(s1,'(a5,a14,i4,a3,a25,a3,a55)')
     &           'MNC: ','      text_at:',i,
     &           ' : ', mnc_cw_vtnm(i,j)(1:25), ' : ',
     &           mnc_cw_vtat(i,j)(1:MNC_MAX_CHAR)
            CALL PRINT_MESSAGE(
     &           s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)
          ENDDO
          DO i = 1,mnc_cw_vnat(2,j)
            s1(1:NBLNK) = blnk(1:NBLNK)
            write(s1,'(a5,a14,i4,a3,a25,a3,i20)')
     &           'MNC: ','      int__at:',i,
     &           ' : ', mnc_cw_vinm(i,j)(1:25), ' : ',
     &           mnc_cw_viat(i,j)
            CALL PRINT_MESSAGE(
     &           s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)
          ENDDO
          DO i = 1,mnc_cw_vnat(3,j)
            s1(1:NBLNK) = blnk(1:NBLNK)
            write(s1,'(a5,a14,i4,a3,a25,a3,f25.10)')
     &           'MNC: ','      dbl__at:',i,
     &           ' : ', mnc_cw_vdnm(i,j)(1:25), ' : ',
     &           mnc_cw_vdat(i,j)
            CALL PRINT_MESSAGE(
     &           s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)
        ENDDO

        ENDIF
      ENDDO
      IF (ntot .EQ. 0) THEN
        s1(1:NBLNK) = blnk(1:NBLNK)
        write(s1,'(a)') 'MNC:    None defined!'
        CALL PRINT_MESSAGE(
     &       s1, standardMessageUnit, SQUEEZE_RIGHT, mythid)
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_APPEND_VNAME

C     !INTERFACE:
      SUBROUTINE MNC_CW_APPEND_VNAME(
     I     vname,
     I     gname,
     I     bi_dim, bj_dim,
     I     myThid )

C     !DESCRIPTION:
C     If it is not yet defined within the MNC CW layer, append a
C     variable type.  Calls MNC\_CW\_ADD\_VNAME().

C     !USES:
      implicit none
#include "MNC_COMMON.h"

C     !INPUT PARAMETERS:
      integer myThid, bi_dim, bj_dim
      character*(*) vname, gname
CEOP

C     !LOCAL VARIABLES:
      integer indv

C     Check whether vname is defined
      CALL MNC_GET_IND(MNC_MAX_ID, vname, mnc_cw_vname, indv, myThid)
      IF (indv .LT. 1) THEN
        CALL MNC_CW_ADD_VNAME(vname, gname, bi_dim, bj_dim, myThid)
      ENDIF


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_ADD_VNAME

C     !INTERFACE:
      SUBROUTINE MNC_CW_ADD_VNAME(
     I     vname,
     I     gname,
     I     bi_dim, bj_dim,
     I     myThid )

C     !DESCRIPTION:
C     Add a variable type to the MNC CW layer.  The variable type is an
C     association between a variable type name and the following items:
C     \begin{center}
C       \begin{tabular}[h]{|ll|}\hline
C         \textbf{Item}  & \textbf{Purpose}  \\\hline
C         grid type  &  defines the in-memory arrangement  \\
C         \texttt{bi,bj} dimensions  &  tiling indices, if present  \\\hline
C       \end{tabular}
C     \end{center}

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid, bi_dim, bj_dim
      character*(*) vname, gname
CEOP

C     !LOCAL VARIABLES:
      integer i, nvf,nvl, ngf,ngl, indv,indg
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer IFNBLNK, ILNBLNK

      nvf = IFNBLNK(vname)
      nvl = ILNBLNK(vname)
      ngf = IFNBLNK(gname)
      ngl = ILNBLNK(gname)

C     Check that this vname is not already defined
      CALL MNC_GET_IND(MNC_MAX_ID, vname, mnc_cw_vname, indv, myThid)
      IF (indv .GT. 0) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_VNAME ERROR: ''',
     &       vname(nvf:nvl), ''' is already defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_VNAME'
      ENDIF
      CALL MNC_GET_NEXT_EMPTY_IND(MNC_MAX_ID, mnc_cw_vname,
     &     'mnc_cw_vname', indv, myThid)

C     Check that gname exists
      CALL MNC_GET_IND(MNC_MAX_ID, gname, mnc_cw_gname, indg, myThid)
      IF (indg .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_VNAME ERROR: ''',
     &       gname(ngf:ngl), ''' is not defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_VNAME'
      ENDIF

      mnc_cw_vname(indv)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
      mnc_cw_vname(indv)(1:(nvl-nvf+1)) = vname(nvf:nvl)
      mnc_cw_vgind(indv) = indg
      DO i = 1,3
        mnc_cw_vnat(i,indv) = 0
      ENDDO
      mnc_cw_vbij(1,indv) = bi_dim
      mnc_cw_vbij(2,indv) = bj_dim

#ifdef MNC_DEBUG_GTYPE
      CALL MNC_CW_ADD_VATTR_TEXT(vname,1,'mitgcm_grid',gname, myThid)
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_DEL_VNAME

C     !INTERFACE:
      SUBROUTINE MNC_CW_DEL_VNAME(
     I     vname,
     I     myThid )

C     !DESCRIPTION:
C     Delete a variable type from the MNC CW layer.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) vname
CEOP

C     !LOCAL VARIABLES:
      integer i, indv

C     Check that this vname is not already defined
      CALL MNC_GET_IND(MNC_MAX_ID, vname, mnc_cw_vname, indv, myThid)
      IF (indv .LT. 1) THEN
        RETURN
      ENDIF

      mnc_cw_vname(indv)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
      mnc_cw_vgind(indv) = 0
      DO i = 1,3
        mnc_cw_vnat(i,indv) = 0
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MNC_CW_ADD_VATTR_TEXT
C     !INTERFACE:
      SUBROUTINE MNC_CW_ADD_VATTR_TEXT(
     I     vname, tname, tval,
     I     myThid )

C     !DESCRIPTION:
C     Add a text attribute

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) vname, tname, tval
      integer ival
      REAL*8 dval
CEOP
      ival = 0
      dval = 0.0D0
      CALL MNC_CW_ADD_VATTR_ANY(vname, 1,
     &     tname, ' ', ' ', tval, ival, dval, myThid )
      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MNC_CW_ADD_VATTR_INT
C     !INTERFACE:
      SUBROUTINE MNC_CW_ADD_VATTR_INT(
     I     vname, iname, ival,
     I     myThid )

C     !DESCRIPTION:
C     Add integer attribute

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) vname, iname
      integer ival
      REAL*8 dval
CEOP
      dval = 0.0D0
      CALL MNC_CW_ADD_VATTR_ANY(vname, 2,
     &     ' ', iname, ' ', ' ', ival, dval, myThid )
      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MNC_CW_ADD_VATTR_DBL
C !INTERFACE:
      SUBROUTINE MNC_CW_ADD_VATTR_DBL(
     I     vname, dname, dval,
     I     myThid )

C     !DESCRIPTION:
C     Add double-precision real attribute

C     !USES:
      implicit none

C     !INPUT PARAMETERS:
      integer myThid
      character*(*) vname, dname
      integer ival
      REAL*8 dval
CEOP
      ival = 0
      CALL MNC_CW_ADD_VATTR_ANY(vname, 3,
     &     ' ', ' ', dname, ' ', ival, dval, myThid )
      RETURN
      END
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_ADD_VATTR_ANY

C     !INTERFACE:
      SUBROUTINE MNC_CW_ADD_VATTR_ANY(
     I     vname,
     I     atype,
     I     tname, iname, dname,
     I     tval,  ival,  dval,
     I     myThid )

C     !DESCRIPTION:

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
      integer atype
      character*(*) vname
      character*(*) tname, iname, dname
      character*(*) tval
      integer ival
      REAL*8 dval
CEOP

C     !LOCAL VARIABLES:
      integer n, nvf,nvl, n1,n2, indv, ic
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer IFNBLNK, ILNBLNK

      nvf = IFNBLNK(vname)
      nvl = ILNBLNK(vname)

C     Check that vname is defined
      CALL MNC_GET_IND(MNC_MAX_ID, vname, mnc_cw_vname, indv, myThid)
      IF (indv .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_VATTR_ANY ERROR: ''',
     &       vname(nvf:nvl), ''' is not defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_VATTR_ANY'
      ENDIF

      IF (atype .EQ. 1) THEN
C       Text Attribute
        n = mnc_cw_vnat(1,indv) + 1
        n1 = IFNBLNK(tname)
        n2 = ILNBLNK(tname)
        IF ((n2-n1+1) .GT. MNC_MAX_CHAR) THEN
          write(msgbuf,'(3a,i6,2a)')
     &         'MNC_CW_ADD_VATTR_ANY WARNING: attribute name ''',
     &         tname(n1:n2), ''' has more than ', MNC_MAX_CHAR,
     &         ' characters and has been truncated to fit--please',
     &         'use a smaller name or increase MNC_MAX_CHAR'
          CALL PRINT_MESSAGE( msgbuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
C         MNC_MAX_CHAR = n2 - n1 + 1
          n2 = MNC_MAX_CHAR + n1 - 1
        ENDIF
C       write(*,*) atype,tname(n1:n2)
        mnc_cw_vtnm(n,indv)(1:MNC_MAX_CHAR) =
     &       mnc_blank_name(1:MNC_MAX_CHAR)
        mnc_cw_vtnm(n,indv)(1:(n2-n1+1)) = tname(n1:n2)

        n1 = IFNBLNK(tval)
        n2 = ILNBLNK(tval)
        IF ((n2-n1+1) .GT. MNC_MAX_CATT) THEN
          write(msgbuf,'(3a,i6,2a)')
     &         'MNC_CW_ADD_VATTR_ANY WARNING: attribute value ''',
     &         tval(n1:n2), ''' has more than ', MNC_MAX_CATT,
     &         ' characters and has been truncated to fit--please',
     &         'use a smaller name or increase MNC_MAX_CATT'
          CALL PRINT_MESSAGE( msgbuf, errorMessageUnit,
     &                        SQUEEZE_RIGHT , myThid)
          n2 = MNC_MAX_CATT + n1 - 1
        ENDIF

        mnc_cw_vnat(1,indv) = n
        DO ic = 1,MNC_MAX_CATT
          mnc_cw_vtat(n,indv)(ic:ic) = ' '
        ENDDO
        IF ((n1 .NE. 0) .AND. (n2 .NE. 0)) THEN
          mnc_cw_vtat(n,indv)(1:(n2-n1+1)) = tval(n1:n2)
        ENDIF
      ENDIF

      IF (atype .EQ. 2) THEN
C       Integer Attribute
        n = mnc_cw_vnat(2,indv) + 1
        n1 = IFNBLNK(iname)
        n2 = ILNBLNK(iname)
C       write(*,*) atype,iname(n1:n2)
        mnc_cw_vinm(n,indv)(1:(n2-n1+1)) = iname(n1:n2)
        mnc_cw_viat(n,indv) = ival
        mnc_cw_vnat(2,indv) = n
      ENDIF

      IF (atype .EQ. 3) THEN
C       Double Attribute
        n = mnc_cw_vnat(3,indv) + 1
        n1 = IFNBLNK(dname)
        n2 = ILNBLNK(dname)
C       write(*,*) atype,dname(n1:n2)
        mnc_cw_vdnm(n,indv)(1:(n2-n1+1)) = dname(n1:n2)
        mnc_cw_vdat(n,indv) = dval
        mnc_cw_vnat(3,indv) = n
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_GET_TILE_NUM

C     !INTERFACE:
      SUBROUTINE MNC_CW_GET_TILE_NUM(
     I     bi, bj,
     O     uniq_tnum,
     I     myThid )

C     !DESCRIPTION:

C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif

C     !INPUT PARAMETERS:
      integer myThid, bi,bj, uniq_tnum
CEOP

C     !LOCAL VARIABLES:
      integer iG,jG

      iG = 0
      jG = 0

#ifdef ALLOW_EXCH2

      uniq_tnum = W2_myTileList(bi,bj)

#else

C     Global tile number for simple (non-cube) domains
      iG = bi+(myXGlobalLo-1)/sNx
      jG = bj+(myYGlobalLo-1)/sNy

      uniq_tnum = (jG - 1)*(nPx*nSx) + iG

#endif

CEH3      write(*,*) 'iG,jG,uniq_tnum :', iG,jG,uniq_tnum

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_GET_FACE_NUM

C     !INTERFACE:
      SUBROUTINE MNC_CW_GET_FACE_NUM(
     I     bi, bj,
     O     uniq_fnum,
     I     myThid )

C     !DESCRIPTION:

C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif

C     !INPUT PARAMETERS:
      integer myThid, bi,bj, uniq_fnum
CEOP

#ifdef ALLOW_EXCH2

      uniq_fnum = exch2_myFace( W2_myTileList(bi,bj) )

#else

C     Global face number for simple (EXCH "1") domains
      uniq_fnum = -1

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_GET_XYFO

C     !INTERFACE:
      SUBROUTINE MNC_CW_GET_XYFO(
     I     bi, bj,
     O     ixoff, iyoff,
     I     myThid )

C     !DESCRIPTION:

C     !USES:
      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif

C     !INPUT PARAMETERS:
      integer myThid, bi,bj, ixoff,iyoff
CEOP

C     !LOCAL VARIABLES:
#ifdef ALLOW_EXCH2
      integer uniq_tnum
#endif

#ifdef ALLOW_EXCH2

      uniq_tnum = W2_myTileList(bi,bj)
      ixoff = exch2_tbasex( uniq_tnum )
      iyoff = exch2_tbasey( uniq_tnum )

#else

C     Global tile number for simple (non-cube) domains
C     iG = bi+(myXGlobalLo-1)/sNx
C     jG = bj+(myYGlobalLo-1)/sNy
C     uniq_tnum = (jG - 1)*(nPx*nSx) + iG
      ixoff = myXGlobalLo + bi * sNx
      iyoff = myYGlobalLo + bj * sNy

#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_CW_FILE_AORC

C     !INTERFACE:
      SUBROUTINE MNC_CW_FILE_AORC(
     I     fname,
     O     indf,
     I     lbi, lbj, uniq_tnum,
     I     myThid )

C     !DESCRIPTION:
C     Open a NetCDF file, appending to the file if it already exists
C     and, if not, creating a new file.

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"
#include "netcdf.inc"

C     !INPUT PARAMETERS:
      integer myThid, indf, lbi, lbj, uniq_tnum
      character*(*) fname
CEOP

C     !LOCAL VARIABLES:
      integer ierr

C     Check if the file is already open
      CALL MNC_GET_IND(MNC_MAX_FID, fname, mnc_f_names, indf, myThid)
      IF (indf .GT. 0) THEN
        RETURN
      ENDIF

C     Try to open an existing file
      CALL MNC_FILE_TRY_READ(fname, ierr, indf, myThid)
      IF (ierr .NE. NF_NOERR) THEN
C       Try to create a new one
        CALL MNC_FILE_OPEN(fname, 0, indf, myThid)
      ENDIF

C     Add the global attributes
      CALL MNC_CW_SET_GATTR(fname, lbi,lbj, uniq_tnum, myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
