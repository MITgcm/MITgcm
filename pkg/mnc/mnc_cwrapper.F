C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cwrapper.F,v 1.6 2004/02/05 05:42:07 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_GNAME( 
     I     myThid, 
     I     name, 
     I     ndim, 
     I     dlens, 
     I     dnames, 
     I     inds_beg, inds_end ) 

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, ndim
      character*(*) name
      integer dlens(*), inds_beg(*), inds_end(*)
      character*(*) dnames(*)

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer i, nnf,nnl, indg
      character*(MAX_LEN_MBUF) msgbuf

      nnf = IFNBLNK(name)
      nnl = ILNBLNK(name)

C     Check that this name is not already defined
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, name, mnc_cw_gname, indg)
      IF (indg .GT. 0) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_GNAME ERROR: ''', name, 
     &       ''' is already defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_GNAME'
      ENDIF
      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_cw_gname, 
     &     indg)

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

      SUBROUTINE MNC_CW_DUMP()

      implicit none
#include "mnc_common.h"

C     Local Variables
      integer i,j, ntot

      write(*,'(a)') 'The currently defined Grid Types are:'
      ntot = 0
      DO j = 1,MNC_MAX_ID
        IF (mnc_cw_gname(j)(1:MNC_MAX_CHAR) 
     &       .NE. mnc_blank_name(1:MNC_MAX_CHAR)) THEN
          
          ntot = ntot + 1
          write(*,'(2i5,a3,a20,i3,a3,5i4,a4,5i4,a4,5i4,6a4)')
     &         j, ntot, ' : ', mnc_cw_gname(j)(1:20), mnc_cw_ndim(j), 
     &         ' : ', (mnc_cw_dims(i,j), i=1,5), 
     &         '  | ', (mnc_cw_is(i,j), i=1,5),
     &         '  | ', (mnc_cw_ie(i,j), i=1,5),
     &         '  | ', (mnc_cw_dn(i,j)(1:4), i=1,5)

        ENDIF
      ENDDO

      write(*,'(a)') 'The currently defined Variable Types are:'
      ntot = 0
      DO j = 1,MNC_MAX_ID
        IF (mnc_cw_vname(j)(1:MNC_MAX_CHAR) 
     &       .NE. mnc_blank_name(1:MNC_MAX_CHAR)) THEN

          ntot = ntot + 1
          write(*,'(2i5,a3,a25,a3,i4)') j, ntot, ' : ', 
     &         mnc_cw_vname(j)(1:20), ' : ', mnc_cw_vgind(j)

          DO i = 1,mnc_cw_vnat(1,j)
            write(*,'(a14,i4,a3,a25,a3,a25)') '      text_at:',i, 
     &           ' : ', mnc_cw_vtnm(i,j)(1:25), ' : ',
     &           mnc_cw_vtat(i,j)(1:25)
          ENDDO
          DO i = 1,mnc_cw_vnat(2,j)
            write(*,'(a14,i4,a3,a25,a3,i20)') '      int__at:',i, 
     &           ' : ', mnc_cw_vinm(i,j)(1:25), ' : ',
     &           mnc_cw_viat(i,j)
          ENDDO
          DO i = 1,mnc_cw_vnat(3,j)
            write(*,'(a14,i4,a3,a25,a3,f25.10)') '      dbl__at:',i, 
     &           ' : ', mnc_cw_vdnm(i,j)(1:25), ' : ',
     &           mnc_cw_vdat(i,j)
          ENDDO

        ENDIF
      ENDDO
      IF (ntot .EQ. 0) THEN
        write(*,'(a)') '   None defined!'
      ENDIF


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_INIT( 
     I     myThid,
     I     sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr ) 

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid
      integer sNx,sNy, OLx,OLy, nSx,nSy, nPx,nPy, Nr

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer CW_MAX_LOC
      parameter ( CW_MAX_LOC = 5 )
      integer i, ihorz,ihsub,ivert,itime,ihalo, is,ih, n,ntot
      integer ndim, ncomb
      character*(MAX_LEN_MBUF) msgbuf
      character*(MNC_MAX_CHAR) name
      character*(MNC_MAX_CHAR) dn(CW_MAX_LOC)
      character*(5) horz_dat(CW_MAX_LOC), hsub_dat(CW_MAX_LOC),
     &     vert_dat(CW_MAX_LOC), time_dat(CW_MAX_LOC), 
     &     halo_dat(CW_MAX_LOC)
      integer dim(CW_MAX_LOC), ib(CW_MAX_LOC), ie(CW_MAX_LOC)

C     ......12345....12345....12345....12345....12345...
      data horz_dat /
     &     '-    ', 'U    ', 'V    ', 'Cen  ', 'Cor  '  /
      data hsub_dat /
     &     'xy   ', 'x    ', 'y    ', '-    ', '     '  /
      data halo_dat /
     &     'Hn   ', 'Hy   ', '--   ', '     ', '     '  /
      data vert_dat /
     &     '-    ', 'C    ', 'I    ', '     ', '     '  /
      data time_dat /
     &     '-    ', 't    ', '     ', '     ', '     '  /

      ncomb = 0
      DO ihorz = 1,5
        DO is = 1,3
          DO ih = 1,2
            
C           Loop just ONCE if the Horiz component is "-"
            ihsub = is
            ihalo = ih
            IF (ihorz .EQ. 1) THEN
              IF ((is .EQ. 1) .AND. (ih .EQ. 1)) THEN
                ihsub = 4
                ihalo = 3
              ELSE
                GOTO 10
              ENDIF
            ENDIF
            
            DO ivert = 1,3
              DO itime = 1,2
                
C               horiz and hsub
                name(1:MNC_MAX_CHAR) = mnc_blank_name(1:MNC_MAX_CHAR)
                n = ILNBLNK(horz_dat(ihorz))
                name(1:n) = horz_dat(ihorz)(1:n)
                ntot = n + 1              
                name(ntot:ntot) = '_'
                n = ILNBLNK(hsub_dat(ihsub))
                name((ntot+1):(ntot+n)) = hsub_dat(ihsub)(1:n)
                ntot = ntot + n

C               vert, time, and halo
                write(name((ntot+1):(ntot+9)), '(a1,2a2,a1,a2,a1)') 
     &               '_', halo_dat(ihalo)(1:2), '__',
     &               vert_dat(ivert)(1:1), '__', 
     &               time_dat(itime)(1:1)

                ndim = 0
                DO i = 1,CW_MAX_LOC
                  dn(i)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
                  dim(i) = 0
                  ib(i) = 0
                  ie(i) = 0
                ENDDO

C               Horizontal dimensions
                IF (halo_dat(ihalo)(1:5) .EQ. 'Hn   ') THEN

                  IF (hsub_dat(ihsub)(1:1) .EQ. 'x') THEN
                    ndim = ndim + 1
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cen')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'V') ) THEN
                      dn(ndim)(1:1) = 'X'
                      dim(ndim) = sNx + 2*OLx
                      ib(ndim)  = OLx + 1
                      ie(ndim)  = OLx + sNx
                    ENDIF
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cor')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'U') ) THEN
                      dn(ndim)(1:3) = 'Xp1'
                      dim(ndim) = sNx + 2*OLx
                      ib(ndim)  = OLx + 1
                      ie(ndim)  = OLx + sNx + 1
                    ENDIF
                  ENDIF
                  IF ((hsub_dat(ihsub)(1:1) .EQ. 'y')
     &                 .OR. (hsub_dat(ihsub)(2:2) .EQ. 'y')) THEN
                    ndim = ndim + 1
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cen')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'U') ) THEN
                      dn(ndim)(1:1) = 'Y'
                      dim(ndim) = sNy + 2*OLy
                      ib(ndim)  = OLy + 1
                      ie(ndim)  = OLy + sNy
                    ENDIF
                    IF ( (horz_dat(ihorz)(1:3) .EQ. 'Cor')
     &                   .OR. (horz_dat(ihorz)(1:1) .EQ. 'V') ) THEN
                      dn(ndim)(1:3) = 'Yp1'
                      dim(ndim) = sNy + 2*OLy
                      ib(ndim)  = OLy + 1
                      ie(ndim)  = OLy + sNy + 1
                    ENDIF
                  ENDIF

                ELSEIF (halo_dat(ihalo)(1:5) .EQ. 'Hy   ') THEN

                  IF (hsub_dat(ihsub)(1:1) .EQ. 'x') THEN
                    ndim = ndim + 1
                    dn(ndim)(1:3) = 'Xwh'
                    dim(ndim) = sNx + 2*OLx
                    ib(ndim)  = 1
                    ie(ndim)  = sNx + 2*OLx
                  ENDIF
                  IF ((hsub_dat(ihsub)(1:1) .EQ. 'y')
     &                 .OR. (hsub_dat(ihsub)(2:2) .EQ. 'y')) THEN
                    ndim = ndim + 1
                    dn(ndim)(1:3) = 'Ywh'
                    dim(ndim) = sNy + 2*OLy
                    ib(ndim)  = 1
                    ie(ndim)  = sNy + 2*OLy
                  ENDIF

                ENDIF

C               Vertical dimension
                IF (vert_dat(ivert)(1:1) .EQ. 'C') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'Z'
                  dim(ndim) = Nr
                  ib(ndim)  = 1
                  ie(ndim)  = Nr
                ENDIF
                IF (vert_dat(ivert)(1:1) .EQ. 'I') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:3) = 'Zp1'
                  dim(ndim) = Nr + 1
                  ib(ndim)  = 1
                  ie(ndim)  = Nr + 1
                ENDIF

C               Time dimension
                IF (time_dat(itime)(1:1) .EQ. 't') THEN
                  ndim = ndim + 1
                  dn(ndim)(1:1) = 'T'
                  dim(ndim) = -1
                  ib(ndim)  = 1
                  ie(ndim)  = 1
                ENDIF

                IF (ndim .GT. 0) THEN
#ifdef MNC_DEBUG
                  ncomb = ncomb + 1
                  write(*,'(i4,a3,a15,i3,a3,5i4,a4,5i4,a4,5i4,6a4)')
     &                 ncomb, ' : ', name(1:15), ndim, 
     &                 ' : ', (dim(i), i=1,5), 
     &                 '  | ', (ib(i), i=1,5),
     &                 '  | ', (ie(i), i=1,5),
     &                 '  | ', (dn(i)(1:4), i=1,5)
#endif

                  CALL MNC_CW_ADD_GNAME(myThid, name, ndim, 
     &                 dim, dn, ib, ie)
                ENDIF

              ENDDO
            ENDDO

 10         CONTINUE
          ENDDO
        ENDDO
      ENDDO
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_VNAME( 
     I     myThid, 
     I     vname, 
     I     gname, 
     I     bi_dim, bj_dim ) 

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, bi_dim, bj_dim
      character*(*) vname, gname

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer i, nvf,nvl, ngf,ngl, indv,indg
      character*(MAX_LEN_MBUF) msgbuf

      nvf = IFNBLNK(vname)
      nvl = ILNBLNK(vname)
      ngf = IFNBLNK(gname)
      ngl = ILNBLNK(gname)

C     Check that this vname is not already defined
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, vname, mnc_cw_vname, indv)
      IF (indv .GT. 0) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_VNAME ERROR: ''', 
     &       vname(nvf:nvl), ''' is already defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_VNAME'
      ENDIF
      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_cw_vname, 
     &     indv)

C     Check that gname exists
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, gname, mnc_cw_gname, indg)
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

      CALL MNC_CW_ADD_VATTR_TEXT(myThid,vname,1,'mitgcm_grid',gname)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_VATTR_TEXT( 
     I     myThid, 
     I     vname, 
     I     ntat, 
     I     tnames, 
     I     tvals ) 

      implicit none

C     Arguments
      integer myThid, ntat
      character*(*) vname, tnames(*), tvals(*)

      CALL MNC_CW_ADD_VATTR_ANY(myThid, vname, 
     &     ntat, 0, 0, 
     &     tnames, ' ', ' ', 
     &     tvals, 0, 0.0D0 )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_VATTR_INT( 
     I     myThid, 
     I     vname, 
     I     niat, 
     I     inames, 
     I     ivals ) 

      implicit none

C     Arguments
      integer myThid, niat
      character*(*) vname, inames(*)
      integer ivals(*)

      CALL MNC_CW_ADD_VATTR_ANY(myThid, vname, 
     &     0, niat, 0, 
     &     ' ', inames, ' ', 
     &     ' ', ivals, 0.0D0 )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_VATTR_DBL( 
     I     myThid, 
     I     vname, 
     I     ndat, 
     I     dnames, 
     I     dvals ) 

      implicit none

C     Arguments
      integer myThid, ndat
      character*(*) vname, dnames(*)
      REAL*8 dvals(*)

      CALL MNC_CW_ADD_VATTR_ANY(myThid, vname, 
     &     0, 0, ndat, 
     &     ' ', ' ', dnames, 
     &     ' ', 0, dvals )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_VATTR_ANY( 
     I     myThid, 
     I     vname, 
     I     ntat,   niat,   ndat,
     I     tnames, inames, dnames,
     I     tvals,  ivals,  dvals ) 

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, ntat, niat, ndat
      character*(*) vname
      character*(*) tnames(*), inames(*), dnames(*)
      character*(*) tvals(*)
      integer ivals(*)
      REAL*8 dvals(*)

C     Functions
      integer IFNBLNK, ILNBLNK

C     Local Variables
      integer i, n, nvf,nvl, n1,n2, indv
      character*(MAX_LEN_MBUF) msgbuf

      nvf = IFNBLNK(vname)
      nvl = ILNBLNK(vname)

C     Check that vname is defined
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, vname, mnc_cw_vname, indv)
      IF (indv .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_VATTR_ANY ERROR: ''', 
     &       vname(nvf:nvl), ''' is not defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_VATTR_ANY'
      ENDIF

C     Text Attributes
      n = mnc_cw_vnat(1,indv)
      DO i = 1,ntat
        n1 = IFNBLNK(tnames(i))
        n2 = ILNBLNK(tnames(i))
        mnc_cw_vtnm(n+i,indv)(1:(n2-n1+1)) = tnames(i)(n1:n2)
        n1 = IFNBLNK(tvals(i))
        n2 = ILNBLNK(tvals(i))
        mnc_cw_vtat(n+i,indv)(1:(n2-n1+1)) = tvals(i)(n1:n2)
      ENDDO
      mnc_cw_vnat(1,indv) = n + ntat

C     Integer Attributes
      n = mnc_cw_vnat(2,indv)
      DO i = 1,niat
        n1 = IFNBLNK(inames(i))
        n2 = ILNBLNK(inames(i))
        mnc_cw_vinm(n+i,indv)(1:(n2-n1+1)) = inames(i)(n1:n2)
        mnc_cw_viat(n+i,indv) = ivals(i)
      ENDDO
      mnc_cw_vnat(2,indv) = n + niat

C     Double Attributes
      n = mnc_cw_vnat(3,indv)
      DO i = 1,ndat
        n1 = IFNBLNK(dnames(i))
        n2 = ILNBLNK(dnames(i))
        mnc_cw_vdnm(n+i,indv)(1:(n2-n1+1)) = dnames(i)(n1:n2)
        mnc_cw_vdat(n+i,indv) = dvals(i)
      ENDDO
      mnc_cw_vnat(3,indv) = n + ndat

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_GET_TILE_NUM( 
     I     myThid, 
     I     bi, bj, 
     O     uniq_tnum ) 

      implicit none
#include "EEPARAMS.h"
#include "SIZE.h"

C     Arguments
      integer myThid, bi,bj, uniq_tnum

C     Local Variables
      integer iG,jG

      iG = 0
      jG = 0

#ifdef ALLOW_EXCH2

#include "W2_EXCH2_PARAMS.h"
      uniq_tnum = W2_myTileList(bi)

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

      SUBROUTINE MNC_CW_FILE_AORC( 
     I     myThid, 
     I     fname, 
     O     indf ) 

      implicit none
#include "netcdf.inc"
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Arguments
      integer myThid, indf
      character*(*) fname

C     Local Variables
      integer i, ierr
      character*(MAX_LEN_MBUF) msgbuf

C     Check if the file is already open
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, fname, mnc_f_names, indf)
      IF (indf .GT. 0) THEN
        RETURN
      ENDIF

C     Try to open an existing file
      CALL MNC_FILE_TRY_READ(myThid, fname, ierr, indf)
      IF (ierr .EQ. NF_NOERR) THEN
        RETURN
      ENDIF

C     Try to create a new one
      CALL MNC_FILE_OPEN(myThid, fname, 0, indf)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

