C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cwrapper.F,v 1.1 2004/01/27 05:47:32 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C       SUBROUTINE MNC_CW_W_RL( 
C      I     myThid, myIter, 
C      I     filebn, 
C      I     bi,bj, 
C      I     Gtype, 
C      I     Rtype, 
C      I     vname, 
C      I     var )

C       implicit none
C #include "netcdf.inc"
C #include "mnc_common.h"
C #include "EEPARAMS.h"

C C     Arguments
C       integer myThid, myIter, bi,bj
C       character*(*) filebn, Gtype
C       character*(2) Rtype
C       _RL var*(*)

      

C       RETURN
C       END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_ADD_NAME( 
     I     myThid, 
     I     name, 
     I     ndim, 
     I     dlens, 
     I     dnames, 
     I     inds_beg, inds_end ) 

      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     Functions
      integer IFNBLNK, ILNBLNK

C     Arguments
      integer myThid, ndim
      character*(*) name
      integer dlens(*), inds_beg(*), inds_end(*)
      character*(*) dnames(*)

C     Local Variables
      integer i, nnf,nnl, indg
      character*(MAX_LEN_MBUF) msgbuf

      nnf = IFNBLNK(name)
      nnl = ILNBLNK(name)

C     Check that this name is not already defined
      CALL MNC_GET_IND(myThid, MNC_MAX_ID, name, mnc_cw_names, indg)
      IF (indg .GT. 0) THEN
        write(msgbuf,'(3a)') 'MNC_CW_ADD_NAME ERROR: ''', name, 
     &       ''' is already defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_ADD_NAME'
      ENDIF
      CALL MNC_GET_NEXT_EMPTY_IND(myThid, MNC_MAX_ID, mnc_cw_names, 
     &     indg)

      mnc_cw_names(indg)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
      mnc_cw_names(indg)(1:(nnl-nnf+1)) = name(nnf:nnl)
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
      integer i, ihorz,ihsub,ivert,itime,ihalo, is, n,ntot
      integer ndim
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
      data vert_dat /
     &     '-    ', 'C    ', 'I    ', '     ', '     '  /
      data time_dat /
     &     '-    ', 't    ', '     ', '     ', '     '  /
      data halo_dat /
     &     'Hn   ', 'Hy   ', '     ', '     ', '     '  /


      DO ihorz = 1,5
        DO is = 1,3

C         Loop just ONCE if the Horiz component is "-"
          ihsub = is
          IF (ihorz .EQ. 1) THEN
            IF (is .EQ. 1) THEN
              ihsub = 4
            ELSE
              GOTO 10
            ENDIF
          ENDIF

          DO ivert = 1,3
            DO itime = 1,2
              DO ihalo = 1,2

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
                write(name((ntot+1):(ntot+7)), '(5a1,a2)') '_', 
     &               vert_dat(ivert)(1:1), '_', 
     &               time_dat(itime)(1:1), '_', 
     &               halo_dat(ihalo)(1:2)

                ndim = 0
                DO i = 1,CW_MAX_LOC
                  dn(i)(1:MNC_MAX_CHAR)=mnc_blank_name(1:MNC_MAX_CHAR)
                  dim(i) = 0
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
                    ie(ndim)  = sNx + 2*OLx
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

                write(*,*) name(1:15), ndim, ' : ', (dim(i), i=1,5)

                IF (ndim .GT. 0) THEN
                  CALL MNC_CW_ADD_NAME(myThid, name, ndim, 
     &                 dim, dn, ib, ie)
                ENDIF

              ENDDO
            ENDDO
          ENDDO

 10       CONTINUE
        ENDDO
      ENDDO
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

