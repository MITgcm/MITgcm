C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_read_pickup.F,v 1.6 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIAGNOSTICS_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_READ_PICKUP(
     I     myThid )

C     !DESCRIPTION:
C     Reads previously saved state for the diagnostics package.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid  :: Number of this instance
      INTEGER myThid

#ifdef DIAGNOSTICS_HAS_PICKUP

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
c     INTEGER prec, iChar, lChar, k
      INTEGER bi,bj
      INTEGER prec, sn
      CHARACTER*(MAX_LEN_FNAM) fn

      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      INTEGER dUnit, n, m

#ifdef ALLOW_MNC
      INTEGER i, ii
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn
      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) d_cw_name
      CHARACTER*(NLEN) dn_blnk
#endif /*  ALLOW_MNC  */


C     Add pickup capability
      IF (diag_pickup_read) THEN

#ifdef ALLOW_MNC
        IF (diag_pickup_read_mnc) THEN
          DO i = 1,NLEN
            dn_blnk(i:i) = ' '
          ENDDO
          DO i = 1,MAX_LEN_FNAM
            diag_mnc_bn(i:i) = ' '
          ENDDO
          WRITE(diag_mnc_bn,'(A)') 'pickup_diagnostics'

C         Update the record dimension by writing the iteration number
          CALL MNC_FILE_CLOSE_ALL_MATCHING(diag_mnc_bn, myThid)
          CALL MNC_CW_SET_UDIM(diag_mnc_bn, 1, myThid)

C         Read the qdiag() array
          d_cw_name(1:NLEN) = dn_blnk(1:NLEN)
          DO ii = 1,CW_DIMS
            dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
          ENDDO
          d_cw_name(1:10) = 'diag_state'
          dn(1)(1:3) = 'Xp1'
          dim(1)     = sNx + 2*OLx
          ib(1)      = OLx + 1
          ie(1)      = OLx + sNx + 1
          dn(2)(1:3) = 'Yp1'
          dim(2)     = sNy + 2*OLy
          ib(2)      = OLy + 1
          ie(2)      = OLy + sNy + 1
          dn(3)(1:2) = 'Zd'
          dim(3)     = numDiags
          ib(3)      = 1
          ie(3)      = numDiags
          dn(4)(1:1) = 'T'
          dim(4)     = -1
          ib(4)      = 1
          ie(4)      = 1
          CALL MNC_CW_ADD_GNAME(d_cw_name, 4,
     &         dim, dn, ib, ie, myThid)
          CALL MNC_CW_ADD_VNAME(d_cw_name, d_cw_name,
     &         4,5, myThid)
          CALL MNC_CW_RL_R('D',diag_mnc_bn,0,0,
     &         d_cw_name, qdiag, myThid)
          CALL MNC_CW_DEL_VNAME(d_cw_name, myThid)
          CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)

C         Read the ndiag() array
          d_cw_name(1:NLEN) = dn_blnk(1:NLEN)
          DO ii = 1,CW_DIMS
            dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
          ENDDO
          d_cw_name(1:10) = 'diag_count'
          dn(1)(1:2) = 'Nd'
          dim(1)     = numDiags
          ib(1)      = 1
          ie(1)      = numDiags
          dn(2)(1:1) = 'T'
          dim(2)     = -1
          ib(2)      = 1
          ie(2)      = 1
          CALL MNC_CW_ADD_GNAME(d_cw_name, 2,
     &         dim, dn, ib, ie, myThid)
          CALL MNC_CW_ADD_VNAME(d_cw_name, d_cw_name,
     &         4,5, myThid)
          CALL MNC_CW_ADD_VATTR_TEXT(d_cw_name,'description',
     &         'diagnostics state',myThid)
C- jmc: get warnings when I compile this S/R because something is not right
C       in the type or one or more arguments. commented out for now
c         CALL MNC_CW_RL_R('I',diag_mnc_bn,0,0,
c    &         d_cw_name, ndiag, myThid)
          CALL MNC_CW_DEL_VNAME(d_cw_name, myThid)
          CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)

        ENDIF
#endif /* ALLOW_MNC */

        IF (diag_pickup_read_mdsio) THEN

C         Read qdiag()
          prec = precFloat64
          WRITE(fn,'(A,I10.10)') 'pickup_qdiag.', nIter0
          CALL READ_REC_3D_RL( fn, prec,
     &                         numDiags, qdiag, nIter0, myThid )

C         Read ndiag()
          _BARRIER
          _BEGIN_MASTER(myThid)

C--    jmc: should really write 1 file per tile
          WRITE(fn,'(A,I10.10)') 'pickup_ndiag.', nIter0
          CALL MDSFINDUNIT( dUnit, myThid )
          OPEN( dUnit, file=fn )
          DO n = 1,nlists
            DO m = 1,nfields(n)
              READ(dUnit,'(I10)') ndiag(jdiag(m,n),1,1)
            ENDDO
          ENDDO
          CLOSE( dUnit )
C-        Need to fill-in ndiag for other tiles
          DO bj=1,nSy
           DO bi=1,nSx
            DO n=1,ndiagt
             ndiag(n,bi,bj) = ndiag(n,1,1)
            ENDDO
           ENDDO
          ENDDO
          _END_MASTER(myThid)
          _BARRIER
        ENDIF

      ENDIF

#endif /* DIAGNOSTICS_HAS_PICKUP */

      RETURN
      END
