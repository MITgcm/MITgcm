C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write_pickup.F,v 1.10 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIAGNOSTICS_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_WRITE_PICKUP(
     I     isPerm,
     I     suff,
     I     myTime,
     I     myIter,
     I     myThid )

C     !DESCRIPTION:
C     Writes current state of the diagnostics package.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     isPerm  :: permanent checkpoint flag
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      LOGICAL isPerm
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef DIAGNOSTICS_HAS_PICKUP

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
c     INTEGER prec, iChar, lChar, k
      INTEGER prec, lChar, sn
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

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (diag_pickup_write) THEN

#ifdef ALLOW_MNC
        IF (diag_pickup_write_mnc) THEN
          DO i = 1,NLEN
            dn_blnk(i:i) = ' '
          ENDDO
          DO i = 1,MAX_LEN_FNAM
            diag_mnc_bn(i:i) = ' '
          ENDDO

          IF ( isPerm ) THEN
            WRITE(diag_mnc_bn,'(A)') 'pickup_diagnostics'
          ELSE
            ii = ILNBLNK(suff)
            WRITE(diag_mnc_bn,'(A,A)')
     &           'pickup_diagnostics.',suff(1:ii)
          ENDIF

          CALL MNC_CW_SET_UDIM(fn, 0, myThid)
          IF ( isPerm ) THEN
            CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
          ELSE
            CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
          ENDIF
C         Then set the actual unlimited dimension
          CALL MNC_CW_SET_UDIM(fn, 1, myThid)

C         Update the record dimension by writing the iteration number
          CALL MNC_CW_RL_W_S('D',diag_mnc_bn,0,0,'T',myTime,myThid)
          CALL MNC_CW_I_W_S('I',diag_mnc_bn,0,0,'iter',myIter,myThid)

C         Write the qdiag() array
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
          dn(3)(1:2) = 'Nd'
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
          CALL MNC_CW_ADD_VATTR_TEXT(d_cw_name,'description',
     &         'diagnostics state',myThid)

          CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &         d_cw_name, qdiag, myThid)

          CALL MNC_CW_DEL_VNAME(d_cw_name, myThid)
          CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)

C         Write the ndiag() array
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

          CALL MNC_CW_I_W('I',diag_mnc_bn,0,0,
     &         d_cw_name, ndiag, myThid)

          CALL MNC_CW_DEL_VNAME(d_cw_name, myThid)
          CALL MNC_CW_DEL_GNAME(d_cw_name, myThid)


        ENDIF
#endif

        IF (diag_pickup_write_mdsio) THEN

          sn = ILNBLNK(suff)

C         Write qdiag()
          WRITE(fn,'(a,a)') 'pickup_qdiag.', suff(1:sn)
          prec = precFloat64
          CALL WRITE_REC_3D_RL( fn, prec, numDiags, qdiag,
     &                          1, myIter, myThid )

C         Write ndiag()
          _BARRIER
          _BEGIN_MASTER( myThid )
          WRITE(fn,'(a,a)') 'pickup_ndiag.', suff(1:sn)
          CALL MDSFINDUNIT( dUnit, mythid )
          OPEN( dUnit, file=fn )
          DO n = 1,nlists
            DO m = 1,nfields(n)
              WRITE(dUnit,'(I10)') ndiag(jdiag(m,n),1,1)
            ENDDO
          ENDDO
          CLOSE( dUnit )
          _END_MASTER( myThid )
          _BARRIER
        ENDIF

      ENDIF

#endif /* DIAGNOSTICS_HAS_PICKUP */

      RETURN
      END
