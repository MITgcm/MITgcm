C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_write_pickup.F,v 1.3 2005/02/20 05:28:20 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIAGNOSTICS_WRITE_PICKUP 
C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_WRITE_PICKUP(
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
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: current time
C     myIter  :: time-step number
C     myThid  :: Number of this instance
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     lgf     :: flag to write "global" files
c     INTEGER prec, iChar, lChar, k
      INTEGER prec, lChar, i
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf

      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

#ifdef ALLOW_MNC
      INTEGER ii
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
          WRITE(diag_mnc_bn,'(A)') 'pickup_diagnostics'

C         Update the record dimension by writing the iteration number
          CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
          CALL MNC_CW_I_W_S('I',diag_mnc_bn,0,0,'T',myIter,myThid)
          CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)
          
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
          dim(3)     = numdiags
          ib(3)      = 1
          ie(3)      = numdiags
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
          
        ENDIF
#endif
        
        DO i = 1,MAX_LEN_FNAM
          fn(i:i) = ' '
        ENDDO
        lChar = ILNBLNK(suff)
        WRITE(fn,'(A,A)') 'pickup_diagnostics.',suff(1:lChar)
        prec = precFloat64
        lgf = globalFiles
        
        IF (diag_pickup_write_mdsio) THEN
          CALL MDSWRITEFIELD(fn,prec,lgf,'RL',numdiags,qdiag,
     &         1,myIter,myThid)
        ENDIF

      ENDIF

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
