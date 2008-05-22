C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_missingvals.F,v 1.2 2008/05/22 12:21:19 mlosch Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_CW_VATTR_MISSING

C     !INTERFACE:
      SUBROUTINE MNC_CW_VATTR_MISSING(
     I     vname,
     I     iflag,
     I     mv_r8,  mv_r4,  mv_int,
     I     myThid )

C     !DESCRIPTION:
C     Set missing values to be associated with a CW variable name.
      
C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid, iflag
      character*(*) vname
      REAL*8  mv_r8(2)
      REAL*4  mv_r4(2)
      INTEGER mv_int(2)
CEOP

C     !LOCAL VARIABLES:
      integer nvf,nvl, indv, ii
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer IFNBLNK, ILNBLNK

      nvf = IFNBLNK(vname)
      nvl = ILNBLNK(vname)

C     Check that vname is defined
      CALL MNC_GET_IND(MNC_MAX_ID, vname, mnc_cw_vname, indv, myThid)
      IF (indv .LT. 1) THEN
        write(msgbuf,'(3a)') 'MNC_CW_VATTR_MISSING ERROR: ''',
     &       vname(nvf:nvl), ''' is not defined'
        CALL print_error(msgbuf, mythid)
        stop 'ABNORMAL END: S/R MNC_CW_VATTR_MISSING'
      ENDIF

      mnc_cw_vfmv(indv) = iflag
      DO ii = 1,2
        mnc_cw_vmvd(ii,indv) = mv_r8(ii)
        mnc_cw_vmvr(ii,indv) = mv_r4(ii)
        mnc_cw_vmvi(ii,indv) = mv_int(ii)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
