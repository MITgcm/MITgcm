C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_udim.F,v 1.3 2004/03/29 03:33:51 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MNC_CW_SET_UDIM

C !INTERFACE:
      SUBROUTINE MNC_CW_SET_UDIM( 
     I     fgname, 
     I     nudim, 
     I     myThid ) 

C     !DESCRIPTION:
C     For a specified file group name, set the size of the NetCDF
C     unlimited (or record) dimension.
      
C     !USES:
      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer nudim, myThid
      character*(*) fgname

C     !LOCAL VARIABLES:
      integer fgf,fgl, indfg
      character*(MAX_LEN_MBUF) msgbuf
CEOP
C     Functions
      integer IFNBLNK, ILNBLNK

C     Check that this name is not already defined
      fgf = IFNBLNK(fgname)
      fgl = ILNBLNK(fgname)
      CALL MNC_GET_IND(MNC_MAX_ID, fgname, mnc_cw_fgnm, indfg, myThid)
      IF (indfg .LT. 1) THEN
        CALL MNC_GET_NEXT_EMPTY_IND(
     &       MNC_MAX_ID, mnc_cw_fgnm, indfg, myThid)
      ENDIF
      mnc_cw_fgnm(indfg)(1:(fgl-fgf+1)) = fgname(fgf:fgl)
      mnc_cw_fgud(indfg) = nudim

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: MNC_CW_GET_UDIM

C !INTERFACE:
      SUBROUTINE MNC_CW_GET_UDIM( 
     I     fgname, 
     O     nudim, 
     I     myThid ) 

C     !DESCRIPTION:
C     For a specified file group name, get the size of the NetCDF
C     unlimited (or record) dimension.

C     !USES:
      implicit none
#include "mnc_common.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer nudim, myThid
      character*(*) fgname

C     !LOCAL VARIABLES:
      integer fgf,fgl, indfg
      character*(MAX_LEN_MBUF) msgbuf
CEOP
C     Functions
      integer IFNBLNK, ILNBLNK

      fgf = IFNBLNK(fgname)
      fgl = ILNBLNK(fgname)
      CALL MNC_GET_IND(MNC_MAX_ID, fgname, mnc_cw_fgnm, indfg, myThid)
      IF (indfg .LT. 1) THEN
        write(msgbuf,'(3a)') 
     &       'MNC_CW_SET_UDIM ERROR: file group name ''', 
     &       fgname(fgf:fgl), ''' is not defined'
        CALL print_error(msgbuf, mythid)
        STOP 'ABNORMAL END: S/R MNC_CW_SET_UDIM'
      ENDIF
      nudim = mnc_cw_fgud(indfg)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

