C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_udim.F,v 1.11 2008/05/22 12:21:19 mlosch Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: MNC_CW_SET_UDIM

C !INTERFACE:
      SUBROUTINE MNC_CW_SET_UDIM( 
     I     fgname, 
     I     nudim, 
     I     myThid ) 

C     !DESCRIPTION:
C     For a specified file group name, set the size of the NetCDF
C     unlimited (or record) dimension.  The options are:
C     \begin{equation}
C       \label{eq:yo}
C       \mbox{\bf nudim} = \left\{
C         \begin{array}[htb]{cl}
C           >0,  &  \mbox{\small use the specified value} \\
C           0,   &  \mbox{\small use the largest currently defined value} \\
C           -1,  &  \mbox{\small increment the largest value and then use it} \\
C         \end{array}
C         \right.
C     \end{equation}

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer nudim, myThid
      character*(*) fgname
CEOP

C     !LOCAL VARIABLES:
      integer fgf,fgl, indfg
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer IFNBLNK, ILNBLNK

C     Check that this name is not already defined
      fgf = IFNBLNK(fgname)
      fgl = ILNBLNK(fgname)
      CALL MNC_GET_IND(MNC_MAX_ID, fgname, mnc_cw_fgnm, indfg, myThid)
      IF (indfg .LT. 1) THEN
        CALL MNC_GET_NEXT_EMPTY_IND(
     &       MNC_MAX_ID, mnc_cw_fgnm, 'mnc_cw_fgnm', indfg, myThid)
        mnc_cw_fgnm(indfg)(1:(fgl-fgf+1)) = fgname(fgf:fgl)
        mnc_cw_fgud(indfg) = 0
        mnc_cw_fgig(indfg) = 0
      ENDIF
      IF (nudim .GT. 0) THEN
        mnc_cw_fgig(indfg) = 0
        mnc_cw_fgud(indfg) = nudim
        RETURN
      ENDIF
      IF (nudim .EQ. 0) THEN
        mnc_cw_fgig(indfg) = 0
        RETURN
      ENDIF
      IF (nudim .EQ. -1) THEN
        mnc_cw_fgig(indfg) = 1
        mnc_cw_fgud(indfg) = mnc_cw_fgud(indfg) + 1
        RETURN
      ENDIF
      write(msgbuf,'(3a,i10,a)') 
     &     'MNC_CW_SET_UDIM ERROR: for file group name ''', 
     &     fgname(fgf:fgl), ''' the unlim dim ''', nudim, 
     &     ''' is not allowed'
      CALL print_error(msgbuf, mythid)
      STOP 'ABNORMAL END: S/R MNC_CW_SET_UDIM'

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
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
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer nudim, myThid
      character*(*) fgname
CEOP

C     !LOCAL VARIABLES:
      integer fgf,fgl, indfg
      character*(MAX_LEN_MBUF) msgbuf

C     Functions
      integer IFNBLNK, ILNBLNK

      fgf = IFNBLNK(fgname)
      fgl = ILNBLNK(fgname)
      CALL MNC_GET_IND(MNC_MAX_ID, fgname, mnc_cw_fgnm, indfg, myThid)
      IF (indfg .LT. 1) THEN
        write(msgbuf,'(3a)') 
     &       'MNC_CW_GET_UDIM ERROR: file group name ''', 
     &       fgname(fgf:fgl), ''' is not defined'
        CALL print_error(msgbuf, mythid)
        STOP 'ABNORMAL END: S/R MNC_CW_GET_UDIM'
      ENDIF
      nudim = mnc_cw_fgud(indfg)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
