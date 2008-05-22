C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_citer.F,v 1.3 2008/05/22 12:21:19 mlosch Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: MNC_CW_CITER_SETG
      
C     !INTERFACE:
      SUBROUTINE MNC_CW_CITER_SETG( 
     I     igroup,
     I     iflag, ival_curr, ival_next,
     I     myThid ) 

C     !DESCRIPTION:
C     Set CITER information for group "igroup"

C     !USES:
      implicit none
#include "MNC_COMMON.h"

C     !INPUT PARAMETERS:
      integer igroup, iflag, ival_curr, ival_next, myThid
CEOP

C     !LOCAL VARIABLES:
      integer i

      mnc_cw_cit(1,igroup) = iflag
      IF ( ival_curr .GT. 0 ) THEN

        IF ( mnc_cw_cit(2,igroup) .NE. ival_curr ) THEN

C         The current iteration number has changed so we need to reset
C         the unlimited dimension for all the files in this citer group
          DO i = 1,MNC_MAX_ID
            IF ( mnc_cw_fgci(i) .eq. igroup ) THEN
              mnc_cw_fgud(i) = 0
            ENDIF
          ENDDO

          mnc_cw_cit(2,igroup) = ival_curr

        ENDIF
        
      ENDIF
      IF ( ival_next .GT. 0 ) THEN
        mnc_cw_cit(3,igroup) = ival_next
      ENDIF

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: MNC_CW_CITER_GETG
      
C     !INTERFACE:
      SUBROUTINE MNC_CW_CITER_GETG( 
     I     igroup,
     O     iflag, ival_curr, ival_next,
     I     myThid ) 

C     !DESCRIPTION:
C     Get the current CITER information for group "igroup"

C     !USES:
      implicit none
#include "MNC_COMMON.h"

C     !INPUT PARAMETERS:
      integer igroup, iflag, ival_curr, ival_next, myThid
CEOP

      iflag     = mnc_cw_cit(1,igroup) 
      ival_curr = mnc_cw_cit(2,igroup)
      ival_next = mnc_cw_cit(3,igroup)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: MNC_CW_SET_CITER
      
C     !INTERFACE:
      SUBROUTINE MNC_CW_SET_CITER( 
     I     fgname, 
     I     igroup, 
     I     iflag, ival_curr, ival_next,
     I     myThid ) 

C     !DESCRIPTION:
C     Set the flag and/or current iteration value

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer igroup, iflag, ival_curr, ival_next, myThid
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
C       Error if this file group name is not set
        write(msgbuf,'(3a)') 
     &       'MNC_CW_SET_CITER ERROR: the file group name ''', 
     &       fgname(fgf:fgl), ''' does not exist'
        CALL print_error(msgbuf, mythid)
        STOP 'ABNORMAL END: S/R MNC_CW_SET_CITER'
      ENDIF

      IF (igroup .LT. 1) THEN
        igroup = mnc_cw_fgci(indfg)
      ELSE
        mnc_cw_fgci(indfg) = igroup
      ENDIF
      IF ((igroup .LT. 1) .OR. (igroup .GT. MNC_MAX_INFO)) THEN
        write(msgbuf,'(4a)') 
     &       'MNC_CW_SET_CITER ERROR: invalid igroup index for ', 
     &       'file group name ''', fgname(fgf:fgl), ''''
        CALL print_error(msgbuf, mythid)
        STOP 'ABNORMAL END: S/R MNC_CW_SET_CITER'
      ENDIF

      CALL MNC_CW_CITER_SETG( igroup, 
     &     iflag, ival_curr, ival_next, myThid )

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: MNC_CW_GET_CITER
      
C     !INTERFACE:
      SUBROUTINE MNC_CW_GET_CITER( 
     I     fgname, 
     O     igroup, 
     O     iflag, ival_curr, ival_next,
     I     myThid ) 

C     !DESCRIPTION:
C     Set the flag and/or current iteration value

C     !USES:
      implicit none
#include "MNC_COMMON.h"
#include "EEPARAMS.h"

C     !INPUT PARAMETERS:
      integer igroup, iflag, ival_curr, ival_next, myThid
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
C       Error if this file group name is not set
        write(msgbuf,'(3a)') 
     &       'MNC_CW_SET_CITER ERROR: the file group name ''', 
     &       fgname(fgf:fgl), ''' does not exist'
        CALL print_error(msgbuf, mythid)
        STOP 'ABNORMAL END: S/R MNC_CW_SET_CITER'
      ENDIF

      igroup = mnc_cw_fgci(indfg)
      IF ((igroup .LT. 1) .OR. (igroup .GT. MNC_MAX_INFO)) THEN
        igroup    = -1
        iflag     = -1
        ival_curr = -1
        ival_next = -1
      ELSE
        CALL MNC_CW_CITER_GETG( igroup, 
     &       iflag, ival_curr, ival_next, myThid )
      ENDIF


      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

