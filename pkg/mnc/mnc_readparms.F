C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_readparms.F,v 1.1 2004/03/19 15:09:03 edhill Exp $
C $Name:  $

#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_READPARMS( myThid )

      implicit none

#include "SIZE.h"
#include "mnc_common.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     Arguments
      integer myThid

C     Functions
      integer ILNBLNK

C     Local Variables
      integer iUnit
      character*(MAX_LEN_MBUF) msgBuf
      NAMELIST /MNC_PARM01/
     &     mnc_echo_gtypes,
     &     mnc_pickup_create,
     &     mnc_pickup_read


      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(a)') ' MNC_READPARMS: opening data.mnc'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)
    

      CALL OPEN_COPY_DATA_FILE(
     I     'data.mnc', 'MNC_READPARMS',
     O     iUnit,
     I     myThid )
      
C     Default values MNC
      mnc_echo_gtypes    =  .TRUE.
      mnc_pickup_create  =  .FALSE.
      mnc_pickup_read    =  .FALSE.
      
      WRITE(msgBuf,'(a)') ' MNC_READPARMS: finished reading data.mnc'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C     Everyone else must wait for the parameters to be loaded
      _BARRIER

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
