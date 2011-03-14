C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_read_co2_pickup.F,v 1.2 2011/03/14 01:38:04 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

      SUBROUTINE DIC_READ_CO2_PICKUP(
     I                                myIter, myThid )

      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"
#include "DIC_ATMOS.h"

C     == Routine arguments ==
C     myThid  :: my Thread Id number
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC

#ifdef USE_ATMOSCO2
C if coupled to atmsopheric model, use the
C CO2 value passed from the coupler

#else /* USE_ATMOSCO2 */

C     !FUNCTIONS
      INTEGER  MDS_RECLEN
      EXTERNAL MDS_RECLEN

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER fp, ioUnit
      CHARACTER*(MAX_LEN_FNAM) fn, filNam
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL useCurrentDir, fileExist
      REAL*8 tmpFld(2)
      INTEGER length_of_rec
CEOP

      IF ( dic_int1.EQ.3 ) THEN

        IF (pickupSuff.EQ.' ') THEN
          WRITE(fn,'(A,I10.10)') 'pickup_dic_co2atm.', myIter
        ELSE
          WRITE(fn,'(A,A10)') 'pickup_dic_co2atm.', pickupSuff
        ENDIF
        fp = precFloat64

C--   First check if pickup file exist
#ifdef ALLOW_MDSIO
        useCurrentDir = .FALSE.
        CALL MDS_CHECK4FILE(
     I                       fn, '.data', 'DIC_READ_CO2_PICKUP',
     O                       filNam, fileExist,
     I                       useCurrentDir, myThid )
#else
        STOP 'ABNORMAL END: S/R DIC_READ_CO2_PICKUP: Needs MDSIO pkg'
#endif
        IF ( .NOT.fileExist ) THEN
          WRITE(msgBuf,'(4A)') 'DIC_READ_CO2_PICKUP: ',
     &      'with dic_int1=3, needs CO2-atmos pickup to restart'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R DIC_READ_CO2_PICKUP'
        ENDIF
        _BEGIN_MASTER(myThid)
        CALL MDSFINDUNIT( ioUnit, myThid )
        length_of_rec = MDS_RECLEN( fp, 2, myThid )
        OPEN( ioUnit, file=filNam, status='old',
     &        access='direct', recl=length_of_rec )
        READ(ioUnit,rec=1) tmpFld
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8( 2, tmpFld )
#endif
        total_atmos_carbon = tmpFld(1)
        atpco2 = tmpFld(2)

       _END_MASTER(myThid)
c      _BARRIER
      ENDIF

#endif /* ndef USE_ATMOSCO2 */

#endif /*  ALLOW_DIC  */

      RETURN
      END
