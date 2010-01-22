C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_readparms.F,v 1.2 2010/01/22 00:51:54 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

CBOP
C !ROUTINE: ICEFRONT_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE ICEFRONT_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize ICEFRONT parameters, read in data.icefront

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "ICEFRONT.h"
#include "PARAMS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_ICEFRONT

C !LOCAL VARIABLES: ====================================================
C  iUnit		:: unit number for I/O
C  msgBuf		:: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /ICEFRONT_PARM01/
     &     ICEFRONTheatTransCoeff,
     &     ICEFRONTsaltTransCoeff,
     &     rhoIcefront, ICEFRONTkappa,
     &     ICEFRONTlatentHeat, ICEFRONTHeatCapacity_Cp,
     &     ICEFRONTthetaSurface,
     &     useISOMIPTD,
     &     ICEFRONTconserve,
     &     ICEFRONTtopoFile

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      ICEFRONTisON=.TRUE.

C Set defaults values for parameters in ICEFRONT.h
      useISOMIPTD              = .FALSE.
      ICEFRONTconserve         = .FALSE.
      ICEFRONTtopoFile         = ' '
      ICEFRONTlatentHeat       =  334.0 _d 3
      ICEFRONTHeatCapacity_Cp  = 2000.0 _d 0
      recip_ICEFRONTlatentHeat =    0.0 _d 0
      rhoIcefront              =  917.0 _d 0
      ICEFRONTheatTransCoeff   =    1.0 _d -04
      ICEFRONTsaltTransCoeff   = UNSET_RL
      ICEFRONTkappa            =   1.54 _d -06
      ICEFRONTthetaSurface     = - 20.0 _d 0

C Open and read the data.icefront file
      WRITE(msgBuf,'(A)') ' ICEFRONT_READPARMS: opening data.icefront'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.icefront', 'ICEFRONT_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=ICEFRONT_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' ICEFRONT_READPARMS: finished reading data.icefront'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )

C Close the open data file
      CLOSE(iUnit)

C Now set-up any remaining parameters that result from the input parameters
      IF ( ICEFRONTlatentHeat .NE. 0. _d 0 )
     &     recip_ICEFRONTlatentHeat = 1. _d 0/ICEFRONTlatentHeat
      IF ( ICEFRONTsaltTransCoeff .EQ. UNSET_RL )
     &     ICEFRONTsaltTransCoeff =
     &     5.05 _d -3 *ICEFRONTheatTransCoeff

      _END_MASTER(myThid)
C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
