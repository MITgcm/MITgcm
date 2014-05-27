C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_readparms.F,v 1.12 2014/05/27 23:41:27 jmc Exp $
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
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_ICEFRONT

C !LOCAL VARIABLES: ====================================================
C  iUnit                :: unit number for I/O
C  msgBuf               :: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /ICEFRONT_PARM01/
     &     rhoIcefront, ICEFRONTkappa,
     &     ICEFRONTlatentHeat, ICEFRONTHeatCapacity_Cp,
     &     ICEFRONTthetaSurface,
     &     applyIcefrontTendT,applyIcefrontTendS,
     &     ICEFRONTdepthFile, ICEFRONTlengthFile

      IF ( .NOT.useICEFRONT ) THEN
C-    pkg ICEFRONT is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
         ICEFRONTisON = .FALSE.
C     print a (weak) warning if data.icefront is found
         CALL PACKAGES_UNUSED_MSG( 'useICEFRONT', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      ICEFRONTisON = .TRUE.

C Set defaults values for parameters in ICEFRONT.h
#ifdef ALLOW_SHELFICE
      IF (useShelfice) THEN
C Use the same values as in SHELFICE
       ICEFRONTlatentHeat       = SHELFICElatentHeat
       ICEFRONTHeatCapacity_Cp  = SHELFICEHeatCapacity_Cp
       rhoIcefront              = rhoShelfice
       ICEFRONTkappa            = SHELFICEkappa
       ICEFRONTthetaSurface     = SHELFICEthetaSurface
      ELSE
#else /* ifndef ALLOW_SHELFICE */
      IF (.TRUE.) THEN
#endif /* ALLOW_SHELFICE */
C Default values when SHELFICE is not used
       ICEFRONTlatentHeat       =  334.0 _d 3
       ICEFRONTHeatCapacity_Cp  = 2000.0 _d 0
       rhoIcefront              =  917.0 _d 0
       ICEFRONTkappa            =   1.54 _d -06
       ICEFRONTthetaSurface     = - 20.0 _d 0
      ENDIF

      recip_ICEFRONTlatentHeat =    0.0 _d 0
      ICEFRONTlengthFile       = ' '
      ICEFRONTdepthFile        = ' '
      applyIcefrontTendS       = .TRUE.
      applyIcefrontTendT       = .TRUE.
      SGrunoffFile             = ' '

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
      _END_MASTER(myThid)
C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
