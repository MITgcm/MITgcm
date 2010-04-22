C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_readparms.F,v 1.5 2010/04/22 18:24:44 yunx Exp $
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
     &     applyIcefrontTendT,applyIcefrontTendS,
     &     ICEFRONTdepthFile, ICEFRONTlengthFile

#ifdef ALLOW_SUBGLACIAL_RUNOFF
      NAMELIST /SGRUNOFF_PARMS/
     &     SGRUNOFF_period, SGRUNOFF_cycle,
     &     SGrunoffFile
#endif /* ALLOW_SUBGLACIAL_RUNOFF*/

      _BEGIN_MASTER(myThid)

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      ICEFRONTisON=.TRUE.

C Set defaults values for parameters in ICEFRONT.h
#ifdef ALLOW_SHELFICE
      IF (useShelfice) THEN
C Use the same values as in SHELFICE
       ICEFRONTlatentHeat       = SHELFICElatentHeat
       ICEFRONTHeatCapacity_Cp  = SHELFICEHeatCapacity_Cp
       rhoIcefront              = rhoShelfice
       ICEFRONTheatTransCoeff   = SHELFICEheatTransCoeff
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
       ICEFRONTheatTransCoeff   =    1.0 _d -04
       ICEFRONTkappa            =   1.54 _d -06
       ICEFRONTthetaSurface     = - 20.0 _d 0
      ENDIF

      recip_ICEFRONTlatentHeat =    0.0 _d 0
      ICEFRONTsaltTransCoeff   = UNSET_RL
      ICEFRONTlengthFile       = ' '
      ICEFRONTdepthFile        = ' '
      applyIcefrontTendS       = .TRUE.
      applyIcefrontTendT       = .TRUE.

#ifdef ALLOW_SUBGLACIAL_RUNOFF
      SGrunoffFile             = ' '
      SGRUNOFF_period          = 0.0
      SGRUNOFF_cycle           = 31104000.0
#endif /* ALLOW_SUBGLACIAL_RUNOFF*/

C Open and read the data.icefront file
      WRITE(msgBuf,'(A)') ' ICEFRONT_READPARMS: opening data.icefront'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.icefront', 'ICEFRONT_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=ICEFRONT_PARM01)
#ifdef ALLOW_SUBGLACIAL_RUNOFF
      READ(UNIT=iUnit,NML=SGRUNOFF_PARMS)
#endif /* ALLOW_SUBGLACIAL_RUNOFF*/
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
#ifdef ALLOW_SUBGLACIAL_RUNOFF
      IF (SGrunoffFile .NE. ' ') THEN
C If there is subglacial runoff, addMass (in main code) should be turn on by 
C setting selectAddFluid = 1; temperature is 0 degree Celsius (UNSET_RL by 
C default otherwise); salinity is 0 by default, and doesn't need to define here;
C staggerTimeStep should be set to avoid conflict.
         selectAddFluid = 1
         temp_EvPrRn = 0
         staggerTimeStep = .true.
      ENDIF
#endif /* ALLOW_SUBGLACIAL_RUNOFF*/
      _END_MASTER(myThid)
C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
