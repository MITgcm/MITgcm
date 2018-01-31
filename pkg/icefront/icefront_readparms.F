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
C  myThid               :: my thread Id number
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

#ifdef ALLOW_EXF
      NAMELIST /ICEFRONT_EXF_PARM02/
     &     SGRunOffFile,   SGRunOffperiod,   SGRunOffStartTime,
     &     SGRunOffstartdate1, SGRunOffstartdate2,
     &     SGRunOffconst,      SGRunOff_inscal,
     &     SGRunOff_remov_intercept, SGRunOff_remov_slope
#endif /* ALLOW_EXF */

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

#ifdef ALLOW_EXF
      SGRunOffstartdate1       = 0
      SGRunOffstartdate2       = 0
      SGRunOffStartTime        = UNSET_RL
      SGRunOffperiod           = 0.0 _d 0
      SGRunOffconst            = 0.0 _d 0
      SGRunOff_remov_intercept = 0.0 _d 0
      SGRunOff_remov_slope     = 0.0 _d 0
      SGRunOff_inscal          = 1.0 _d 0
      SGRunOffFile             = ' '
C-    internal parameters (not in namelist):
      SGRunOffmask             = 'c'
#endif /* ALLOW_EXF */

C Open and read the data.icefront file
      WRITE(msgBuf,'(A)') ' ICEFRONT_READPARMS: opening data.icefront'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.icefront', 'ICEFRONT_READPARMS',
     O                   iUnit,
     I                   myThid )

      WRITE(msgBuf,'(A)') ' ICEFRONT_READPARMS: reading ICEFRONT_PARM01'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      READ(UNIT=iUnit,NML=ICEFRONT_PARM01)

#ifdef ALLOW_EXF
      IF ( useEXF ) THEN
       WRITE(msgBuf,'(A)')
     &     ' ICEFRONT_READPARMS: reading ICEFRONT_EXF_PARM02'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       READ( UNIT=iUnit, NML=ICEFRONT_EXF_PARM02 )
      ENDIF
#endif /* ALLOW_EXF */

      WRITE(msgBuf,'(A)')
     &  ' ICEFRONT_READPARMS: finished reading data.icefront'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C Close the open data file
#ifdef SINGLE_DISK_IO
      CLOSE(iUnit)
#else
      CLOSE(iUnit,STATUS='DELETE')
#endif /* SINGLE_DISK_IO */

C Now set-up any remaining parameters that result from the input parameters
      IF ( ICEFRONTlatentHeat .NE. 0. _d 0 )
     &     recip_ICEFRONTlatentHeat = 1. _d 0/ICEFRONTlatentHeat

      _END_MASTER(myThid)
C Everyone else must wait for the parameters to be loaded
      _BARRIER

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
