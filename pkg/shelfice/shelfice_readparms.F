C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_readparms.F,v 1.2 2006/02/10 10:00:42 mlosch Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_READPARMS

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_READPARMS( myThid )

C !DESCRIPTION:
C     Initialize SHELFICE parameters, read in data.shelfice

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SHELFICE.h"
#include "PARAMS.h"
#ifdef ALLOW_MNC
# include "MNC_PARAMS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_SHELFICE

C !LOCAL VARIABLES: ====================================================
C  iUnit		:: unit number for I/O
C  msgBuf		:: message buffer
      INTEGER iUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      NAMELIST /SHELFICE_PARM01/
     &     SHELFICEexchangeVelocity,
     &     SHELFICElatentHeat, 
     &     SHELFICEDragLinear, SHELFICEDragQuadratic,
     &     useISOMIPTD, no_slip_shelfice,
     &     SHELFICEwriteState,
     &     SHELFICE_dumpFreq,
     &     SHELFICE_taveFreq,
     &     SHELFICE_tave_mnc,
     &     SHELFICE_dump_mnc,
     &     SHELFICEloadAnomalyFile

C This routine has been called by the main model so we set our
C internal flag to indicate we are in business
      SHELFICEisON=.TRUE.

C Set defaults values for parameters in SHELFICE.h
      useISOMIPTD              = .TRUE.
      SHELFICEloadAnomalyFile  = ' '
      SHELFICElatentHeat       = 334.0 _d 3
      recip_SHELFICElatentHeat = 0. _d 0
      SHELFICEexchangeVelocity = 0. _d 0
      no_slip_shelfice         = no_slip_bottom
      SHELFICEDragLinear       = bottomDragLinear
      SHELFICEDragQuadratic    = bottomDragQuadratic
      SHELFICEwriteState       = .FALSE.
      SHELFICE_dumpFreq        = dumpFreq
      SHELFICE_taveFreq        = taveFreq
#ifdef ALLOW_MNC
      SHELFICE_tave_mnc = timeave_mnc
      SHELFICE_dump_mnc = snapshot_mnc
#else
      SHELFICE_tave_mnc = .FALSE.
      SHELFICE_dump_mnc = .FALSE.
#endif
CMLC Eingabe der Konstanten
CMLC gat = turbul. heat exchange coeff. [m/s]
CMLC gas = turbul. salt exchange coeff. [m/s]
CMLC HeMW=Heliumgehalt des Schmelzwassers
CMLC OoFW=Isotopeneintrag beim Frieren
CMLC OoMW=Isotopeneintrag beim Schmelzen
CMLc aqui
CML      a = -0.0575           !
CML      b = 0.0901                !Freezing point constants
CML      c = 7.61e-4               !
CML
CML      pn    = 13.8              !Prandtl number
CML      sn    = 2432.             !Schmidt number
CMLc      appa_t = 1.34e-07          !molecular thermal diffusivity
CMLc      appa_s = 7.40e-10          !molecular salt diffusivity
CML      un     = 1.95e-6
CMLc      pn     = un/appa_t
CMLc      sn     = un/appa_s
CML      pn1    = pn**(2./3.)
CML      sn1    = sn**(2./3.)
CML      cd     = 2.50e-3
CML      srcd   = sqrt(cd)
CML
CMLc      arma   = 0.4                 ! Karman constant
CMLc      z0_i   = 0.00005             ! roughness length
CMLc      srcd   = arma/log(1/z0_i)
CML
CML      lhf = 3.33e+5
CML      cpi = 152.5+7.122*(atk+tob) !Paterson:"The Physics of Glaciers"
CML
CML      rhoi=917.
CML      rho0=1000.
CML 
CML      hemw= 4.02*14.
CML      oomw=-30.
CML      oofw=-2.5
CML      SHELFICEnRi        = 2
CML      SHELFICEviscMin    = 0. _d 0
CML      SHELFICEdiffMin    = 0. _d 0
CML      SHELFICEviscMax    = 1. _d 0
CML      SHELFICEnu0        = 1. _d -02
CML      SHELFICEalpha      = 5. _d 0
CML      RiLimit      = UNSET_RL
CML      SHELFICEdumpFreq   = dumpFreq
CML      SHELFICEtaveFreq   = taveFreq
CML      SHELFICEMixingMaps = .FALSE.
CML      SHELFICEwriteState = .FALSE.

C Open and read the data.shelfice file
      _BEGIN_MASTER(myThid)
      WRITE(msgBuf,'(A)') ' SHELFICE_READPARMS: opening data.shelfice'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)
      CALL OPEN_COPY_DATA_FILE(
     I                   'data.shelfice', 'SHELFICE_READPARMS',
     O                   iUnit,
     I                   myThid )
      READ(UNIT=iUnit,NML=SHELFICE_PARM01)
      WRITE(msgBuf,'(A)')
     &  ' SHELFICE_READPARMS: finished reading data.shelfice'
      CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &                   SQUEEZE_RIGHT , 1)

C Close the open data file
      CLOSE(iUnit)
      _END_MASTER(myThid)

C Everyone else must wait for the parameters to be loaded
      _BARRIER

C Now set-up any remaining parameters that result from the input parameters 
      IF ( SHELFICElatentHeat .NE. 0. _d 0 )
     &     recip_SHELFICElatentHeat = 1. _d 0/SHELFICElatentHeat

C-    Set Output type flags :
      SHELFICE_tave_mdsio = .TRUE.
      SHELFICE_dump_mdsio = .TRUE.
#ifdef ALLOW_MNC
      IF (useMNC) THEN
        IF ( .NOT.outputTypesInclusive
     &       .AND. SHELFICE_tave_mnc ) SHELFICE_tave_mdsio = .FALSE.
        IF ( .NOT.outputTypesInclusive
     &       .AND. SHELFICE_dump_mnc ) SHELFICE_dump_mdsio = .FALSE.
      ENDIF
#endif

#ifdef ALLOW_MNC
C     Initialize MNC variable information for SHELFICE
      IF ( useMNC .AND. (shelfice_tave_mnc.OR.shelfice_dump_mnc)
     &    ) THEN
       CALL SHELFICE_MNC_INIT( myThid )
      ENDIF
#endif /* ALLOW_MNC */

#endif /* ALLOW_SHELFICE */

      RETURN
      END
