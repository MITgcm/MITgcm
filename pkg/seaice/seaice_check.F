C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_check.F,v 1.54 2012/02/03 13:34:31 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_CHECK
C     !INTERFACE:
      SUBROUTINE SEAICE_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_CHECK
C     | o Validate basic package setup and inter-package
C     |   dependencies.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL checkAdvSchArea, checkAdvSchHeff, checkAdvSchSnow
      LOGICAL checkAdvSchSalt

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _BEGIN_MASTER(myThid)

C--   ALLOW_SEAICE
      WRITE(msgBuf,'(A)') 'SEAICE_CHECK: #define ALLOW_SEAICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , myThid )

C--   SEAICE needs forcing_In_AB FALSE
      IF (tracForcingOutAB.NE.1) THEN
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &         ' Need T,S forcing out of AB (tracForcingOutAB=1)'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C--   check ice cover fraction formula
      IF ((SEAICEareaFormula.LT.1).OR.(SEAICEareaFormula.GT.3)) THEN
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &         ' SEAICEareaFormula must be between 1 and 3'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C--   Check advection schemes
      checkAdvSchArea = SEAICEadvArea .AND. (
     &     SEAICEadvSchArea.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchArea.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchArea.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchArea.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchArea.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchArea.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchArea.NE.ENUM_OS7MP )
      checkAdvSchHEFF = SEAICEadvHeff .AND. (
     &     SEAICEadvSchHeff.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchHeff.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchHeff.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchHeff.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchHeff.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchHeff.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchHeff.NE.ENUM_OS7MP )
      checkAdvSchSnow = SEAICEadvSnow .AND. (
     &     SEAICEadvSchSnow.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchSnow.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchSnow.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchSnow.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchSnow.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchSnow.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchSnow.NE.ENUM_OS7MP )
      checkAdvSchSalt = SEAICEadvSalt .AND. (
     &     SEAICEadvSchSalt.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchSalt.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchSalt.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchSalt.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchSalt.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchSalt.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchSalt.NE.ENUM_OS7MP )
      IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR.
     &     checkAdvSchSnow .OR. checkAdvSchSalt ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: allowed advection schemes',
     &          ' for heff, area, snow, and salt are: '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,7I3)') 'SEAICE_CHECK:',
     &      ENUM_UPWIND_1RST, ENUM_CENTERED_2ND, ENUM_DST2,
     &      ENUM_FLUX_LIMIT, ENUM_DST3, ENUM_DST3_FLUX_LIMIT,
     &      ENUM_OS7MP
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &      ' the following Adv.Scheme are not allowed:'
       CALL PRINT_ERROR( msgBuf, myThid )
       IF ( checkAdvSchArea ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchArea = ',  SEAICEadvSchArea
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( checkAdvSchHeff ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchHeff = ',  SEAICEadvSchHeff
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( checkAdvSchSnow ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchSnow = ',  SEAICEadvSchSnow
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( checkAdvSchSalt ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchSalt = ',  SEAICEadvSchSalt
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
      IF ( SEAICEadvScheme.EQ.ENUM_CENTERED_2ND ) THEN
C-- for now, the code does not allow to use the default advection scheme
C    (Centered 2nd order) for 1 ice-field and an other advection scheme
C    for an other ice-field. In this case, stop here.
        checkAdvSchArea = SEAICEadvArea .AND.
     &     SEAICEadvSchArea.NE.ENUM_CENTERED_2ND
        checkAdvSchHEFF = SEAICEadvHeff .AND.
     &     SEAICEadvSchHeff.NE.ENUM_CENTERED_2ND
        checkAdvSchSnow = SEAICEadvSnow .AND.
     &     SEAICEadvSchSnow.NE.ENUM_CENTERED_2ND
        checkAdvSchSalt = SEAICEadvSalt .AND.
     &     SEAICEadvSchSalt.NE.ENUM_CENTERED_2ND
       IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR.
     &      checkAdvSchSnow .OR. checkAdvSchSalt ) THEN
        WRITE(msgBuf,'(A,I3,A)') 'SEAICE_CHECK: SEAICEadvScheme=',
     &   SEAICEadvScheme, ' not compatible with those Adv.Scheme:'
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( checkAdvSchArea ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchArea = ',  SEAICEadvSchArea
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( checkAdvSchHeff ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchHeff = ',  SEAICEadvSchHeff
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( checkAdvSchSnow ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchSnow = ',  SEAICEadvSchSnow
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( checkAdvSchSalt ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchSalt = ',  SEAICEadvSchSalt
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
       ENDIF
      ELSEIF ( DIFF1 .NE. 0. _d 0 ) THEN
C-- for now, the code does not allow to use DIFF1 without the default
C   advection scheme (Centered 2nd order). In this case, stop here.
        WRITE(msgBuf,'(2A,1PE16.8)') 'SEAICE_CHECK: ',
     &        'harmonic+biharmonic DIFF1=',  DIFF1
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK: ',
     &        'not available with SEAICEadvScheme=', SEAICEadvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C     Avoid using both type of diffusion scheme (DIFF1 & SEAICEdiffKh)
      IF ( DIFF1 .NE. 0. _d 0 .AND. (
     &     ( SEAICEdiffKhHeff .NE. 0. _d 0 ) .OR.
     &     ( SEAICEdiffKhArea .NE. 0. _d 0 ) .OR.
     &     ( SEAICEdiffKhSnow .NE. 0. _d 0 ) .OR.
     &     ( SEAICEdiffKhSalt .NE. 0. _d 0 )
     &                              ) ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' DIFF1 > 0 and one of the SEAICEdiffKh[] > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' => Cannot use both type of diffusion'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C--
#ifdef ALLOW_AUTODIFF_TAMC
      IF ( NPSEUDOTIMESTEPS .GT. MPSEUDOTIMESTEPS ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' need to increase MPSEUDOTIMESTEPS in SEAICE_PARAMS.h'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,2I4)') 'SEAICE_CHECK:',
     &  ' MPSEUDOTIMESTEPS, NPSEUDOTIMESTEPS = ',
     &  MPSEUDOTIMESTEPS, NPSEUDOTIMESTEPS
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
      IF ( IMAX_TICE .GT. NMAX_TICE ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' need to increase NMAX_TICE in SEAICE_PARAMS.h'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,2I4)') 'SEAICE_CHECK:',
     &  ' NMAX_TICE, MAX_TICE = ', NMAX_TICE, IMAX_TICE
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
      IF ( SEAICE_maskRHS ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICE_maskRHS not allowed'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif

C--   LAD has been retired
      IF ( LAD .EQ. 1 ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     & ' 2nd order advection with Leap frog (LAD==1) has been retired'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     & ' please use the defauld modified Euler step (LAD==2)'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C--   SEAICE_ALLOW_DYNAMICS and SEAICEuseDYNAMICS
#ifndef SEAICE_ALLOW_DYNAMICS
      IF (SEAICEuseDYNAMICS) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_ALLOW_DYNAMICS needed for SEAICEuseDYNAMICS'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif

C--   SEAICE_EXTERNAL_FORCING is obsolete: issue warning but continue.
#ifdef SEAICE_EXTERNAL_FORCING
         WRITE(msgBuf,'(A)')
     &        'SEAICE_EXTERNAL_FORCING option is obsolete:'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &        'seaice now always uses exf to read input files.'
         CALL PRINT_ERROR( msgBuf, myThid )
#endif

C--   SEAICE_ALLOW_TD_IF is obsolete: issue warning and stop.
#ifdef SEAICE_ALLOW_TD_IF
         WRITE(msgBuf,'(A)')
     &     'SEAICE_ALLOW_TD_IF option is obsolete:'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'the seaice*_IF codes are now merged into the main branch.'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif /* SEAICE_SALINITY */

C--   SEAICE_SALINITY is obsolete: issue warning and stop.
#ifdef SEAICE_SALINITY
         WRITE(msgBuf,'(A)')
     &     'SEAICE_SALINITY option is obsolete'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'use SEAICE_VARIABLE_SALINITY instead.'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif /* SEAICE_SALINITY */

C--   SEAICE_OLD_AND_BAD_DISCRETIZATION is obsolete: issue warning and stop.
#ifdef SEAICE_OLD_AND_BAD_DISCRETIZATION
         WRITE(msgBuf,'(A)')
     &     'SEAICE_OLD_AND_BAD_DISCRETIZATION option is obsolete'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'and has no effect.'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif /* SEAICE_OLD_AND_BAD_DISCRETIZATION */

C--   pkg/seaice requires pkg/exf with following CPP options/
#ifndef ALLOW_EXF
         WRITE(msgBuf,'(A)')
     &        'need to define ALLOW_EXF'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#else /* ALLOW_EXF */
         IF ( .NOT.useEXF ) THEN
           WRITE(msgBuf,'(A)')
     &        'S/R SEAICE_CHECK: need to set useEXF in data.pkg'
           CALL PRINT_ERROR( msgBuf, myThid )
           STOP 'ABNORMAL END: S/R SEAICE_CHECK'
         ENDIF
#ifndef ALLOW_ATM_TEMP
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'need to define pkg/exf ALLOW_ATM_TEMP'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#ifndef ALLOW_DOWNWARD_RADIATION
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'need to define pkg/exf ALLOW_DOWNWARD_RADIATION'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#ifdef SEAICE_EXTERNAL_FLUXES
#if !defined(EXF_READ_EVAP) && !defined(ALLOW_BULKFORMULAE)
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &     'need to set EXF_READ_EVAP or ALLOW_BULKFORMULAE in pkg/exf '
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#else
         WRITE(msgBuf,'(3A)') 'S/R SEAICE_CHECK: ',
     &     'SEAICE_EXTERNAL_FLUXES is defined, so we assume you know ',
     &     'what you are doing.'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &     'Use S/R SEAICE_BUDGET_OCEAN to compute fluxes over ocean.'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT , myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
#endif /* SEAICE_EXTERNAL_FLUXES */
#if !defined(ALLOW_ATM_WIND) && !defined(SEAICE_CGRID)
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'need to define pkg/exf ALLOW_ATM_WIND'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#endif /* ALLOW_EXF */

      IF ( Olx.LT.3 .OR. Oly.LT.3 ) THEN
        WRITE(msgBuf,'(A,A)')
     &  'SEAICE_CHECK: cannot use EVP nor LSR solver with',
     &  ' overlap (Olx,Oly) smaller than 3'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

#ifdef SEAICE_ALLOW_EVP
# ifdef ALLOW_AUTODIFF_TAMC
      IF ( INT(SEAICE_deltaTdyn/SEAICE_deltaTevp).GT.nEVPstepMax ) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_ALLOW_EVP: need to set nEVPstepMax to >= nEVPstep'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I4)')
     &        'nEVPstep = INT(SEAICE_deltaTdyn/SEAICE_deltaTevp) = ',
     &        INT(SEAICE_deltaTdyn/SEAICE_deltaTevp)
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
# endif
#else
      IF ( SEAICE_deltaTevp .NE. UNSET_RL ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICE_deltaTevp is set'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'SEAICE_ALLOW_EVP is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif

#ifndef ALLOW_SEAICE_FLOODING
      IF ( SEAICEuseFlooding ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseFlooding = .TRUE.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'ALLOW_SEAICE_FLOODING is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif /* ALLOW_SEAICE_FLOODING */

#ifndef SEAICE_ALLOW_CLIPVELS
      IF ( SEAICE_clipVelocities ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICE_clipVelocities = .TRUE.'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'SEAICE_ALLOW_CLIPVELS is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif /* SEAICE_ALLOW_CLIPVELS */

#ifndef SEAICE_ALLOW_CLIPZETA
      IF ( SEAICE_evpDampC .GT. 0. _d 0 .OR.
     &     SEAICE_zetaMin  .GT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICE_evpDampC and/or SEAICE_zetaMin '//
     &      'are set in data.seaice'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'SEAICE_ALLOW_CLIPZETA is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif /* SEAICE_ALLOW_CLIPZETA */

#if !defined(SEAICE_ALLOW_TEM) || !defined(SEAICE_CGRID)
      IF ( SEAICEuseTEM ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseTEM requires that'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: SEAICE_ALLOW_TEM and SEAICE_CGRID are defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif

#ifndef SEAICE_CGRID
#ifdef SEAICE_TEST_ICE_STRESS_1
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: Only relevant for B-grid:'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: SEAICE_TEST_ICE_STRESS_1 is replaced by'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: SEAICE_BICE_STRESS (defined by default)'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif /* SEAICE_TEST_ICE_STRESS_1 */
      IF ( SEAICEuseDYNAMICS.AND.useCubedSphereExchange ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'B-grid dynamics not working on Cubed-Sphere grid'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
      IF ( SEAICEuseDYNAMICS.AND.useOBCS ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'Open-Boundaries not implemented in B-grid dynamics'
       CALL PRINT_ERROR( msgBuf, myThid )
C      STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif /* ndef SEAICE_CGRID */

C--   SEAICE_ALLOW_FREEDRIFT and SEAICEuseFREEDRIFT
#ifndef SEAICE_ALLOW_FREEDRIFT
      IF (SEAICEuseFREEDRIFT) THEN
        WRITE(msgBuf,'(A)')
     &   'need to #define SEAICE_ALLOW_FREEDRIFT for SEAICEuseFREEDRIFT'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
      IF ( LSR_mixIniGuess.GE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'need to #define SEAICE_ALLOW_FREEDRIFT to use LSR_mixIniGuess'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif

#ifndef SEAICE_VARIABLE_SALINITY
      IF ( SEAICEadvSalt ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICEadvSalt = .TRUE. but cpp-flag'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: '//
     &   'SEAICE_VARIABLE_SALINITY is undef in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif /* SEAICE_VARIABLE_SALINITY */

#if defined(FENTY_AREA_EXPANSION_CONTRACTION) && !defined(SEAICE_DO_OPEN_WATER_GROWTH)
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &      'FENTY_AREA_EXPANSION_CONTRACTION requires that'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: SEAICE_DO_OPEN_WATER_GROWTH is defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif

      _END_MASTER(myThid)

      RETURN
      END
