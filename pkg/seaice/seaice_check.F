C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_check.F,v 1.36 2010/05/30 17:43:24 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE SEAICE_CHECK
C     | o Validate basic package setup and inter-package
C     | dependencies.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "GAD.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

C     === Routine arguments ===
C     myThid ::  Number of this instance of SEAICE_CHECK
      INTEGER myThid

C     === Local variables ===
C     msgBuf :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL checkAdvSchArea, checkAdvSchHeff, checkAdvSchSnow
      LOGICAL checkAdvSchSalt, checkAdvSchAge

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
      checkAdvSchAge  = SEAICEadvAge  .AND. (
     &     SEAICEadvSchAge .NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchAge .NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchAge .NE.ENUM_DST2 .AND.
     &     SEAICEadvSchAge .NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchAge .NE.ENUM_DST3 .AND.
     &     SEAICEadvSchAge .NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchAge .NE.ENUM_OS7MP )
      IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR.
     &     checkAdvSchSnow .OR. checkAdvSchSalt .OR.
     &     checkAdvSchAge ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: allowed advection schemes',
     &          ' for heff, area, snow, salt, and ice-age are: '
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
       IF ( checkAdvSchAge  ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchAge  = ',  SEAICEadvSchAge
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
        checkAdvSchAge  = SEAICEadvAge  .AND.
     &     SEAICEadvSchAge .NE.ENUM_CENTERED_2ND
       IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR.
     &      checkAdvSchSnow .OR. checkAdvSchSalt .OR.
     &      checkAdvSchAge ) THEN
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
        IF ( checkAdvSchAge  ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchAge  = ',  SEAICEadvSchAge
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
       ENDIF
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
     &        'seaice now always exf to read input files.'
         CALL PRINT_ERROR( msgBuf, myThid )
#endif

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
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_ATM_TEMP'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#ifndef ALLOW_DOWNWARD_RADIATION
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_DOWNWARD_RADIATION'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#ifndef ALLOW_RUNOFF
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_RUNOFF'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#if !defined(EXF_READ_EVAP) && !defined(ALLOW_BULKFORMULAE)
         WRITE(msgBuf,'(A)')
     &     'need to set EXF_READ_EVAP or ALLOW_BULKFORMULAE in pkg/exf '
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#if !defined(ALLOW_ATM_WIND) && !defined(SEAICE_CGRID)
         WRITE(msgBuf,'(A)')
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

#ifdef ALLOW_SALT_PLUME
#ifndef SEAICE_SALINITY
      IF ( UseSALT_PlUME ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: UseSALT_PlUME requires that'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICE_SALINITY be defined.'
        CALL PRINT_ERROR( msgBuf , myThid)
        STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF
#endif /* SEAICE_SALINITY */
#endif /* ALLOW_SALT_PLUME */

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

#if !defined(SEAICE_CGRID)
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
#endif /* SEAICE_CGRID not defined */

      _END_MASTER(myThid)

      RETURN
      END
