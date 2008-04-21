C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_check.F,v 1.27 2008/04/21 11:36:37 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

      SUBROUTINE SEAICE_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_CHECK                                  |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     \==========================================================/
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
C     myThid -  Number of this instance of SEAICE_CHECK
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL checkAdvSchArea, checkAdvSchHeff, checkAdvSchSnow
      LOGICAL checkAdvSchSalt

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
      IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR. 
     &     checkAdvSchSnow .OR. checkAdvSchSalt ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     & ' allowed advection schemes for heff, area, snow and salt are: '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,7I3)') 'SEAICE_CHECK:',
     &      ENUM_UPWIND_1RST, ENUM_CENTERED_2ND, ENUM_DST2,
     &      ENUM_FLUX_LIMIT, ENUM_DST3, ENUM_DST3_FLUX_LIMIT, 
     &      ENUM_OS7MP
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &      ' one of the following is not allowed'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &      ' SEAICEadvSchArea = ',  SEAICEadvSchArea
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &      ' SEAICEadvSchHeff = ',  SEAICEadvSchHeff
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &      ' SEAICEadvSchSnow = ',  SEAICEadvSchSnow
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &      ' SEAICEadvSchSalt = ',  SEAICEadvSchSalt
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C--   SEAICEadvSnow does not work with LAD = 1
      IF (SEAICEadvSnow .AND. SEAICEadvSchSnow .EQ. 2
     &     .AND. LAD .EQ. 1) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     & ' 2nd order snow advection does not work with Leap frog (LAD==1)'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

C--   SEAICEadvSalt does not work with LAD = 1
      IF (SEAICEadvSalt .AND. SEAICEadvSchSalt .EQ. 2
     &     .AND. LAD .EQ. 1) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     & ' 2nd order salt advection does not work with Leap frog (LAD==1)'
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
      _END_MASTER(myThid)

      RETURN
      END
