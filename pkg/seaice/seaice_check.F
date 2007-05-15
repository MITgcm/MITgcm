C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_check.F,v 1.20 2007/05/15 14:32:56 mlosch Exp $
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

C     === Routine arguments ===
C     myThid -  Number of this instance of SEAICE_CHECK
      INTEGER myThid

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

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

C--   SEAICEadvSnow doees one work with LAD = 2
      IF (SEAICEadvSnow .AND. SEAICEadvSchSnow .EQ. 2
     &     .AND. LAD .EQ. 1) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     & ' 2nd order snow advection does not work with Leap frog (LAD==1)'
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

C--   SEAICE_EXTERNAL_FORCING
C     At present this option is hardwired for pkg/exf.
#ifdef SEAICE_EXTERNAL_FORCING
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
#if !defined(EXF_READ_EVAP) && !defined(ALLOW_BULKFORMULAE)
         WRITE(msgBuf,'(A)')
     &     'need to set EXF_READ_EVAP or ALLOW_BULKFORMULAE in pkg/exf '
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#if !defined(ALLOW_ATM_WIND) && \
    ( !defined(SEAICE_EXTERNAL_FLUXES) || !defined(SEAICE_CGRID) )
         WRITE(msgBuf,'(A)')
     &        'need to define pkg/exf ALLOW_ATM_WIND'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif
#endif /* ALLOW_EXF */

#else  /* SEAICE_EXTERNAL_FORCING */
C--   SEAICE_EXTERNAL_FLUXES :
C     currently, cannot be turned on if SEAICE_EXTERNAL_FORCING is undef
#ifdef SEAICE_EXTERNAL_FLUXES
         WRITE(msgBuf,'(2A)') 
     &    'S/R SEAICE_CHECK: SEAICE_EXTERNAL_FLUXES not allowed',
     &    ' without SEAICE_EXTERNAL_FORCING'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK'
#endif /* SEAICE_EXTERNAL_FLUXES */
#endif /* SEAICE_EXTERNAL_FORCING */

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

      _END_MASTER(myThid)

      RETURN
      END
