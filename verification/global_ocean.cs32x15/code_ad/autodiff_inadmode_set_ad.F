C $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code_ad/Attic/autodiff_inadmode_set_ad.F,v 1.2 2012/07/04 20:25:50 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE ADAUTODIFF_INADMODE_SET( myThid )
C     *==========================================================*
C     | SUBROUTINE ADAUTODIFF_INADMODE_SET
C     *==========================================================*

      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AUTODIFF_PARAMS.h"
#ifdef ALLOW_SEAICE
#include "SEAICE_PARAMS.h"
#endif

C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
#ifdef ALLOW_DEBUG
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

      inAdMode  = inAdTrue

      useKPP    = useKPPinAdMode
      useGMRedi = useGMRediInAdMode
      useSEAICE = useSEAICEinAdMode

      IF ( SEAICEuseDynamics ) SEAICEuseFREEDRIFT = .TRUE.

#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevC ) THEN
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET: useKPP    =', useKPP
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET: useGMRedi =', useGMRedi
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET: useSEAICE =', useSEAICE
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

      RETURN
      END
