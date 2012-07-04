C $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code_ad/Attic/autodiff_inadmode_unset_ad.F,v 1.2 2012/07/04 20:25:51 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      SUBROUTINE ADAUTODIFF_INADMODE_UNSET( myThid )
C     *==========================================================*
C     | SUBROUTINE ADAUTODIFF_INADMODE_UNSET
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

      inAdMode  = inAdFalse

      useKPP    = useKPPinFwdMode
      useGMRedi = useGMRediInFwdMode
      useSEAICE = useSEAICEinFwdMode

      IF ( SEAICEuseDynamics ) SEAICEuseFREEDRIFT = .FALSE.

#ifdef ALLOW_DEBUG
      IF ( debugLevel.GE.debLevC ) THEN
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_UNSET: useKPP    =', useKPP
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_UNSET: useGMRedi =', useGMRedi
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_UNSET: useSEAICE =', useSEAICE
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#endif

      RETURN
      END
