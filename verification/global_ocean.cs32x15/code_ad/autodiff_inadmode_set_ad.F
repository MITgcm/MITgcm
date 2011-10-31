C $Header: /u/gcmpack/MITgcm/verification/global_ocean.cs32x15/code_ad/Attic/autodiff_inadmode_set_ad.F,v 1.1 2011/10/31 02:58:19 heimbach Exp $
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
#ifdef ALLOW_SEAICE
#include "SEAICE_PARAMS.h"
#endif

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
#ifdef ALLOW_DEBUG
      character*(MAX_LEN_MBUF) msgBuf
#endif

      inAdmode = inAdTrue

      IF ( SEAICEuseDynamics ) SEAICEuseFREEDRIFT = .TRUE.

      useKPP    = useKPPinAdMode
      useGMredi = useGMrediInAdMode
      useSEAICE = useSEAICEinAdMode
#ifdef ALLOW_DEBUG
      if ( debugLevel.GE.debLevC ) then
       write(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET_AD: useKPP    =', useKPP
       call print_message( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
       write(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET_AD: useGMredi =', useGMredi
       call print_message( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
       write(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET_AD: useSEAICE =', useSEAICE
       call print_message( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
      endif
#endif

      return
      end
