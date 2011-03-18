C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_inadmode_unset_ad.F,v 1.3 2011/03/18 10:49:26 mlosch Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

      subroutine adautodiff_inadmode_unset( myThid )
      IMPLICIT NONE
C     /==========================================================\
C     | SUBROUTINE autodiff_inadmode_unset                       |
C     |==========================================================|
C     == Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      integer myThid
#ifdef ALLOW_DEBUG
      character*(MAX_LEN_MBUF) msgBuf
#endif

      inAdMode = inAdFALSE

      useKPP    = useKPPinFwdMode
      useGMredi = useGMrediInFwdMode
      useSEAICE = useSEAICEinFwdMode
#ifdef ALLOW_DEBUG
      if ( debugLevel .GE. debLevB ) then
       write(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_UNSET_AD: useKPP    =', useKPP
       call print_message( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
       write(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_UNSET_AD: useGMredi =', useGMredi
       call print_message( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
       write(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_UNSET_AD: useSEAICE =', useSEAICE
       call print_message( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT , myThid)
      endif
#endif

      end









