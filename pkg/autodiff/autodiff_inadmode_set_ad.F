C $Header: /u/gcmpack/MITgcm/pkg/autodiff/autodiff_inadmode_set_ad.F,v 1.12 2014/09/09 22:37:23 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

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
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#endif
#ifdef ALLOW_CTRL
#include "ctrl.h"
#endif

C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      inAdMode  = inAdTrue

      useKPP    = useKPPinAdMode
      useGMRedi = useGMRediInAdMode
      useSEAICE = useSEAICEinAdMode
      useGGL90  = useGGL90inAdMode
      useSALT_PLUME = useSALT_PLUMEinAdMode
#ifdef ALLOW_CTRL
      ctrlSmoothCorrel2D  = useSmoothCorrel2DinAdMode
#endif
#ifdef ALLOW_SEAICE
      IF ( SEAICEuseFREEDRIFTswitchInAd ) THEN
        SEAICEuseFREEDRIFT = .NOT.SEAICEuseFREEDRIFTinFwdMode
      ENDIF
      IF ( SEAICEuseDYNAMICSswitchInAd ) THEN
        SEAICEuseDYNAMICS  = .NOT.SEAICEuseDYNAMICSinFwdMode
      ENDIF
      SEAICEadjMODE=SEAICEapproxLevInAd
#endif /* ALLOW_SEAICE */
      viscFacAdj=viscFacInAd

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
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET: useGGL90    =', useGGL90
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET: useSALT_PLUME    =',
     &      useSALT_PLUME
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

       WRITE(msgBuf,'(A,1PE21.14)')
     &      'S/R ADAUTODIFF_INADMODE_SET: viscFacAdj =',viscFacAdj
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

#ifdef ALLOW_CTRL
       WRITE(msgBuf,'(A,L5)')
     &      'S/R ADAUTODIFF_INADMODE_SET: ctrlSmoothCorrel2D    =',
     &       ctrlSmoothCorrel2D
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
#endif

#ifdef ALLOW_SEAICE
       IF ( SEAICEuseFREEDRIFTswitchInAd ) THEN
         WRITE(msgBuf,'(2A,L5)') 'S/R ADAUTODIFF_INADMODE_SET: ',
     &        'SEAICEuseFREEDRIFT =', SEAICEuseFREEDRIFT
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( SEAICEuseDYNAMICSswitchInAd ) THEN
         WRITE(msgBuf,'(2A,L5)') 'S/R ADAUTODIFF_INADMODE_SET: ',
     &        'SEAICEuseDYNAMICS =', SEAICEuseDYNAMICS
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( SEAICEapproxLevInAd.NE.0 ) THEN
         WRITE(msgBuf,'(2A,I2)') 'S/R ADAUTODIFF_INADMODE_SET: ',
     &        'SEAICEadjMODE =', SEAICEadjMODE
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
       ENDIF
#endif /* ALLOW_SEAICE */

      ENDIF

      RETURN
      END
