#include "AUTODIFF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE ADAUTODIFF_INADMODE_SET( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE ADAUTODIFF_INADMODE_SET
C     *==========================================================*

C     !USES:
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
#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_DIAGNOSTICS
#  include "DIAGNOSTICS_SIZE.h"
#  include "DIAGNOSTICS.h"
# endif
#endif /* ALLOW_AUTODIFF_MONITOR */

C     !INPUT/OUTPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_AUTODIFF_MONITOR
# ifdef ALLOW_DIAGNOSTICS
      IF ( useDiag4AdjOutp ) THEN
        CALL DIAGNOSTICS_SWITCH_ONOFF( myTime, myIter, myThid )
      ENDIF
# endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_AUTODIFF_MONITOR */

      inAdMode  = .TRUE.

      useKPP    = useKPPinAdMode
      useGMRedi = useGMRediInAdMode
      useSEAICE = useSEAICEinAdMode
      useGGL90  = useGGL90inAdMode
      useSALT_PLUME = useSALT_PLUMEinAdMode
#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
      ctrlSmoothCorrel2D  = useSmoothCorrel2DinAdMode
#endif
#ifdef ALLOW_SEAICE
      IF ( SEAICEuseFREEDRIFTswitchInAd ) THEN
        SEAICEuseFREEDRIFT = .NOT.SEAICEuseFREEDRIFTinFwdMode
        SEAICEuseLSR       = .NOT.SEAICEuseFREEDRIFT
      ENDIF
      IF ( SEAICEuseDYNAMICSswitchInAd ) THEN
        SEAICEuseDYNAMICS  = .NOT.SEAICEuseDYNAMICSinFwdMode
      ENDIF
      SEAICEadjMODE=SEAICEapproxLevInAd
C     SIregFacInAd = UNSET_RL by default, so we need to check this here
      IF ( SIregFacInAd .NE. UNSET_RL ) SINegFac = SIregFacInAd
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

#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
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
         WRITE(msgBuf,'(2A,L5)') 'S/R ADAUTODIFF_INADMODE_SET: ',
     &        'SEAICEuseLSR       =', SEAICEuseLSR
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
