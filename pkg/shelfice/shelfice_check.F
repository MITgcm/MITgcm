C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_check.F,v 1.7 2012/04/11 15:51:05 jmc Exp $
C $Name:  $
#include "SHELFICE_OPTIONS.h"

      SUBROUTINE SHELFICE_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE SHELFICE_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"

C     === Routine arguments ===
C     myThid   :  Number of this instance of SHELFICE_CHECK
      INTEGER myThid

#ifdef ALLOW_SHELFICE

C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') 'SHELFICE_CHECK: #define ALLOW_SHELFICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     So far, SHELFICE works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'SHELFICE works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF

      IF ( selectNHfreeSurf.NE.0 ) THEN
C--   Non-Hydrostatic Free-Surf below Ice-Shelf:
C     a) there is a practical implementation issue (kSurf > 1) that has to be
C        fixed before removing this stop.
C     b) In general, should also account for the vertical acceleration of the
C        Ice-shelf mass above Free-Surf in NH.Free-Surf equation (less obvious).
C        Ignore this term for now; as a consequence, might over-estimate
C        d.Eta/dt below Ice-Shelf.
         WRITE(msgBuf,'(2A,I3,A)') 'Current nonHydrostatic',
     &    ' Free-Surf option (selectNHfreeSurf=',selectNHfreeSurf,' )'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)') ' not compatible with SHELFICE code'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF

C     SHELFICE may not work with many other packages,
C     e.g. vertical mixing schemes, in particular KPP will not work properly,
C     as KPP requires surface fluxes at the surface, whereas shelfice will
C     provide interfacial fluxes at some depth. Richardson flux number based
C     schemes such as Packanowski-Philander (PP81) should be no problem.
CML#ifdef ALLOW_KPP
CML      IF ( useKPP ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'SHELFICE and KPP cannot be turned on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
CML      ENDIF
CML#endif ALLOW_KPP

#ifndef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time control flag useISOMIPTD was used'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A,A)')
     &      'when CPP flag ALLOW_ISOMIP_TD was unset'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF
#else
      IF ( useISOMIPTD ) THEN
       IF ( SHELFICEconserve ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &       'Run-time control flag SHELFICEconserve=.TRUE. was used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,A,A)')
     &       'when useISOMIPTD=.TRUE.; this does not work!'
        CALL PRINT_ERROR( msgBuf, myThid )
        STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
       ENDIF
      ENDIF
#endif /* ALLOW_ISOMIP_TD */

#ifndef SHI_ALLOW_GAMMAFRICT
      IF ( SHELFICEuseGammaFrict ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time control flag SHELFICEuseGammaFrict was used'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A,A)')
     &      'when CPP flag SHI_ALLOW_GAMMAFRICT was unset'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SHELFICE_CHECK'
      ENDIF
#endif /* SHI_ALLOW_GAMMAFRICT */

      _END_MASTER(myThid)

#endif /* ALLOW_SHELFICE */

      RETURN
      END
