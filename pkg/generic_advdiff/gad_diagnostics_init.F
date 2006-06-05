C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_diagnostics_init.F,v 1.2 2006/06/05 17:17:31 molod Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_INIT
C     !INTERFACE:
      SUBROUTINE GAD_DIAGNOSTICS_INIT( myThid )
C     !DESCRIPTION:
C     Routine to initialize Generic Advection/Diffusion diagnostics

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid -  Number of this instance of GAD_INIT
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

      CHARACTER*10  flxUnits
      CHARACTER*12  locName
      CHARACTER*4 GAD_DIAG_SUFX, diagSufx
c     EXTERNAL    GAD_DIAG_SUFX

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Add diagnostics to the (long) list
       IF ( usingPCoords ) THEN
         flxUnits = '.Pa.m^2/s '
       ELSE
         flxUnits = '.m^3/s    '
       ENDIF

C-     add diagnostics of advective & diffusive flux of Temp :
       IF ( fluidIsAir ) THEN
         WRITE(diagUnits,'(2A)') 'K',flxUnits
       ELSE
         WRITE(diagUnits,'(2A)') 'degC',flxUnits
       ENDIF
       diagSufx = GAD_DIAG_SUFX( GAD_TEMPERATURE, myThid )

C-     Advective flux:
       diagName  = 'ADVr'//diagSufx
       diagTitle = 'Vertical   Advective Flux of Pot.Temperature'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'ADVx'//diagSufx
       diagTitle = 'Zonal      Advective Flux of Pot.Temperature'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'ADVy'//diagSufx
       diagTitle = 'Meridional Advective Flux of Pot.Temperature'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ',diagNum,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
C-     Diffusive flux:
       diagName  = 'DFrE'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of Pot.Temperature'
     &           //' (Explicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'DIFx'//diagSufx
       diagTitle = 'Zonal      Diffusive Flux of Pot.Temperature'
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'DIFy'//diagSufx
       diagTitle = 'Meridional Diffusive Flux of Pot.Temperature'
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ',diagNum,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 

       diagName  = 'DFrI'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of Pot.Temperature'
     &           //' (Implicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-     add diagnostics of advective & diffusive flux of Salt :
       IF ( fluidIsAir ) THEN
         locName = ' Water-Vapor'
         WRITE(diagUnits,'(2A)') 'g/kg',flxUnits
#ifdef ALLOW_FIZHI
        IF(useFIZHI)THEN
         WRITE(diagUnits,'(2A)') 'kg/kg',flxUnits
        ENDIF
#endif /* ALLOW_FIZHI */
       ELSE
         locName = ' Salinity   '
         WRITE(diagUnits,'(2A)') 'psu',flxUnits
       ENDIF
       diagSufx = GAD_DIAG_SUFX( GAD_SALINITY, myThid )

C-     Advective flux:
       diagName  = 'ADVr'//diagSufx
       diagTitle = 'Vertical   Advective Flux of'//locName
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'ADVx'//diagSufx
       diagTitle = 'Zonal      Advective Flux of'//locName
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'ADVy'//diagSufx
       diagTitle = 'Meridional Advective Flux of'//locName
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ',diagNum,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
C-     Diffusive flux:
       diagName  = 'DFrE'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of'//locName
     &           // ' (Explicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'DIFx'//diagSufx
       diagTitle = 'Zonal      Diffusive Flux of'//locName
       WRITE(diagCode,'(A,I3.3,A)') 'UU   ',diagNum+2,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 
       diagName  = 'DIFy'//diagSufx
       diagTitle = 'Meridional Diffusive Flux of'//locName
       WRITE(diagCode,'(A,I3.3,A)') 'VV   ',diagNum,'MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 

       diagName  = 'DFrI'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of'//locName
     &           //' (Implicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 

       diagName  = 'SALTFILL'
       diagTitle = 'Filling of Negative Values of'//locName
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADD2LIST( diagNum,
     I              diagName, diagCode, diagUnits, diagTitle, myThid ) 

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP 0
C     !ROUTINE: GAD_DIAG_SUFX

C     !INTERFACE:
      CHARACTER*4 FUNCTION GAD_DIAG_SUFX( tracerId, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION GAD_DIAG_SUFX
C     | o Return diagnostic suffix (4 character long) for the
C     |   "tracerId" tracer (used to build diagnostic names).
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "GAD.h"

C     !INPUT PARAMETERS:
C     tracerId   ::  tracer identifier
C     myThid     ::  my thread Id number
      INTEGER      tracerId
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:

C--   Set diagnostic suffix (4 character long) for the "tracerId" tracer
      IF ( tracerId.EQ.GAD_TEMPERATURE ) THEN
        GAD_DIAG_SUFX = '_TH '
      ELSEIF( tracerId.EQ.GAD_SALINITY ) THEN
        GAD_DIAG_SUFX = '_SLT'
      ELSEIF( tracerId.GE.GAD_TR1
     &    .AND. tracerId.LT.100 ) THEN
        WRITE(GAD_DIAG_SUFX,'(A,I2.2)') 'Tr',tracerId + 1-GAD_TR1
      ELSE
        GAD_DIAG_SUFX = 'aaaa'
      ENDIF

      RETURN
      END
