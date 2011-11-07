C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_diagnostics_init.F,v 1.10 2011/11/07 20:22:33 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE PTRACERS_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Routine to initialize pTracers diagnostics

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "PTRACERS_SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_PARAMS.h"
#include "GAD.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid :: my Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf      :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

      INTEGER      iTrc, ilnb, tracerId
      CHARACTER*7  trcUnits
      CHARACTER*9  flxUnits, wUnits
      CHARACTER*30 locName
      CHARACTER*4  diagSufx
C     Functions ::
      INTEGER     ILNBLNK
      EXTERNAL    ILNBLNK
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Add diagnostics to the (long) list

      IF ( usingPCoords ) THEN
         flxUnits = '.Pa.m^2/s'
         wUnits   = '.Pa/s    '
      ELSE
         flxUnits = '.m^3/s   '
         wUnits   = '.m/s     '
      ENDIF

      DO iTrc=1,PTRACERS_num
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    Set default name & tracer Units:
       WRITE(locName,'(A,I4,A)') 'Tracer ',iTrc
       trcUnits  = '(kg/kg)'
C-     use name & units from data.ptracers :
       ilnb = ILNBLNK(PTRACERS_units(iTrc))
       IF ( ilnb.GE.1 ) THEN
         ilnb = LEN(trcUnits)
         trcUnits = PTRACERS_units(iTrc)(1:ilnb)
       ENDIF
       ilnb = ILNBLNK(PTRACERS_long_names(iTrc))
       IF ( ilnb.GE.1 ) THEN
         ilnb = MIN(LEN(locName),ilnb)
         WRITE(locName,'(A)') PTRACERS_long_names(iTrc)(1:ilnb)
       ELSE
         ilnb = ILNBLNK(PTRACERS_names(iTrc))
         IF ( ilnb.GE.1 ) THEN
           ilnb = MIN(LEN(locName),ilnb)
           WRITE(locName,'(A)') PTRACERS_names(iTrc)(1:ilnb)
         ENDIF
       ENDIF
       ilnb = MAX(ILNBLNK(locName),1)

C--    Add diagnostics of Tracer concentration & u,v,w Transport
       WRITE(diagName,'(A4,A2,A2)') 'TRAC',PTRACERS_ioLabel(iTrc),'  '
       WRITE(diagTitle,'(2A)') locName(1:ilnb), ' concentration'
       diagUnits = trcUnits//'         '
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       WRITE(diagName,'(A5,A2,A1)') 'UTRAC',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') 'Zonal Mass-Weighted Transp of ',
     &                 locName(1:ilnb)
       diagUnits = trcUnits//'.m/s   '
       diagCode  = 'UUr     MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       WRITE(diagName,'(A5,A2,A1)') 'VTRAC',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') 'Merid Mass-Weighted Transp of ',
     &                 locName(1:ilnb)
       diagUnits = trcUnits//'.m/s   '
       diagCode  = 'VVr     MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       WRITE(diagName,'(A5,A2,A1)') 'WTRAC',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') 'Vert  Mass-Weighted Transp of ',
     &                 locName(1:ilnb)
       diagUnits = trcUnits//wUnits
       diagCode  = 'WM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    add diagnostics of advective & diffusive flux:
       diagUnits = trcUnits//flxUnits
       tracerId = iTrc + GAD_TR1 - 1
       diagSufx = GAD_DIAG_SUFX( tracerId, myThid )

C-     Advective flux:
       diagName  = 'ADVr'//diagSufx
       WRITE(diagTitle,'(2A)') 'Vertical   Advective Flux of ',
     &                 locName(1:ilnb)
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'ADVx'//diagSufx
       WRITE(diagTitle,'(2A)') 'Zonal      Advective Flux of ',
     &                 locName(1:ilnb)
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'ADVy'//diagSufx
       WRITE(diagTitle,'(2A)') 'Meridional Advective Flux of ',
     &                 locName(1:ilnb)
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C-     Diffusive flux:
       diagName  = 'DFrE'//diagSufx
       WRITE(diagTitle,'(2A)') 'Vertical Diffusive Flux of ',
     &                 locName(1:ilnb)//' (Explicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'DFxE'//diagSufx
       WRITE(diagTitle,'(2A)') 'Zonal      Diffusive Flux of ',
     &                 locName(1:ilnb)
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'DFyE'//diagSufx
       WRITE(diagTitle,'(2A)') 'Meridional Diffusive Flux of ',
     &                 locName(1:ilnb)
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'DFrI'//diagSufx
       WRITE(diagTitle,'(2A)') 'Vertical Diffusive Flux of ',
     &                 locName(1:ilnb)//' (Implicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--    add diagnostics for Adams-Bashforth tendency increment
       diagName  = 'AB_g'//diagSufx
       WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &                   ' tendency from Adams-Bashforth'
       diagCode  = 'SMR     MR      '
       diagUnits = trcUnits//'/s       '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--    add diagnostics for other pkg contribution to ptracer solution
       IF ( useKPP ) THEN
        diagName  = 'KPPg'//diagSufx
        WRITE(diagTitle,'(2A)') 'KPP non-local Flux of ',
     &                 locName(1:ilnb)
        diagCode  = 'WM      LR      '
        diagUnits = trcUnits//flxUnits
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       ENDIF

       IF ( useDOWN_SLOPE ) THEN
        diagName  = 'DSLP'//diagSufx
        WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &                   ' tendency from Down-Slope package'
        diagCode  = 'SMR     MR      '
        diagUnits = trcUnits//'/s       '
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ENDDO

      diagName  = 'Add2EmP '
      diagTitle = 'additional EmP from salt relaxation'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
