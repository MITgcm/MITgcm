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
C     !FUNCTIONS:
      INTEGER     ILNBLNK
      EXTERNAL    ILNBLNK
      CHARACTER*4 GAD_DIAG_SUFX
      EXTERNAL    GAD_DIAG_SUFX
      CHARACTER*16 DIAGS_MK_UNITS
      EXTERNAL     DIAGS_MK_UNITS

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

      INTEGER      iTrc, ilnb, ilng, tracerId
      CHARACTER*7  trcUnits
      CHARACTER*9  flxUnits, wUnits
      CHARACTER*30 locName
      CHARACTER*60 longName
      CHARACTER*4  diagSufx

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
       WRITE(locName,'(A,I4.4,A)') 'Tracer ',iTrc
       trcUnits  = 'kg/kg  '
C-     use units & names from data.ptracers :
       ilnb = ILNBLNK(PTRACERS_units(iTrc))
       IF ( ilnb.GE.1 ) THEN
         ilnb = LEN(trcUnits)
         trcUnits = PTRACERS_units(iTrc)(1:ilnb)
       ENDIF
       ilnb = ILNBLNK(PTRACERS_names(iTrc))
       IF ( ilnb.GE.1 ) THEN
         ilnb = MIN(LEN(locName),ilnb)
         WRITE(locName,'(A)') PTRACERS_names(iTrc)(1:ilnb)
       ENDIF
       ilnb = MAX(ILNBLNK(locName),1)
C-     long name:
       WRITE(longName,'(A)') locName
       ilng = ILNBLNK(PTRACERS_long_names(iTrc))
       IF ( ilng.GE.1 ) THEN
         ilng = MIN(LEN(longName),ilng)
         WRITE(longName,'(A)') PTRACERS_long_names(iTrc)(1:ilng)
       ENDIF
       ilng = MAX(ILNBLNK(longName),1)

C--    Add diagnostics of Tracer concentration & u,v,w Transport
       WRITE(diagName,'(A4,A2,A2)') 'TRAC',PTRACERS_ioLabel(iTrc),'  '
       WRITE(diagTitle,'(2A)') longName(1:ilng), ' concentration'
       WRITE(diagUnits,'(A)') trcUnits
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagUnits = DIAGS_MK_UNITS( trcUnits//'.m/s', myThid )
       WRITE(diagName,'(A5,A2,A1)') 'UTRAC',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') 'Zonal Mass-Weighted Transp of ',
     &                 locName(1:ilnb)
       diagCode  = 'UUr     MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       WRITE(diagName,'(A5,A2,A1)') 'VTRAC',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') 'Merid Mass-Weighted Transp of ',
     &                 locName(1:ilnb)
       diagCode  = 'VVr     MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagUnits = DIAGS_MK_UNITS( trcUnits//wUnits, myThid )
       WRITE(diagName,'(A5,A2,A1)') 'WTRAC',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') 'Vert  Mass-Weighted Transp of ',
     &                 locName(1:ilnb)
       diagCode  = 'WM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C--    add diagnostics for Forcing and Adams-Bashforth tendency increment
       tracerId = iTrc + GAD_TR1 - 1
       diagSufx = GAD_DIAG_SUFX( tracerId, myThid )
       diagUnits = DIAGS_MK_UNITS( trcUnits//'/s', myThid )
       diagName  = 'Forc'//diagSufx
       WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &                   ' forcing tendency'
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'AB_g'//diagSufx
       WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &                   ' tendency from Adams-Bashforth'
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'Tp_g'//diagSufx
       WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &         ' total transport tendency (before gchem_forcing_sep)'
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--    Add diagnostics of advective & diffusive flux:
       diagUnits = DIAGS_MK_UNITS( trcUnits//flxUnits, myThid )

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--    Add diagnostics for other pkg contribution to ptracer solution
       IF ( useKPP ) THEN
        diagName  = 'KPPg'//diagSufx
        WRITE(diagTitle,'(2A)') 'KPP non-local Flux of ',
     &                 locName(1:ilnb)
        diagCode  = 'WM      LR      '
        diagUnits = DIAGS_MK_UNITS( trcUnits//flxUnits, myThid )
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I          diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       ENDIF

       IF ( useDOWN_SLOPE ) THEN
        diagName  = 'DSLP'//diagSufx
        WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &                   ' tendency from Down-Slope package'
        diagCode  = 'SMR     MR      '
        diagUnits = DIAGS_MK_UNITS( trcUnits//'/s', myThid )
        CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF
       WRITE(diagName,'(A6,A2)') 'ADJptr',PTRACERS_ioLabel(iTrc)
       WRITE(diagTitle,'(3A)') 'sensitivity to ',
     &                         longName(1:ilng), ' concentration'
       WRITE(diagUnits,'(2A)') 'dJ/',trcUnits
       diagCode  = 'SMRA    MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

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
