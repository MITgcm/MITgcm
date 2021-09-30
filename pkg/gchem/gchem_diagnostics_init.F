#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_DIAGNOSTICS_INIT( myThid )

C !DESCRIPTION:
C define diagnostics for gchem package
C experiment

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "GCHEM.h"
c#include "GAD.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
#ifdef GCHEM_SEPARATE_FORCING

C     Functions ::
      INTEGER     ILNBLNK
      EXTERNAL    ILNBLNK
c     CHARACTER*4 GAD_DIAG_SUFX
c     EXTERNAL    GAD_DIAG_SUFX

C     !LOCAL VARIABLES:
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

      INTEGER      iTrc, ilnb
      CHARACTER*7  trcUnits
      CHARACTER*30 locName

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Define diagnostics Names :
      DO iTrc=1,MIN( gchem_sepFTr_num, PTRACERS_num )
C-     Set default name & tracer Units:
       WRITE(locName,'(A,I4.4,A)') 'Tracer ',iTrc
       trcUnits  = '(kg/kg)'
C-     use units & (short) name from data.ptracers :
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

       WRITE(diagName,'(A5,A2,A1)') 'GC_Tr',PTRACERS_ioLabel(iTrc),' '
       WRITE(diagTitle,'(2A)') locName(1:ilnb),
     &                         ' concentration before GCHEM'
       diagUnits = trcUnits//'         '
       diagCode  = 'SMR     MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I           diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      ENDDO

C--   Other diagnostics should be initialized in each package called by gchem

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
