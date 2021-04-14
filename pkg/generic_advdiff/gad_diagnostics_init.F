#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_DIAGNOSTICS_INIT
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
C     myThid   :: My Thread Id. number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     ! FUNCTIONS:
      CHARACTER*4  GAD_DIAG_SUFX
      CHARACTER*16 DIAGS_MK_UNITS
      CHARACTER*80 DIAGS_MK_TITLE
      EXTERNAL     GAD_DIAG_SUFX
      EXTERNAL     DIAGS_MK_UNITS
      EXTERNAL     DIAGS_MK_TITLE

C     !LOCAL VARIABLES:
C     === Local variables ===
C     msgBuf   :: Informational/error meesage buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER        diagNum
      INTEGER        diagMate
      CHARACTER*8    diagName
      CHARACTER*16   diagCode
      CHARACTER*16   diagUnits
      CHARACTER*(80) diagTitle

      CHARACTER*10  flxUnits, trUnits
      CHARACTER*12  locName
      CHARACTER*4   diagSufx

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Add diagnostics to the (long) list
       IF ( usingPCoords ) THEN
         flxUnits = '.Pa.m^2/s '
       ELSE
         flxUnits = '.m^3/s    '
       ENDIF

C-     add diagnostics of advective & diffusive flux of Temp :
       IF ( fluidIsAir ) THEN
         trUnits = 'K'
       ELSE
         trUnits = 'degC'
       ENDIF
       diagUnits = DIAGS_MK_UNITS( trUnits//flxUnits, myThid )
       diagSufx = GAD_DIAG_SUFX( GAD_TEMPERATURE, myThid )

C-     Advective flux:
       diagName  = 'ADVr'//diagSufx
       diagTitle = 'Vertical   Advective Flux of Pot.Temperature'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'ADVx'//diagSufx
       diagTitle = 'Zonal      Advective Flux of Pot.Temperature'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'ADVy'//diagSufx
       diagTitle = 'Meridional Advective Flux of Pot.Temperature'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
C-     Diffusive flux:
       diagName  = 'DFrE'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of Pot.Temperature'
     &           //' (Explicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'DFxE'//diagSufx
       diagTitle = 'Zonal      Diffusive Flux of Pot.Temperature'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'DFyE'//diagSufx
       diagTitle = 'Meridional Diffusive Flux of Pot.Temperature'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'DFrI'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of Pot.Temperature'
     &           //' (Implicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef GAD_ALLOW_TS_SOM_ADV
       diagUnits = DIAGS_MK_UNITS( trUnits, myThid )

       diagName  = 'SM_x'//diagSufx
       diagTitle = 'Pot.Temp.   1rst Order Moment Sx'
       diagCode  = 'UM      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SM_y'//diagSufx
       diagTitle = 'Pot.Temp.   1rst Order Moment Sy'
       diagCode  = 'VM      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SM_z'//diagSufx
       diagTitle = 'Pot.Temp.   1rst Order Moment Sz'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SMxx'//diagSufx
       diagTitle = 'Pot.Temp.   2nd Order Moment Sxx'
       diagCode  = 'UM      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SMyy'//diagSufx
       diagTitle = 'Pot.Temp.   2nd Order Moment Syy'
       diagCode  = 'VM      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SMzz'//diagSufx
       diagTitle = 'Pot.Temp.   2nd Order Moment Szz'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SMxy'//diagSufx
       diagTitle = 'Pot.Temp.   2nd Order Moment Sxy'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'SMxz'//diagSufx
       diagTitle = 'Pot.Temp.   2nd Order Moment Sxz'
       diagCode  = 'UM      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SMyz'//diagSufx
       diagTitle = 'Pot.Temp.   2nd Order Moment Syz'
       diagCode  = 'VM      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagUnits = DIAGS_MK_UNITS( '('//trUnits//')^2', myThid )
       diagName  = 'SM_v'//diagSufx
       diagTitle = 'Pot.Temp.   sub-grid variance'
       diagCode  = 'SM P    MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* GAD_ALLOW_TS_SOM_ADV */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-     add diagnostics of advective & diffusive flux of Salt :
       IF ( fluidIsAir ) THEN
         locName = 'Water-Vapor '
         IF ( useAIM ) THEN
           trUnits = 'g/kg'
         ELSE
           trUnits = 'kg/kg'
         ENDIF
       ELSE
         locName = 'Salinity    '
         trUnits = 'g/kg'
       ENDIF
       diagUnits = DIAGS_MK_UNITS( trUnits//flxUnits, myThid )
       diagSufx = GAD_DIAG_SUFX( GAD_SALINITY, myThid )

C-     Advective flux:
       diagName  = 'ADVr'//diagSufx
       diagTitle = 'Vertical   Advective Flux of '//locName
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'ADVx'//diagSufx
       diagTitle = 'Zonal      Advective Flux of '//locName
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'ADVy'//diagSufx
       diagTitle = 'Meridional Advective Flux of '//locName
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
C-     Diffusive flux:
       diagName  = 'DFrE'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of '//locName
     &           // '(Explicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'DFxE'//diagSufx
       diagTitle = 'Zonal      Diffusive Flux of '//locName
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'DFyE'//diagSufx
       diagTitle = 'Meridional Diffusive Flux of '//locName
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagName  = 'DFrI'//diagSufx
       diagTitle = 'Vertical Diffusive Flux of '//locName
     &           //'(Implicit part)'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SALTFILL'
       diagTitle = 'Filling of Negative Values of '//locName
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef GAD_ALLOW_TS_SOM_ADV

       diagUnits = DIAGS_MK_UNITS( trUnits, myThid )

       diagName  = 'SM_x'//diagSufx
       diagTitle = locName//'1rst Order Moment Sx'
       diagCode  = 'UM      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SM_y'//diagSufx
       diagTitle = locName//'1rst Order Moment Sy'
       diagCode  = 'VM      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SM_z'//diagSufx
       diagTitle = locName//'1rst Order Moment Sz'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SMxx'//diagSufx
       diagTitle = locName//'2nd Order Moment Sxx'
       diagCode  = 'UM      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SMyy'//diagSufx
       diagTitle = locName//'2nd Order Moment Syy'
       diagCode  = 'VM      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SMzz'//diagSufx
       diagTitle = locName//'2nd Order Moment Szz'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagName  = 'SMxy'//diagSufx
       diagTitle = locName//'2nd Order Moment Sxy'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'SMxz'//diagSufx
       diagTitle = locName//'2nd Order Moment Sxz'
       diagCode  = 'UM      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SMyz'//diagSufx
       diagTitle = locName//'2nd Order Moment Syz'
       diagCode  = 'VM      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

       diagUnits = DIAGS_MK_UNITS( '('//trUnits//')^2', myThid )
       diagName  = 'SM_v'//diagSufx
       diagTitle = locName//'sub-grid variance'
       diagCode  = 'SM P    MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* GAD_ALLOW_TS_SOM_ADV */

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
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GAD.h"
#ifdef ALLOW_PTRACERS
c#include "PARAMS.h"
# include "PTRACERS_SIZE.h"
# include "PTRACERS_PARAMS.h"
#endif /* ALLOW_PTRACERS */

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
#ifdef ALLOW_PTRACERS
c     ELSEIF( usePTRACERS
c       .AND. tracerId.GE.GAD_TR1
      ELSEIF( tracerId.GE.GAD_TR1
     &  .AND. tracerId.LT.GAD_TR1+PTRACERS_num ) THEN
c       WRITE(GAD_DIAG_SUFX,'(A,A2)')
c    &                  'Tr', PTRACERS_ioLabel( tracerId+1-GAD_TR1 )
        GAD_DIAG_SUFX = 'Tr'//PTRACERS_ioLabel( tracerId+1-GAD_TR1 )
#endif /* ALLOW_PTRACERS */
      ELSE
        GAD_DIAG_SUFX = 'aaaa'
      ENDIF

      RETURN
      END
