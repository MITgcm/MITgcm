#include "GAD_OPTIONS.h"

C--  File gad_diagnostics_init.F:
C--   Contents
C--   o GAD_DIAGNOSTICS_INIT
C--   o GAD_DIAG_SUFX (Function)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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
C     msgBuf   :: Informational/error message buffer
c     CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER        diagNum
      INTEGER        diagMate
      CHARACTER*8    diagName
      CHARACTER*16   diagCode
      CHARACTER*16   diagUnits
      CHARACTER*(80) diagTitle

      CHARACTER*10  flxUnits, trUnits, hvUnits
      CHARACTER*12  locName
      CHARACTER*4   diagSufx

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Add diagnostics to the (long) list
       IF ( usingPCoords ) THEN
         flxUnits = '.Pa.m^2/s '
         hvUnits  = '.m.Pa     '
       ELSE
         flxUnits = '.m^3/s    '
         hvUnits  = '.m^2      '
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
       diagTitle = 'Vertical Diffusive Flux of Pot.Temp.'
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
       diagTitle = 'Vertical Diffusive Flux of Pot.Temp.'
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

c      diagUnits = DIAGS_MK_UNITS( trUnits//'^2 '//flxUnits, myThid )
       diagUnits = DIAGS_MK_UNITS(
     &                   '('//trUnits//')^2'//flxUnits, myThid )
C-     Transport of tracer variance (splitted by direction):
       diagName  = 'VTRr'//diagSufx
       diagTitle = 'Vert. transport Flux of Pot.Temp. Variance'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'VTRx'//diagSufx
       diagTitle = 'Zonal transport Flux of Pot.Temp. Variance'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'VTRy'//diagSufx
       diagTitle = 'Merid transport Flux of Pot.Temp. Variance'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
C-     Numerical Production of variance (same units as variance Transport)
       diagName  = 'VPRr'//diagSufx
       diagTitle = 'Pot.Temp. Var Production by Vert. advection'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'VPRx'//diagSufx
       diagTitle = 'Pot.Temp. Var Production by Zonal advection'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'VPRy'//diagSufx
       diagTitle = 'Pot.Temp. Var Production by Merid advection'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
C-     Variance Production due to forcing (same units as variance Transport)
       diagName  = 'VPRf'//diagSufx
       diagTitle = 'Pot.Temp. Variance Production by Forcing'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I             diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C-     Residual in variance budget (same units as variance Transport)
       diagName  = 'VRes'//diagSufx
       diagTitle = 'Pot.Temp. Variance Budget Residual'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I             diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C--- for now, add specific diags for vertical implicit part
       diagName  = 'VTRi'//diagSufx
       diagTitle = 'Impl.Vert. transport Flux of Pot.Temp. Variance'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'VPRi'//diagSufx
       diagTitle = 'Pot.Temp. Var Production by Impl.Vert. advection'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C-     square of tracer difference in each direction (times face area):
       diagUnits = DIAGS_MK_UNITS( '('//trUnits//')^2.m^2', myThid )
C to do: need to fix units = trUnits ^2 x m^2    (if z-coords or vertical dir)
C                          = trUnits ^2 x m.Pa   (if p-coords & x,y dir)
       diagName  = 'SqDi'//diagSufx
       diagTitle = 'Square of Impl.Vert. gradient of Pot.Temperature'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C---
       diagName  = 'SqDr'//diagSufx
       diagTitle = 'Square of Vertical gradient of Pot.Temperature'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagUnits = DIAGS_MK_UNITS( '('//trUnits//')^2'//hvUnits,myThid )
       diagName  = 'SqDx'//diagSufx
       diagTitle = 'Square of Zonal    gradient of Pot.Temperature'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SqDy'//diagSufx
       diagTitle = 'Square of Meridio. gradient of Pot.Temperature'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-     add diagnostics of advective & diffusive flux of Salt :
       IF ( fluidIsAir ) THEN
         locName = 'Water-Vapor '
         IF ( useAIM ) THEN
           trUnits = 'g/kg'
         ELSE
           trUnits = 'kg/kg'
         ENDIF
       ELSEIF ( eosType.EQ.'TEOS10' ) THEN
c        locName = 'Absolute Salinity'
c        locName = 'Abs Salinity'
         locName = 'Salinity    '
         trUnits = 'g/kg'
       ELSE
         locName = 'Salinity    '
         trUnits = 'psu'
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

c      diagUnits = DIAGS_MK_UNITS( trUnits//'^2 '//flxUnits, myThid )
       IF ( trUnits(1:5).EQ.'kg/kg' ) THEN
C      to fit into diagUnits (16c), use a shorter trUnits:
         diagUnits = DIAGS_MK_UNITS( '(g/g)^2'//flxUnits, myThid )
       ELSE
         diagUnits = DIAGS_MK_UNITS(
     &                   '('//trUnits//')^2'//flxUnits, myThid )
       ENDIF
C-     Transport of tracer variance (splitted by direction):
       diagName  = 'VTRr'//diagSufx
       diagTitle = 'Vert. transport Flux of '//locName//'Variance'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'VTRx'//diagSufx
       diagTitle = 'Zonal transport Flux of '//locName//'Variance'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'VTRy'//diagSufx
       diagTitle = 'Merid transport Flux of '//locName//'Variance'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
C-     Numerical Production of variance (same units as variance Transport)
       diagName  = 'VPRr'//diagSufx
       diagTitle = locName//'Var Production by Vert. advection'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'VPRx'//diagSufx
       diagTitle = locName//'Var Production by Zonal advection'
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'VPRy'//diagSufx
       diagTitle = locName//'Var Production by Merid advection'
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
C-     Variance Production due to forcing (same units as variance Transport)
       diagName  = 'VPRf'//diagSufx
       diagTitle = locName//'Variance Production by Forcing'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I             diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C-     Residual in variance budget (same units as variance Transport)
       diagName  = 'VRes'//diagSufx
       diagTitle = locName//'Variance Budget Residual'
       diagCode  = 'SM      MR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I             diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C--- for now, add specific diags for vertical implicit part
       diagName  = 'VTRi'//diagSufx
       diagTitle = 'Impl.Vert. transport Flux of '//locName//'Variance'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
       diagName  = 'VPRi'//diagSufx
       diagTitle = locName//'Var Production by Impl.Vert. advection'
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C-     square of tracer difference in each direction (times face area):
       diagUnits = DIAGS_MK_UNITS( '('//trUnits//')^2.m^2', myThid )
C to do: need to fix units = trUnits ^2 x m^2    (if z-coords or vertical dir)
C                          = trUnits ^2 x m.Pa   (if p-coords & x,y dir)
       diagName  = 'SqDi'//diagSufx
       diagTitle = 'Square of Impl.Vert. gradient of '//locName
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )
C---
       diagName  = 'SqDr'//diagSufx
       diagTitle = 'Square of Vertical gradient of '//locName
       diagCode  = 'WM      LR      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, 0, myThid )

       diagUnits = DIAGS_MK_UNITS( '('//trUnits//')^2'//hvUnits,myThid )
       diagName  = 'SqDx'//diagSufx
       diagTitle = 'Square of Zonal    gradient of '//locName
       diagCode  = 'UU      MR      '
       diagMate  = diagNum + 2
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )
       diagName  = 'SqDy'//diagSufx
       diagTitle = 'Square of Meridio. gradient of '//locName
       diagCode  = 'VV      MR      '
       diagMate  = diagNum
       CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I      diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

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
