C $Header: /u/gcmpack/MITgcm/pkg/salt_plume/salt_plume_diagnostics_init.F,v 1.6 2014/05/21 10:46:03 heimbach Exp $
C $Name:  $

#include "SALT_PLUME_OPTIONS.h"

CBOP 0
C     !ROUTINE: SALT_PLUME_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE SALT_PLUME_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION:
C     Initialize list of all available diagnostics

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_SALT_PLUME
#ifdef ALLOW_DIAGNOSTICS

C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

      diagName  = 'PLUMEKB '
      diagTitle = 'fractional plume: [0-1] (unitless)'
      diagUnits = '                '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceSPtnd'
      diagTitle = 'salt tendency due to salt plume flux' 
     &          //' >0 increases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )


      diagName  = 'oceSPflx'
      diagTitle = 'net surface Salt flux rejected into the ocean during'
     &          //' freezing, (+=down),'
c    &          //' >0 increases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceSPDep'
      diagTitle = 'Salt plume depth based on density criterion (>0)'
      diagUnits = 'm               '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef SALT_PLUME_VOLUME
      diagName  = 'oceSPvol'
      diagTitle = ''
     &          //'surf brineVol associated w salt plume flux'
      diagUnits = 'm/s             '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
c
      diagName  = 'PLUMEKB1'
      diagTitle = 'fractional plume: [0-1] (unitless)'
      diagUnits = '                '
      diagCode  = 'SM      UR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'oceEPtnd'
      diagTitle = 'temperature tendency due to salt plume flux'
     &          //' >0 increases temperature'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
c
      diagName  = 'SPtd_TH '
      diagTitle = 'Energy tendency associated with salt plume flux'
     &          //' >0 increases temperature'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
cSPtd_SLT is a duplicate of oceSPtnd
      diagName  = 'SPtd_SLT'
      diagTitle = 'salt tendency associated with salt plume flux'
     &          //' >0 increases salinity'
      diagUnits = 'g/m^2/s         '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SPforcT1'
      diagTitle = ''
     &          //' range [1:Nr] '
      diagUnits = 'unitless        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SPforcS1'
      diagTitle = ''
     &          //' range [1:Nr] '
      diagUnits = 'unitless        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SPVsuf2k'
      diagTitle = 'salt plume volume flux from surface INTO lev k'
     &          //' >0 increases volume(k)'
      diagUnits = 'm/s             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SPVk2m  '
      diagTitle = 'vol flux LEAVING k to k-1 due to SPVsuf2k&SPVp2k'
     &          //' <0 decreases volume(k)'
      diagUnits = 'm/s             '
      diagCode  = 'SM      MR      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I       diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif /* SALT_PLUME_VOLUME */

#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SALT_PLUME */

      RETURN
      END
