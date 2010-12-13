C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_diagnostics_init.F,v 1.20 2010/12/13 13:54:44 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

C--  File seaice_diagnostics_init.F: Routines initialize SEAICE diagnostics
C--   Contents
C--   o SEAICE_DIAGNOSTICS_INIT
C--   o SEAICE_DIAG_SUFX

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SEAICE_DIAGNOSTICS_INIT
C     !INTERFACE:
      SUBROUTINE SEAICE_DIAGNOSTICS_INIT( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_DIAGNOSTICS_INIT
C     | o Routine to initialize list of all available diagnostics
C     |   for SEAICE package
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SEAICE_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     !LOCAL VARIABLES:
C     === Local variables ===
C     diagNum   :: diagnostics number in the (long) list of available diag.
C     diagMate  :: diag. mate number in the (long) list of available diag.
C     diagName  :: local short name (8c) of a diagnostics
C     diagCode  :: local parser field with characteristics of the diagnostics
C              cf head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

      INTEGER       numArea
      CHARACTER*9   flxUnits
      CHARACTER*15  locName
      CHARACTER*4 SEAICE_DIAG_SUFX, diagSufx
c     EXTERNAL    SEAICE_DIAG_SUFX

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      diagName  = 'SIarea  '
      diagTitle = 'SEAICE fractional ice-covered area [0 to 1]'
      diagUnits = 'm^2/m^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      numArea  = diagNum

      diagName  = 'SIheff  '
      diagTitle = 'SEAICE effective ice thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIuice  '
      diagTitle = 'SEAICE zonal ice velocity, >0 from West to East'
      diagUnits = 'm/s             '
#ifdef SEAICE_CGRID
      diagCode  = 'UU      M1      '
#else
      diagCode  = 'UZ      M1      '
#endif
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIvice  '
      diagTitle = 'SEAICE merid. ice velocity, >0 from South to North'
      diagUnits = 'm/s             '
#ifdef SEAICE_CGRID
      diagCode  = 'VV      M1      '
#else
      diagCode  = 'VZ      M1      '
#endif
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIhsnow '
      diagTitle = 'SEAICE snow thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIhsalt '
      diagTitle = 'SEAICE effective salinity'
      diagUnits = 'g/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIage   '
      diagTitle = 'SEAICE age'
      diagUnits = 's               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SItices '
      diagTitle = 'Surface Temperature over Sea-Ice (area weighted)'
      diagUnits = 'K               '
      diagCode  = 'SM  C   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numArea, myThid )

C     SIqnet, Qnet, and QNETtave are identical.
C     With #undef NONLIN_FRSURF SIqnet is identical to -(TFLUX-TRELAX).
C     Except over land and under sea ice, SIqnet is also identical to
C     EXFlwnet+EXFswnet-EXFhl-EXFhs.
      diagName  = 'SIqnet  '
      diagTitle = 'Ocean surface heatflux, turb+rad, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C     SIqsw, Qsw, and QSWtave are identical.
C     Except under sea ice, SIqsw is also identical to EXFswnet.
      diagName  = 'SIqsw   '
      diagTitle = 'Ocean surface shortwave radiat., >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIatmQnt'
      diagTitle = 'Net atmospheric heat flux, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIqneto '
      diagTitle = 'Heat flux over ocean, turb+rad, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIqneti '
      diagTitle = 'Heat flux under ice, turb+rad, >0 decreases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C     pkg/diagnostics SIempmr, dumpfreq EmPmR, and tavefreq EmPmRtave
C     are identical but they differ from pkg/diagnostics EXFempmr, which
C     is EmPmR before impact of ice.
      diagName  = 'SIempmr '
      diagTitle = 'Ocean surface freshwater flux, > 0 increases salt'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIatmFW '
      diagTitle = 'Net freshwater flux from atmosphere & land (+=down)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIsnPrcp'
      diagTitle = 'Snow precip. (+=dw) over Sea-Ice (area weighted)'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIyneg  '
      diagTitle = 'Ice growth due to oceanic heat flux , >0 melts ice'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIfice  '
      diagTitle = 'Ice growth due to atm heat flux, >0 creates ice'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIuwind '
      diagTitle = 'SEAICE zonal 10-m wind speed, >0 increases uVel'
      diagUnits = 'm/s             '
      diagCode  = 'UM      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIvwind '
      diagTitle = 'SEAICE meridional 10-m wind speed, >0 increases uVel'
      diagUnits = 'm/s             '
      diagCode  = 'VM      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     pkg/diagnostics SIfu and oceTAUX, dumpfreq FU, and tavefreq FUtave
C     are identical but they differ from pkg/diagnostics EXFtaux, which
C     is stress before impact of ice.  Also when using exf bulk
C     formulae, EXFtaux is defined on tracer rather than uvel points.
      diagName  = 'SIfu    '
      diagTitle = 'SEAICE zonal surface wind stress, >0 increases uVel '
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     pkg/diagnostics SIfv and oceTAUY, dumpfreq FV, and tavefreq FVtave
C     are identical but they differ from pkg/diagnostics EXFtauy, which
C     is stress before impact of ice.  Also when using exf bulk
C     formulae, EXFtauy is defined on tracer rather than vvel points.
      diagName  = 'SIfv    '
      diagTitle = 'SEAICE merid. surface wind stress, >0 increases vVel'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIpress   '
      diagTitle = 'SEAICE strength (with upper and lower limit)'
      diagUnits = 'm^2/s^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIzeta   '
      diagTitle = 'SEAICE nonlinear bulk viscosity'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIeta    '
      diagTitle = 'SEAICE nonlinear shear viscosity'
      diagUnits = 'm^2/s           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIsigI  '
      diagTitle = 'SEAICE normalized principle stress, component one'
      diagUnits = 'no units        '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIsigII '
      diagTitle = 'SEAICE normalized principle stress, component two'
      diagUnits = 'no units        '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

CML      diagName  = 'SIttendh'
CML      diagTitle = 'SEAICE tendency of effective ice thickness'
CML      diagUnits = 'm/s             '
CML      diagCode  = 'SM      M1      '
CML      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
CML     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIthdgrh'
      diagTitle = 'SEAICE thermodynamic growth rate of '//
     &     'effective ice thickness'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIsnwice'
      diagTitle = 'SEAICE ice formation rate due to flooding'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIuheff '
      diagTitle = 'Zonal Transport of effective ice thickness'
      diagUnits = 'm^2/s           '
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIvheff '
      diagTitle = 'Meridional Transport of effective ice thickness'
      diagUnits = 'm^2/s           '
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     advective and diffusive fluxes
C     effective thickness
      flxUnits = '.m^2/s   '
      locName = 'eff ice thickn '
      WRITE(diagUnits,'(2A)') 'm',flxUnits
      diagSufx = SEAICE_DIAG_SUFX( GAD_HEFF, myThid )

C--   advective flux
      diagName  = 'ADVx'//diagSufx
      diagTitle = 'Zonal      Advective Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'ADVy'//diagSufx
      diagTitle = 'Meridional Advective Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C--   Diffusive flux:
      diagName  = 'DFxE'//diagSufx
      diagTitle = 'Zonal      Diffusive Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DFyE'//diagSufx
      diagTitle = 'Meridional Diffusive Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     fractional ice covered area (ice concentration)
      locName = 'fract area     '
      WRITE(diagUnits,'(2A)') 'm^2/m^2',flxUnits
      diagSufx = SEAICE_DIAG_SUFX( GAD_AREA, myThid )

C--   advective flux
      diagName  = 'ADVx'//diagSufx
      diagTitle = 'Zonal      Advective Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'ADVy'//diagSufx
      diagTitle = 'Meridional Advective Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C--   Diffusive flux:
      diagName  = 'DFxE'//diagSufx
      diagTitle = 'Zonal      Diffusive Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DFyE'//diagSufx
      diagTitle = 'Meridional Diffusive Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     effective snow thickness
      locName = 'eff snow thickn'
      WRITE(diagUnits,'(2A)') 'm',flxUnits
      diagSufx = SEAICE_DIAG_SUFX( GAD_SNOW, myThid )

C--   advective flux
      diagName  = 'ADVx'//diagSufx
      diagTitle = 'Zonal      Advective Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'ADVy'//diagSufx
      diagTitle = 'Meridional Advective Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C--   Diffusive flux:
      diagName  = 'DFxE'//diagSufx
      diagTitle = 'Zonal      Diffusive Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DFyE'//diagSufx
      diagTitle = 'Meridional Diffusive Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     sea ice salinity
      locName = 'seaice salinity'
      WRITE(diagUnits,'(2A)') 'psu',flxUnits
      diagSufx = SEAICE_DIAG_SUFX( GAD_SALT, myThid )

C--   advective flux
      diagName  = 'ADVx'//diagSufx
      diagTitle = 'Zonal      Advective Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'ADVy'//diagSufx
      diagTitle = 'Meridional Advective Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C--   Diffusive flux:
      diagName  = 'DFxE'//diagSufx
      diagTitle = 'Zonal      Diffusive Flux of '//locName
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'DFyE'//diagSufx
      diagTitle = 'Meridional Diffusive Flux of '//locName
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: SEAICE_DIAG_SUFX

C     !INTERFACE:
      CHARACTER*4 FUNCTION SEAICE_DIAG_SUFX( tracerId, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | FUNCTION SEAICE_DIAG_SUFX
C     | o Return diagnostic suffix (4 character long) for the
C     |   "tracerId" tracer (used to build diagnostic names).
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SEAICE_PARAMS.h"

C     !INPUT PARAMETERS:
C     tracerId   ::  tracer identifier
C     myThid     ::  my thread Id number
      INTEGER      tracerId
      INTEGER      myThid
CEOP

C     !LOCAL VARIABLES:

C--   Set diagnostic suffix (4 character long) for the "tracerId" tracer
      IF ( tracerId.EQ.GAD_HEFF ) THEN
        SEAICE_DIAG_SUFX = 'HEFF'
      ELSEIF( tracerId.EQ.GAD_AREA ) THEN
        SEAICE_DIAG_SUFX = 'AREA'
      ELSEIF( tracerId.EQ.GAD_SNOW ) THEN
        SEAICE_DIAG_SUFX = 'SNOW'
      ELSEIF( tracerId.EQ.GAD_SALT ) THEN
        SEAICE_DIAG_SUFX = 'SSLT'
      ELSE
        SEAICE_DIAG_SUFX = 'aaaa'
      ENDIF

      RETURN
      END
