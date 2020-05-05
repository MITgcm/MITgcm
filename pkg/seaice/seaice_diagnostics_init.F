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
#include "SIZE.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_TRACER.h"

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
C                   see head of S/R DIAGNOSTICS_INIT_EARLY or DIAGNOSTICS_MAIN_INIT
C                   for a list of options
C     diagUnits :: local string (16c): physical units of a diagnostic field
C     diagTitle :: local string (80c): description of field in diagnostic
      INTEGER       diagNum
      INTEGER       diagMate
      CHARACTER*8   diagName
      CHARACTER*16  diagCode
      CHARACTER*16  diagUnits
      CHARACTER*(80) diagTitle

#ifdef ALLOW_SITRACER
      INTEGER       iTr, ilnb, numMateTr, numMateTrPreTh
      CHARACTER*8   locUnitTr
      CHARACTER*30  locNameTr
#endif
      INTEGER       numArea,numAreaPreTh,numHeff,numHeffPreTh
      CHARACTER*9   flxUnits
      CHARACTER*15  locName
      CHARACTER*4 SEAICE_DIAG_SUFX, diagSufx
      EXTERNAL    SEAICE_DIAG_SUFX
C     Functions ::
      INTEGER     ILNBLNK
      EXTERNAL    ILNBLNK

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C=============== state variables ============

      diagName  = 'SIarea  '
      diagTitle = 'SEAICE fractional ice-covered area [0 to 1]'
      diagUnits = 'm^2/m^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      numArea  = diagNum

      diagName  = 'SIareaPR'
      diagTitle = 'SIarea preceeding ridging process'
      diagUnits = 'm^2/m^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIareaPT'
      diagTitle = 'SIarea preceeding thermodynamic growth/melt'
      diagUnits = 'm^2/m^2         '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      numAreaPreTh  = diagNum

      diagName  = 'SIheff  '
      diagTitle = 'SEAICE effective ice thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      numHeff  = diagNum

      diagName  = 'SIheffPT'
      diagTitle = 'SIheff preceeeding thermodynamic growth/melt'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      numHeffPreTh  = diagNum

      diagName  = 'SIhsnow '
      diagTitle = 'SEAICE effective snow thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIhsnoPT'
      diagTitle = 'SIhsnow preceeeding thermodynamic growth/melt'
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

      diagName  = 'SIenpa  '
      diagTitle = 'SEAICE power input by surface wind stresses'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIenpw  '
      diagTitle = 'SEAICE work done by ocean drag'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIenph  '
      diagTitle = 'SEAICE power due to change in inertia'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIenpg  '
      diagTitle = 'SEAICE work generated by gravity'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIenpi  '
      diagTitle = 'SEAICE work done by internal stress'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIenpot '
      diagTitle = 'SEAICE work done to change potential energy'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIpRfric'
      diagTitle = 'SEAICE dissipation in ridging by internal stresses'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIpSfric'
      diagTitle = 'SEAICE dissipation in shearing by internal stresses'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )


#ifdef ALLOW_SITRACER
      DO iTr = 1, SItrNumInUse

C--    Set default name & tracer Units:
       WRITE(locNameTr,'(A,I4.4,A)') 'sea ice tracer no. ',iTr
       if (SItrMate(iTr).EQ.'HEFF') then
         locUnitTr  = '(kg/kg) '
         numMateTr  = numHeff
         numMateTrPreTh = numHeffPreTh
       else
         locUnitTr  = '(kg/m^2)'
         numMateTr  = numArea
         numMateTrPreTh = numAreaPreTh
       endif
C-     use name & units from data.seaice :
       ilnb = ILNBLNK(SItrUnit(iTr))
       IF ( ilnb.GE.1 ) THEN
         ilnb = LEN(locUnitTr)
         locUnitTr = SItrUnit(iTr)(1:ilnb)
       ENDIF
       ilnb = ILNBLNK(SItrNameLong(iTr))
       IF ( ilnb.GE.1 ) THEN
         ilnb = MIN(LEN(locNameTr),ilnb)
         WRITE(locNameTr,'(A)') SItrNameLong(iTr)(1:ilnb)
       ELSE
         ilnb = ILNBLNK(SItrName(iTr))
         IF ( ilnb.GE.1 ) THEN
           ilnb = MIN(LEN(locNameTr),ilnb)
           WRITE(locNameTr,'(2A)') SItrName(iTr)(1:ilnb),' tracer'
         ENDIF
       ENDIF
       ilnb = MAX(ILNBLNK(locNameTr),1)

       WRITE(diagName,'(A4,I2.2,A2)') 'SItr',iTr,'  '
       WRITE(diagTitle,'(4A)') locNameTr(1:ilnb),
     &       ' (associated with ',SItrMate(iTr),')'
       diagUnits = locUnitTr//'        '
       diagCode  = 'SM  C   M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum, diagName,
     I      diagCode, diagUnits, diagTitle, numMateTr, myThid )

       WRITE(diagName,'(A4,I2.2,A2)') 'SItr',iTr,'PT'
       WRITE(diagTitle,'(A4,I2.2,2A)') 'SItr',iTr,
     &     ' preceeeding thermodynamic growth/melt'
       diagUnits = locUnitTr//'        '
       diagCode  = 'SM  C   M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum, diagName,
     I      diagCode, diagUnits, diagTitle, numMateTrPreTh, myThid )

      ENDDO
#endif

      diagName  = 'SItices '
      diagTitle = 'Surface Temperature over Sea-Ice (area weighted)'
      diagUnits = 'K               '
      diagCode  = 'SM  C   M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, numArea, myThid )

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

C=============== momentum, heat and fresh water forcing ============

C     pkg/diagnostics SIfu and oceTAUX, dumpfreq FU, and tavefreq FUtave
C     are identical but they differ from pkg/diagnostics EXFtaux, which
C     is stress before impact of ice.  Also when using exf bulk
C     formulae, EXFtaux is defined on tracer rather than uvel points.
c     diagName  = 'SIfu    '
c     diagTitle = 'SEAICE zonal surface wind stress, >0 increases uVel '
c     diagUnits = 'N/m^2           '
c     diagCode  = 'UU      U1      '
c     diagMate  = diagNum + 2
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C     pkg/diagnostics SIfv and oceTAUY, dumpfreq FV, and tavefreq FVtave
C     are identical but they differ from pkg/diagnostics EXFtauy, which
C     is stress before impact of ice.  Also when using exf bulk
C     formulae, EXFtauy is defined on tracer rather than vvel points.
c     diagName  = 'SIfv    '
c     diagTitle = 'SEAICE merid. surface wind stress, >0 increases vVel'
c     diagUnits = 'N/m^2           '
c     diagCode  = 'VV      U1      '
c     diagMate  = diagNum
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SItaux  '
      diagTitle = 'SEAICE zonal surface wind stress, >0 increases uIce'
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SItauy  '
      diagTitle = 'SEAICE merid surface wind stress, >0 increases vIce'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIatmTx '
      diagTitle = 'Zonal surface wind stress over Ocean+SeaIce'
      diagUnits = 'N/m^2           '
      diagCode  = 'UU      U1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIatmTy '
      diagTitle = 'Merid surface wind stress over Ocean+SeaIce'
      diagUnits = 'N/m^2           '
      diagCode  = 'VV      U1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

c     diagName  = 'SIuwind '
c     diagTitle = 'SEAICE zonal 10-m wind speed, >0 increases uVel'
c     diagUnits = 'm/s             '
c     diagCode  = 'UM      U1      '
c     diagMate  = diagNum + 2
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

c     diagName  = 'SIvwind '
c     diagTitle = 'SEAICE meridional 10-m wind speed, >0 increases uVel'
c     diagUnits = 'm/s             '
c     diagCode  = 'VM      U1      '
c     diagMate  = diagNum
c     CALL DIAGNOSTICS_ADDTOLIST( diagNum,
c    I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

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

      diagName  = 'SItflux '
      diagTitle = 'Same as TFLUX but incl seaice (>0 incr T decr H)'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifndef SEAICE_DISABLE_HEATCONSFIX
      diagName  = 'SIaaflux'
      diagTitle = 'conservative ocn<->seaice adv. heat flux adjust.'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

      diagName  = 'SIhl    '
      diagTitle = 'Latent heat flux into ocean, >0 increases theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIqneto '
      diagTitle = 'Open Ocean Part of SIqnet, turb+rad, >0 decr theta'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIqneti '
      diagTitle = 'Ice Covered Part of SIqnet, turb+rad, >0 decr theta'
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

      diagName  = 'SIfwSubl'
      diagTitle ='Potential sublimation freshwater flux, >0 decr. ice'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIacSubl'
      diagTitle = 'Actual sublimation freshwater flux, >0 decr. ice'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIrsSubl'
      diagTitle = 'Residual subl. freshwater flux, >0 taken from ocn'
      diagUnits = 'kg/m^2/s        '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIactLHF'
      diagTitle = 'Actual latent heat flux over ice'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SImaxLHF'
      diagTitle = 'Maximum latent heat flux over ice'
      diagUnits = 'W/m^2           '
      diagCode  = 'SM      U1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef ALLOW_SITRACER
      DO iTr = 1, SItrNumInUse
       IF (SItrMate(iTr).EQ.'HEFF') then
C--    Set default name & tracer Units:
       WRITE(diagUnits,'(A)') 'kg/m^2/s'
C--     use units from data.seaice :
       ilnb = ILNBLNK(SItrUnit(iTr))
       IF ( ilnb.GE.1 ) THEN
         WRITE(diagUnits,'(2A)') SItrUnit(iTr)(1:ilnb),'.kg/m^2/s'
       ENDIF
C--
       WRITE(diagName,'(A4,I2.2,A2)') 'SItr',iTr,'Fx'
       WRITE(diagTitle,'(A4,I2.2,A)') 'SItr',iTr,
     I      ' flux out of ice pack (that may enter ocean)'
       diagCode  = 'SM      M1      '
       CALL DIAGNOSTICS_ADDTOLIST( diagNum, diagName,
     I      diagCode, diagUnits, diagTitle, 0, myThid )

      ENDIF
      ENDDO
#endif

C============== ice growth/melt ==============

      diagName  = 'SIaQbOCN'
      diagTitle = 'Potential HEFF rate of change by ocean ice flux'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIaQbATC'
      diagTitle = 'Potential HEFF rate of change by atm flux over ice'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIaQbATO'
      diagTitle = 'Potential HEFF rate of change by open ocn atm flux'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdHbOCN'
      diagTitle = 'HEFF rate of change by ocean ice flux'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdSbATC'
      diagTitle = 'HSNOW rate of change by atm flux over sea ice'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdSbOCN'
      diagTitle = 'HSNOW rate of change by ocean ice flux'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdHbATC'
      diagTitle = 'HEFF rate of change by atm flux over sea ice'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdHbATO'
      diagTitle = 'HEFF rate of change by open ocn atm flux'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdHbFLO'
      diagTitle = 'HEFF rate of change by flooding snow'
      diagUnits = 'm/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#ifdef SEAICE_GREASE

      diagName  = 'SIgrsLT '
      diagTitle = 'actual grease ice layer thickness'
      diagUnits = 'm               '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
#endif

C=============== expansion/contraction ============

      diagName  = 'SIdAbATO'
      diagTitle = 'Potential AREA rate of change by open ocn atm flux'
      diagUnits = 'm^2/m^2/s       '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdAbATC'
      diagTitle = 'Potential AREA rate of change by atm flux over ice'
      diagUnits = 'm^2/m^2/s       '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdAbOCN'
      diagTitle = 'Potential AREA rate of change by ocean ice flux'
      diagUnits = 'm^2/m^2/s       '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdA'
      diagTitle = 'AREA rate of change (net)'
      diagUnits = 'm^2/m^2/s       '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

C============== advection/diffusion ============

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

C--   effective thickness transport (centered in space, 1 time-step lag)
      diagName  = 'SIuheff '
      diagTitle = 'Zonal      Transport of eff ice thickn (centered)'
      diagUnits = 'm^2/s           '
      diagCode  = 'UU      M1      '
      diagMate  = diagNum + 2
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

      diagName  = 'SIvheff '
      diagTitle = 'Meridional Transport of eff ice thickn (centered)'
      diagUnits = 'm^2/s           '
      diagCode  = 'VV      M1      '
      diagMate  = diagNum
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, diagMate, myThid )

C=============== dynamics ============

      diagName  = 'SIpress '
      diagTitle = 'SEAICE strength (with upper and lower limit)'
      diagUnits = 'N/m             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIzeta  '
      diagTitle = 'SEAICE nonlinear bulk viscosity'
      diagUnits = 'kg/s            '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIeta   '
      diagTitle = 'SEAICE nonlinear shear viscosity'
      diagUnits = 'kg/s            '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIsig1  '
      diagTitle = 'SEAICE normalized principle stress, component one'
      diagUnits = 'no units        '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIsig2  '
      diagTitle = 'SEAICE normalized principle stress, component two'
      diagUnits = 'no units        '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIshear '
      diagTitle = 'SEAICE shear deformation rate'
      diagUnits = '1/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SIdelta '
      diagTitle = 'SEAICE Delta deformation rate'
      diagUnits = '1/s             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )

      diagName  = 'SItensil'
      diagTitle = 'SEAICE maximal tensile strength'
      diagUnits = 'N/m             '
      diagCode  = 'SM      M1      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I    diagName, diagCode, diagUnits, diagTitle, 0, myThid )

#ifdef SEAICE_ITD
C=============== ice thickness categories ============

      diagName  = 'SIheffN '
      diagTitle = 'SEAICE effective ice thickness per category'
      diagUnits = 'm               '
      diagCode  = 'SM      MX      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, nITD, myThid )

      diagName  = 'SIareaN '
      diagTitle =
     I 'SEAICE fractional ice-covered area per category [0 to 1]'
      diagUnits = 'm^2/m^2         '
      diagCode  = 'SM      MX      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, nITD, myThid )

      diagName  = 'SIhsnowN'
      diagTitle = 'SEAICE effective snow thickness per category'
      diagUnits = 'm               '
      diagCode  = 'SM      MX      '
      CALL DIAGNOSTICS_ADDTOLIST( diagNum,
     I     diagName, diagCode, diagUnits, diagTitle, 0, myThid )
      CALL DIAGNOSTICS_SETKLEV( diagName, nITD, myThid )
#endif

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
#include "SEAICE_SIZE.h"
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
