#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_CHECK
C     !INTERFACE:
      SUBROUTINE SEAICE_CHECK( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SEAICE_CHECK
C     | o Validate basic package setup and inter-package
C     |   dependencies.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_PARAM.h"
#endif
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif /* ALLOW_GENERIC_ADVDIFF */
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid     :: my Thread Id. number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     ioUnit     :: temp for writing msg unit
C     msgBuf     :: Informational/error message buffer
C     errCount   :: error counter
      INTEGER ioUnit
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      LOGICAL checkAdvSchArea, checkAdvSchHeff, checkAdvSchSnow
      LOGICAL checkAdvSchSalt
#ifdef ALLOW_SITRACER
      INTEGER iTracer
#endif
      _RL SEAICE_mcphee_max
      INTEGER  kSrf
      INTEGER  i
      INTEGER  errCount
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( usingPCoords ) THEN
       kSrf        = Nr
      ELSE
       kSrf        = 1
      ENDIF
      ioUnit = errorMessageUnit

      _BEGIN_MASTER(myThid)

C--   ALLOW_SEAICE
      WRITE(msgBuf,'(A)') 'SEAICE_CHECK: #define ALLOW_SEAICE'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

      errCount = 0

C--   SEAICE needs forcing_In_AB FALSE
      IF (tracForcingOutAB.NE.1) THEN
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &         ' Need T,S forcing out of AB (tracForcingOutAB=1)'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

C-------------------------------------------------
C--   Check seaice thermodynamics setting:
      IF ( usePW79thermodynamics ) THEN

C--   check ice cover fraction formula
       IF ((SEAICE_areaGainFormula.LT.1).OR.
     &     (SEAICE_areaGainFormula.GT.2)) THEN
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &         ' SEAICE_areaGainFormula must be between 1 and 2'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
       ENDIF
       IF ((SEAICE_areaLossFormula.LT.1).OR.
     &     (SEAICE_areaLossFormula.GT.3)) THEN
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &         ' SEAICE_areaLossFormula must be between 1 and 2'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
       ENDIF

       IF ( (.NOT.SEAICE_doOpenWaterGrowth)
     &   .AND.( (SEAICE_areaGainFormula.NE.2).OR.
     &          (SEAICE_areaLossFormula.NE.3) ) ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'when SEAICE_doOpenWaterGrowth is false, you need to set'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'SEAICE_areaGainFormula.EQ.2 and SEAICE_areaLossFormula.EQ.3'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

C--   check concistency of turbulent flux term etc. specification

       SEAICE_mcphee_max=drF(kSrf)/SEAICE_deltaTtherm
       IF ( usingPCoords ) SEAICE_mcphee_max
     &      = SEAICE_mcphee_max * recip_rhoConst * recip_gravity
       IF ( SEAICE_mcPheePiston .LT. 0. _d 0 .OR.
     &      SEAICE_mcPheePiston .GT. SEAICE_mcphee_max ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &   ' SEAICE_mcPheePiston is out of bounds.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &   ' They must lie within 0. and drF(1)/SEAICE_deltaTtherm'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

       IF ( ( SEAICE_frazilFrac .LT. 0. _d 0 ) .OR.
     &      ( SEAICE_frazilFrac .GT. 1. _d 0 ) ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &   ' SEAICE_frazilFrac is out of bounds.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &   ' They must lie within 0. and 1. '
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

       IF ( ( SEAICE_mcPheeTaper .LT. 0. _d 0 ) .OR.
     &      ( SEAICE_mcPheeTaper .GT. 1. _d 0 ) ) THEN
        WRITE(msgBuf,'(2A)')
     &      'SEAICE_mcPheeTaper cannot be specified  ',
     &      'outside of the [0. 1.] range'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

       IF ( SEAICE_doOpenWaterMelt .AND.
     &      (.NOT.SEAICE_doOpenWaterGrowth) ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'to use SEAICE_doOpenWaterMelt, you need to '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'also set SEAICE_doOpenWaterGrowth to .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF

C     check albedos
       IF ( SEAICE_dryIceAlb .GT. SEAICE_drySnowAlb ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &       'SEAICE_dryIceAlb > SEAICE_drySnowAlb makes little sense'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: and leads ',
     &       'to albedo = SEAICE_drySnowAlb for all snow thicknesses'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( SEAICE_wetIceAlb .GT. SEAICE_wetSnowAlb ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &       'SEAICE_wetIceAlb > SEAICE_wetSnowAlb makes little sense'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: and leads ',
     &       'to albedo = SEAICE_wetSnowAlb for all snow thicknesses'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( SEAICE_dryIceAlb_south .GT. SEAICE_drySnowAlb_south ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** SEAICE_CHECK: ',
     &       'SEAICE_dryIceAlb_south > SEAICE_drySnowAlb_south ',
     &       'makes little sense'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: and leads ',
     &  'to albedo = SEAICE_drySnowAlb_south for all snow thicknesses'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( SEAICE_wetIceAlb_south .GT. SEAICE_wetSnowAlb_south ) THEN
        WRITE(msgBuf,'(3A)') '** WARNING ** SEAICE_CHECK: ',
     &       'SEAICE_wetIceAlb_south > SEAICE_wetSnowAlb_south ',
     &       'makes little sense'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: and leads ',
     &  'to albedo = SEAICE_wetSnowAlb_south for all snow thicknesses'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF

C--   end if usePW79thermodynamics
      ENDIF
C--   Checking seaice thermodynamics setting: end
C-------------------------------------------------

C--   check specifications of new features for testing

#ifdef ALLOW_BALANCE_FLUXES
      IF ( usePW79thermodynamics .AND. selectBalanceEmPmR.EQ.2 ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'selectBalanceEmPmR=2 not yet implemented in pkg/seaice'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifdef SEAICE_DISABLE_HEATCONSFIX
      IF ( SEAICEheatConsFix ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'to use SEAICEheatConsFix, you need to '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'undef SEAICE_DISABLE_HEATCONSFIX and recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifndef ALLOW_SITRACER
      IF ( SEAICE_salinityTracer ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'to use SEAICE_salinityTracer, you need to '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'define ALLOW_SITRACER and recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

      IF ( SEAICE_ageTracer ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'to use SEAICE_ageTracer, you need to '
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &   'define ALLOW_SITRACER and recompile'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

C--   check SItracer specifications
#ifdef ALLOW_SITRACER

c to be added : if SEAICE_salinityTracer we need one tracer doing that
c to be added : if SEAICE_ageTracer we suggest that one tracer does that

      DO iTracer = 1, SItrNumInUse

      IF ( ( SItrFromOceanFrac(iTracer) .LT. 0. _d 0 ) .OR.
     &     ( SItrFromOceanFrac(iTracer) .GT. 1. _d 0 ) ) THEN
         WRITE(msgBuf,'(2A)')
     &      'SItrFromOceanFrac cannot be specified  ',
     &      'outside of the [0. 1.] range'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

      IF ( ( SItrFromFloodFrac(iTracer) .LT. 0. _d 0 ) .OR.
     &     ( SItrFromFloodFrac(iTracer) .GT. 1. _d 0 ) ) THEN
         WRITE(msgBuf,'(2A)')
     &      'SItrFromFloodFrac cannot be specified  ',
     &      'outside of the [0. 1.] range'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

c      IF ( (SItrName(iTracer).EQ.'salinity') .AND.
c     &     (SItrMate(iTracer).NE.'HEFF') ) THEN
c         WRITE(msgBuf,'(2A)')
c     &      'SItrName = "salinity" requires  ',
c     &      'SItrMate = "HEFF" '
c         CALL PRINT_ERROR( msgBuf, myThid )
c         errCount = errCount + 1
c      ENDIF

      IF ( (SItrName(iTracer).NE.'salinity').AND.
     &     ( (SItrFromOceanFrac(iTracer).NE.ZERO).OR.
     &       (SItrFromFloodFrac(iTracer).NE.ZERO) ) ) THEN
         WRITE(msgBuf,'(2A)')
     &      'SItrFromOceanFrac / SItrFromFloodFrac is only  ',
     &      'available for SItrName = "salinity" (for now)'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

      ENDDO
#endif

C--   Check advection schemes
#ifdef ALLOW_GENERIC_ADVDIFF
      checkAdvSchArea = SEAICEadvArea .AND. (
     &     SEAICEadvSchArea.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchArea.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchArea.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchArea.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchArea.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchArea.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchArea.NE.ENUM_PPM_NULL_LIMIT  .AND.
     &     SEAICEadvSchArea.NE.ENUM_PPM_MONO_LIMIT  .AND.
     &     SEAICEadvSchArea.NE.ENUM_PPM_WENO_LIMIT  .AND.
     &     SEAICEadvSchArea.NE.ENUM_PQM_NULL_LIMIT  .AND.
     &     SEAICEadvSchArea.NE.ENUM_PQM_MONO_LIMIT  .AND.
     &     SEAICEadvSchArea.NE.ENUM_PQM_WENO_LIMIT  .AND.
     &     SEAICEadvSchArea.NE.ENUM_OS7MP )
      checkAdvSchHEFF = SEAICEadvHeff .AND. (
     &     SEAICEadvSchHeff.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchHeff.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchHeff.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchHeff.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchHeff.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchHeff.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchHeff.NE.ENUM_PPM_NULL_LIMIT  .AND.
     &     SEAICEadvSchHeff.NE.ENUM_PPM_MONO_LIMIT  .AND.
     &     SEAICEadvSchHeff.NE.ENUM_PPM_WENO_LIMIT  .AND.
     &     SEAICEadvSchHeff.NE.ENUM_PQM_NULL_LIMIT  .AND.
     &     SEAICEadvSchHeff.NE.ENUM_PQM_MONO_LIMIT  .AND.
     &     SEAICEadvSchHeff.NE.ENUM_PQM_WENO_LIMIT  .AND.
     &     SEAICEadvSchHeff.NE.ENUM_OS7MP )
      checkAdvSchSnow = SEAICEadvSnow .AND. (
     &     SEAICEadvSchSnow.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchSnow.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchSnow.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchSnow.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchSnow.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchSnow.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchSnow.NE.ENUM_PPM_NULL_LIMIT  .AND.
     &     SEAICEadvSchSnow.NE.ENUM_PPM_MONO_LIMIT  .AND.
     &     SEAICEadvSchSnow.NE.ENUM_PPM_WENO_LIMIT  .AND.
     &     SEAICEadvSchSnow.NE.ENUM_PQM_NULL_LIMIT  .AND.
     &     SEAICEadvSchSnow.NE.ENUM_PQM_MONO_LIMIT  .AND.
     &     SEAICEadvSchSnow.NE.ENUM_PQM_WENO_LIMIT  .AND.
     &     SEAICEadvSchSnow.NE.ENUM_OS7MP )
      checkAdvSchSalt = SEAICEadvSalt .AND. (
     &     SEAICEadvSchSalt.NE.ENUM_UPWIND_1RST .AND.
     &     SEAICEadvSchSalt.NE.ENUM_CENTERED_2ND .AND.
     &     SEAICEadvSchSalt.NE.ENUM_DST2 .AND.
     &     SEAICEadvSchSalt.NE.ENUM_FLUX_LIMIT .AND.
     &     SEAICEadvSchSalt.NE.ENUM_DST3 .AND.
     &     SEAICEadvSchSalt.NE.ENUM_DST3_FLUX_LIMIT .AND.
     &     SEAICEadvSchSalt.NE.ENUM_PPM_NULL_LIMIT  .AND.
     &     SEAICEadvSchSalt.NE.ENUM_PPM_MONO_LIMIT  .AND.
     &     SEAICEadvSchSalt.NE.ENUM_PPM_WENO_LIMIT  .AND.
     &     SEAICEadvSchSalt.NE.ENUM_PQM_NULL_LIMIT  .AND.
     &     SEAICEadvSchSalt.NE.ENUM_PQM_MONO_LIMIT  .AND.
     &     SEAICEadvSchSalt.NE.ENUM_PQM_WENO_LIMIT  .AND.
     &     SEAICEadvSchSalt.NE.ENUM_OS7MP )
      IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR.
     &     checkAdvSchSnow .OR. checkAdvSchSalt ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: allowed advection schemes',
     &          ' for heff, area, snow, and salt are: '
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,13I3)') 'SEAICE_CHECK:',
     &      ENUM_UPWIND_1RST, ENUM_CENTERED_2ND, ENUM_DST2,
     &      ENUM_FLUX_LIMIT, ENUM_DST3, ENUM_DST3_FLUX_LIMIT,
     &      ENUM_PPM_NULL_LIMIT,ENUM_PPM_MONO_LIMIT,ENUM_PPM_WENO_LIMIT,
     &      ENUM_PQM_NULL_LIMIT,ENUM_PQM_MONO_LIMIT,ENUM_PQM_WENO_LIMIT,
     &      ENUM_OS7MP
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &      ' the following Adv.Schemes are not allowed:'
       CALL PRINT_ERROR( msgBuf, myThid )
       IF ( checkAdvSchArea ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchArea = ',  SEAICEadvSchArea
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( checkAdvSchHeff ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchHeff = ',  SEAICEadvSchHeff
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( checkAdvSchSnow ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchSnow = ',  SEAICEadvSchSnow
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       IF ( checkAdvSchSalt ) THEN
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &       ' SEAICEadvSchSalt = ',  SEAICEadvSchSalt
        CALL PRINT_ERROR( msgBuf, myThid )
       ENDIF
       errCount = errCount + 1
      ENDIF
      IF ( SEAICEadvScheme.EQ.ENUM_CENTERED_2ND ) THEN
C-- for now, the code does not allow to use the default advection scheme
C    (Centered 2nd order) for 1 ice-field and an other advection scheme
C    for an other ice-field. In this case, stop here.
        checkAdvSchArea = SEAICEadvArea .AND.
     &     SEAICEadvSchArea.NE.ENUM_CENTERED_2ND
        checkAdvSchHEFF = SEAICEadvHeff .AND.
     &     SEAICEadvSchHeff.NE.ENUM_CENTERED_2ND
        checkAdvSchSnow = SEAICEadvSnow .AND.
     &     SEAICEadvSchSnow.NE.ENUM_CENTERED_2ND
        checkAdvSchSalt = SEAICEadvSalt .AND.
     &     SEAICEadvSchSalt.NE.ENUM_CENTERED_2ND
#else /* not ALLOW_GENERIC_ADVDIFF */
      IF ( SEAICEadvScheme.EQ.2 ) THEN
C-- for now, the code does not allow to use the default advection scheme
C    (Centered 2nd order) for 1 ice-field and an other advection scheme
C    for an other ice-field. In this case, stop here.
        checkAdvSchArea = SEAICEadvArea .AND.
     &     SEAICEadvSchArea.NE.2
        checkAdvSchHEFF = SEAICEadvHeff .AND.
     &     SEAICEadvSchHeff.NE.2
        checkAdvSchSnow = SEAICEadvSnow .AND.
     &     SEAICEadvSchSnow.NE.2
        checkAdvSchSalt = SEAICEadvSalt .AND.
     &     SEAICEadvSchSalt.NE.2
#endif /* ALLOW_GENERIC_ADVDIFF */
       IF ( checkAdvSchArea .OR. checkAdvSchHeff .OR.
     &      checkAdvSchSnow .OR. checkAdvSchSalt ) THEN
        WRITE(msgBuf,'(A,I3,A)') 'SEAICE_CHECK: SEAICEadvScheme=',
     &   SEAICEadvScheme, ' not compatible with those Adv.Scheme:'
        CALL PRINT_ERROR( msgBuf, myThid )
        IF ( checkAdvSchArea ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchArea = ',  SEAICEadvSchArea
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( checkAdvSchHeff ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchHeff = ',  SEAICEadvSchHeff
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( checkAdvSchSnow ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchSnow = ',  SEAICEadvSchSnow
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        IF ( checkAdvSchSalt ) THEN
         WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK:',
     &        ' SEAICEadvSchSalt = ',  SEAICEadvSchSalt
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
        errCount = errCount + 1
       ENDIF
      ELSEIF ( DIFF1 .NE. 0. _d 0 ) THEN
C-- for now, the code does not allow to use DIFF1 without the default
C   advection scheme (Centered 2nd order). In this case, stop here.
        WRITE(msgBuf,'(2A,1PE16.8)') 'SEAICE_CHECK: ',
     &        'harmonic+biharmonic DIFF1=',  DIFF1
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I3)') 'SEAICE_CHECK: ',
     &        'not available with SEAICEadvScheme=', SEAICEadvScheme
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C     Avoid using both type of diffusion scheme (DIFF1 & SEAICEdiffKh)
      IF ( DIFF1 .NE. 0. _d 0 .AND. (
     &     ( SEAICEdiffKhHeff .NE. 0. _d 0 ) .OR.
     &     ( SEAICEdiffKhArea .NE. 0. _d 0 ) .OR.
     &     ( SEAICEdiffKhSnow .NE. 0. _d 0 ) .OR.
     &     ( SEAICEdiffKhSalt .NE. 0. _d 0 )
     &                              ) ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' DIFF1 > 0 and one of the SEAICEdiffKh[] > 0'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' => Cannot use both type of diffusion'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF ( postSolvTempIter.GT.2 .OR. postSolvTempIter .LT. 0 ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' => allowed values for postSolveTempIter: 0, 1, 2'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF ( SEAICEpresH0    .LE. 0. _d 0 .OR.
     &     SEAICEpresPow0  .LT. 0 .OR. SEAICEpresPow1  .LT. 0 ) THEN
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &      'SEAICEpresH0 (real), SEAICEpresPow0 (integer)'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: OR SEAICEpresPow1 ',
     &      '(integer) has been specified as negative (data.seaice)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF

C--
#ifdef ALLOW_AUTODIFF_TAMC
      IF ( SEAICEnonLinIterMax .GT. MPSEUDOTIMESTEPS ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' need to increase MPSEUDOTIMESTEPS in SEAICE_PARAMS.h'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,2I4)') 'SEAICE_CHECK:',
     &  ' MPSEUDOTIMESTEPS, SEAICEnonLinIterMax = ',
     &  MPSEUDOTIMESTEPS, SEAICEnonLinIterMax
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( SEAICElinearIterMax .GT. SOLV_MAX_FIXED ) THEN
       WRITE(msgBuf,'(2A,I10,A,I10)') 'SEAICE_CHECK:',
     &      'SEAICElinearIterMax = ', SEAICElinearIterMax,
     &      ' > SOLV_MAX_FIXED = ', SOLV_MAX_FIXED
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,I10,A,I10)') 'SEAICE_CHECK:',
     &'either increase SOLV_MAX_FIXED or decrease SEAICElinearIterMax'
       CALL PRINT_ERROR( msgBuf, myThid )
       STOP 'ABNORMAL END: S/R SEAICE_LSR'
      ENDIF
      IF ( IMAX_TICE .GT. NMAX_TICE ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK:',
     &  ' need to increase NMAX_TICE in SEAICE_PARAMS.h'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(2A,2I4)') 'SEAICE_CHECK:',
     &  ' NMAX_TICE, MAX_TICE = ', NMAX_TICE, IMAX_TICE
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( SEAICE_maskRHS ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICE_maskRHS not allowed'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

C--   SEAICE_ALLOW_DYNAMICS and SEAICEuseDYNAMICS
#ifndef SEAICE_ALLOW_DYNAMICS
      IF (SEAICEuseDYNAMICS) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_ALLOW_DYNAMICS needed for SEAICEuseDYNAMICS'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF
#endif

#ifndef SEAICE_ALLOW_MOM_ADVECTION
      IF ( SEAICEmomAdvection ) THEN
       WRITE(msgBuf,'(A)')
     &   'SEAICE_ALLOW_MOM_ADVECTION needed for SEAICEmomAdvection'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

C--   SEAICE_EXTERNAL_FORCING is obsolete: issue warning but continue.
#ifdef SEAICE_EXTERNAL_FORCING
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &        'SEAICE_EXTERNAL_FORCING option is obsolete:'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &        'seaice now always uses exf to read input files.'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif

C--   SEAICE_GROWTH_LEGACY is obsolete: issue warning but continue.
#ifdef SEAICE_GROWTH_LEGACY
      WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &     'CPP flag SEAICE_GROWTH_LEGACY has been retired.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif /* SEAICE_GROWTH_LEGACY */

C--   SEAICE_CAP_HEFF is obsolete: issue warning but continue.
#ifdef SEAICE_CAP_HEFF
      WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &     'CPP flag SEAICE_CAP_HEFF has been retired.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif /* SEAICE_CAP_HEFF */

C--   SEAICE_MULTICATEGORY is obsolete: issue warning but continue.
#ifdef SEAICE_MULTICATEGORY
      WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &     'CPP flag SEAICE_MULTICATEGORY has been retired.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &     'Specify SEAICE_multDim=7 in data.seaice to recover'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &     'previous default SEAICE_MULTICATEGORY setting.'
      CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif /* SEAICE_MULTICATEGORY */

C--   SEAICE_ALLOW_TD_IF is obsolete: issue warning and stop.
#ifdef SEAICE_ALLOW_TD_IF
         WRITE(msgBuf,'(A)')
     &     'SEAICE_ALLOW_TD_IF option is obsolete:'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'the seaice*_IF codes are now merged into the main branch.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_ALLOW_TD_IF */

C--   SEAICE_DO_OPEN_WATER_GROWTH is obsolete: issue warning and stop.
#if defined(SEAICE_DO_OPEN_WATER_GROWTH) || \
      defined(SEAICE_DO_OPEN_WATER_MELT)
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &     'SEAICE_DO_OPEN_WATER_GROWTH / MELT options are obsolete'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)') 'they are replaced with run time',
     &     ' parameter SEAICE_doOpenWaterGrowth / Melt'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_DO_OPEN_WATER_GROWTH */

C--   SEAICE_OCN_MELT_ACT_ON_AREA is obsolete: issue warning and stop.
#ifdef SEAICE_OCN_MELT_ACT_ON_AREA
         WRITE(msgBuf,'(A)')
     &     'SEAICE_OCN_MELT_ACT_ON_AREA option is obsolete:'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'it is now done with SEAICE_areaLossFormula.EQ.1 and 2'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_OCN_MELT_ACT_ON_AREA */

C--   FENTY_AREA_EXPANSION_CONTRACTION is obsolete: issue warning and stop.
#ifdef FENTY_AREA_EXPANSION_CONTRACTION
         WRITE(msgBuf,'(A)')
     &     'FENTY_AREA_EXPANSION_CONTRACTION option is obsolete:'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'it is now done with SEAICE_areaLoss(Melt)Formula.EQ.1'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_DO_OPEN_WATER_MELT */

C--   SEAICE_AGE is obsolete: issue warning and stop.
#ifdef SEAICE_AGE
         WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &     'SEAICE_AGE option is obsolete: '
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)') 'it now is done',
     &     ' with SEAICE_SITRACER and siTrName=age'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_AGE */

C--   SEAICE_SALINITY is obsolete: issue warning and stop.
#ifdef SEAICE_SALINITY
         WRITE(msgBuf,'(A)')
     &     'SEAICE_SALINITY option is obsolete'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'use SEAICE_VARIABLE_SALINITY instead.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_SALINITY */

C--   SEAICE_OLD_AND_BAD_DISCRETIZATION is obsolete: issue warning and stop.
#ifdef SEAICE_OLD_AND_BAD_DISCRETIZATION
         WRITE(msgBuf,'(A)')
     &     'SEAICE_OLD_AND_BAD_DISCRETIZATION option is obsolete'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)')
     &     'and has no effect.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif /* SEAICE_OLD_AND_BAD_DISCRETIZATION */

C--   pkg/seaice requires pkg/exf with following CPP options/
C jmc: strickly true for Thermodynamics parts since Dynamics can be used
C      without EXF (assuming a simple scaling of wind-stress over ice)
      IF ( usePW79thermodynamics ) THEN
#ifndef ALLOW_EXF
         WRITE(msgBuf,'(A)')
     &        'need to define ALLOW_EXF'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#else /* ALLOW_EXF */
         IF ( .NOT.useEXF ) THEN
           WRITE(msgBuf,'(A)')
     &        'S/R SEAICE_CHECK: need to set useEXF in data.pkg'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#ifndef ALLOW_ATM_TEMP
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'need to define pkg/exf ALLOW_ATM_TEMP'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif
#ifndef ALLOW_DOWNWARD_RADIATION
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'need to define pkg/exf ALLOW_DOWNWARD_RADIATION'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
#endif
#ifdef SEAICE_EXTERNAL_FLUXES
# if !defined(EXF_READ_EVAP) && !defined(ALLOW_BULKFORMULAE)
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &     'need to set EXF_READ_EVAP or ALLOW_BULKFORMULAE in pkg/exf'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
# endif /* !defined(EXF_READ_EVAP) && !defined(ALLOW_BULKFORMULAE) */
         IF ( SEAICE_waterAlbedo .NE. UNSET_RL ) THEN
          WRITE(msgBuf,'(A)')
     &      'SEAICE_waterAlbedo is not used with SEAICE_EXTERNAL_FLUXES'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &      'Set exf_albedo in data.exf EXF_NML_01 instead'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
         ENDIF
         IF ( lwfluxfile .NE. ' ' .AND. lwdownfile .EQ. ' ' ) THEN
          i = ILNBLNK(lwfluxfile)
          WRITE(msgBuf,'(A,A)')
     &         'lwFlux is read from lwfluxfile = ',lwfluxfile(1:i)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &         'implying that lwdown = 0. For pkg/seaice to work '//
     &         'properly lwdown should be read from lwdownfile!'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
         ENDIF
         IF ( swfluxfile .NE. ' ' .AND. swdownfile .EQ. ' ' ) THEN
          i = ILNBLNK(swfluxfile)
          WRITE(msgBuf,'(A,A)')
     &         'swFlux is read from swfluxfile = ',swfluxfile(1:i)
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(A)')
     &         'implying that swdown = 0. For pkg/seaice to work '//
     &         'properly swdown should be read from swdownfile!'
          CALL PRINT_ERROR( msgBuf, myThid )
          errCount = errCount + 1
         ENDIF
#else /* if undef SEAICE_EXTERNAL_FLUXES */
         WRITE(msgBuf,'(3A)') 'S/R SEAICE_CHECK: ',
     &     'SEAICE_EXTERNAL_FLUXES is undefined, so we assume you ',
     &     'know what you are doing.'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &     'Use S/R SEAICE_BUDGET_OCEAN to compute fluxes over ocean.'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         CALL PRINT_ERROR( msgBuf, myThid )
#endif /* SEAICE_EXTERNAL_FLUXES */
#ifndef SEAICE_CGRID
         IF ( .NOT.useAtmWind ) THEN
           WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'needs pkg/exf useAtmWind to be true'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#endif
#ifndef EXF_SEAICE_FRACTION
         IF ( SEAICE_tauAreaObsRelax.GT.zeroRL ) THEN
           WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: ',
     &        'ice-area relaxation needs #define EXF_SEAICE_FRACTION'
           CALL PRINT_ERROR( msgBuf, myThid )
           errCount = errCount + 1
         ENDIF
#endif
#endif /* ALLOW_EXF */
C     end if usePW79thermodynamics
      ENDIF

      IF ( ( SEAICEuseDynamics .OR. SEAICEuseFREEDRIFT ) .AND.
     &     momStepping .AND. .NOT.SEAICEupdateOceanStress ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &      'SEAICEupdateOceanStress= FALSE will'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** SEAICE_CHECK: ',
     &      'give inconsistent Ocean surface stress'
        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

#ifdef SEAICE_ALLOW_DYNAMICS
      IF ( SEAICEuseDynamics ) THEN
       IF ( SEAICEuseJFNK ) THEN
        IF ( OLx.LT.3 .OR. OLy.LT.3 ) THEN
         WRITE(msgBuf,'(A,A)')
     &        'SEAICE_CHECK: cannot use JFNK-solver with',
     &        ' overlap (OLx,OLy) smaller than 3'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ELSE
        IF ( OLx.LT.2 .OR. OLy.LT.2 ) THEN
         WRITE(msgBuf,'(A,A)')
     &        'SEAICE_CHECK: cannot use dynamics solver with',
     &        ' overlap (OLx,OLy) smaller than 2'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
       IF ( SEAICE_OLx .GT. OLx-2 .OR. SEAICE_OLy .GT. OLy-2 .OR.
     &      SEAICE_OLx .LT. 0     .OR. SEAICE_OLy .LT. 0 ) THEN
        WRITE(msgBuf,'(A,I2,A,I2)') 'S/R SEAICE_CHECK: SEAICE_OLx/y = ',
     &       SEAICE_OLx, '/', SEAICE_OLy
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I2,A,I2)')
     &       'S/R SEAICE_CHECK: SEAICE_OLx/y cannot be smaller than 0 ',
     &       'or larger than OLx/y-2 = ', OLx-2, '/', OLy-2
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( SEAICE_2ndOrderBC ) THEN
        IF ( OLx.LT.3 .OR. OLy.LT.3 ) THEN
         WRITE(msgBuf,'(A,A)')
     &   'SEAICE_CHECK: SEAICE_2ndOrderBC = .TRUE. requires',
     &   ' an overlap (OLx,OLy) of at least 3'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
        IF ( SEAICE_OLx .GT. OLx-3 .OR. SEAICE_OLy .GT. OLy-3 ) THEN
         WRITE(msgBuf,'(A,I2,A,I2)')
     &        'S/R SEAICE_CHECK: SEAICE_OLx/y = ',
     &        SEAICE_OLx, '/', SEAICE_OLy
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I2,A,I2)')
     &        'S/R SEAICE_CHECK: with SEAICE_2ndOrderBC, SEAICE_OLx/y',
     &        ' cannot be larger than OLx/y-3 = ', OLx-3, '/', OLy-3
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF

# ifdef SEAICE_ALLOW_EVP
       IF ( SEAICEuseEVP ) THEN
#  ifdef ALLOW_AUTODIFF_TAMC
        IF ( SEAICEnEVPstarSteps.GT.nEVPstepMax ) THEN
         WRITE(msgBuf,'(A)')
     &        'SEAICE_CHECK: need to set nEVPstepMax to >= '//
     &        'SEAICEnEVPstarSteps'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,I4)')
     &   'SEAICE_CHECK: SEAICEnEVPstarSteps = ', SEAICEnEVPstarSteps
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
#  endif
        IF ( .NOT.(SEAICEuseEVPstar.OR.SEAICEuseEVPrev)
     &       .AND. SEAICEnEVPstarSteps.NE.
     &       INT(SEAICE_deltaTdyn/SEAICE_deltaTevp) ) THEN
         WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEnEVPstarSteps is '//
     &        'set in namelist, but SEAICEuseEVPstar = .FALSE.'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
        ENDIF
       ENDIF
# else
       IF ( SEAICEuseEVP ) THEN
        WRITE(msgBuf,'(A)')
     &       'SEAICE_CHECK: SEAICEuseEVP = .TRUE., so EVP is turned on'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'SEAICE_CHECK: by setting appropriate runtime parameters,'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'SEAICE_ALLOW_EVP is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
# endif

#ifdef SEAICE_ALLOW_EVP
       IF ( SEAICEuseEVP .AND. (SEAICE_eccfr.NE.SEAICE_eccen) ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseEVP = .TRUE., '//
     &       'so EVP is turned on by setting appropriate'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,F5.2,A,F5.2,A)')
     &       'SEAICE_CHECK: runtime parameters, but SEAICE_eccfr(=',
     &       SEAICE_eccfr , ')/=SEAICE_eccen(=', SEAICE_eccen, ')'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')'SEAICE_CHECK: implies a non-normal flow '//
     &       'rule for the elliptical yield curve,'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'SEAICE_CHECK: which is not implemented for EVP.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( SEAICEuseEVP .AND. (
# ifdef SEAICE_ALLOW_TEARDROP
     &      SEAICEuseTD .OR. SEAICEusePL .OR.
# endif
# ifdef SEAICE_ALLOW_MCE
     &      SEAICEuseMCE .OR.
# endif
# ifdef SEAICE_ALLOW_MCS
     &      SEAICEuseMCS .OR.
# endif
     &      .FALSE. ) ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseMCS, '//
     &      'SEAICEuseTD, SEAICEusePL, or SEAICEuseMCE = .TRUE.,'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: and SEAICEuseEVP = '//
     &       '.TRUE., but these non-standard rheologies'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)')
     &       'SEAICE_CHECK: are not implemented for EVP'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
#endif /* SEAICE_ALLOW_EVP */

C     SEAICEuseDynamics
      ENDIF
#endif /* SEAICE_ALLOW_DYNAMICS */

#ifndef SEAICE_GLOBAL_3DIAG_SOLVER
      IF ( SEAICEuseMultiTileSolver ) THEN
        WRITE(msgBuf,'(A)')
     &   'SEAICE_CHECK: SEAICEuseMultiTileSolver = .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)') ' but CPP-flag ',
     &   'SEAICE_GLOBAL_3DIAG_SOLVER is #undef in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* SEAICE_GLOBAL_3DIAG_SOLVER */

#ifndef SEAICE_ALLOW_CLIPVELS
      IF ( SEAICE_clipVelocities ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICE_clipVelocities = .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'SEAICE_ALLOW_CLIPVELS is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* SEAICE_ALLOW_CLIPVELS */

#ifndef SEAICE_ALLOW_CLIPZETA
      IF ( SEAICE_evpDampC .GT. 0. _d 0 .OR.
     &     SEAICE_zetaMin  .GT. 0. _d 0 ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICE_evpDampC and/or SEAICE_zetaMin '//
     &      'are set in data.seaice'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: but cpp-flag '//
     &       'SEAICE_ALLOW_CLIPZETA is not defined in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* SEAICE_ALLOW_CLIPZETA */

#ifndef SEAICE_ALLOW_TEM
      IF ( SEAICEuseTEM ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseTEM requires '//
     &      'that SEAICE_ALLOW_TEM is defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
#ifndef SEAICE_ALLOW_MCE
      IF ( SEAICEuseMCE ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseMCE requires '//
     &      'that SEAICE_ALLOW_MCE is defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
#ifndef SEAICE_ALLOW_MCS
      IF ( SEAICEuseMCS ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseMCS requires '//
     &      'that SEAICE_ALLOW_MCS is defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
#ifndef SEAICE_ALLOW_TEARDROP
      IF (SEAICEuseTD ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseTD requires  '//
     &      'that SEAICE_ALLOW_TEARDROP is defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( SEAICEusePL ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEusePL requires '//
     &      'that SEAICE_ALLOW_TEARDROP is defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

#ifndef SEAICE_CGRID

      IF ( SEAICEuseTEM
# ifdef SEAICE_ALLOW_TEARDROP
     &     .OR. SEAICEuseTD .OR. SEAICEusePL
# endif
# ifdef SEAICE_ALLOW_MCE
     &     .OR. SEAICEuseMCE
# endif
# ifdef SEAICE_ALLOW_MCS
     &     .OR. SEAICEuseMCS
# endif
     &     .OR. FALSE ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: non-default rheologies '//
     &      'require that SEAICE_CGRID is defined.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

# ifdef SEAICE_TEST_ICE_STRESS_1
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: Only relevant for B-grid:'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: SEAICE_TEST_ICE_STRESS_1 is replaced by'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &  'SEAICE_CHECK: SEAICE_BICE_STRESS (defined by default)'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
# endif /* SEAICE_TEST_ICE_STRESS_1 */
      IF ( SEAICEuseDYNAMICS.AND.useCubedSphereExchange ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'B-grid dynamics not working on Cubed-Sphere grid'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
      IF ( SEAICEuseDYNAMICS.AND.useOBCS ) THEN
       WRITE(msgBuf,'(2A)') 'SEAICE_CHECK: ',
     &       'Open-Boundaries not implemented in B-grid dynamics'
       CALL PRINT_ERROR( msgBuf, myThid )
C      errCount = errCount + 1
      ENDIF
#endif /* ndef SEAICE_CGRID */

#if ( defined SEAICE_ALLOW_TEARDROP || defined SEAICE_ALLOW_MCS || \
      defined SEAICE_ALLOW_MCE || defined SEAICE_ALLOW_TEM )
      IF ( (SEAICEuseMCE    .AND. SEAICEuseMCS) .OR.
     &     (SEAICEuseMCE    .AND. SEAICEuseTD) .OR.
     &     (SEAICEuseMCE    .AND. SEAICEusePL) .OR.
     &     (SEAICEuseMCE    .AND. SEAICEuseTEM) .OR.
     &     (SEAICEuseMCS    .AND. SEAICEuseTD) .OR.
     &     (SEAICEuseMCS    .AND. SEAICEusePL) .OR.
     &     (SEAICEuseMCS    .AND. SEAICEuseTEM) .OR.
     &     (SEAICEuseTD     .AND. SEAICEusePL) .OR.
     &     (SEAICEuseTD     .AND. SEAICEuseTEM) .OR.
     &     (SEAICEusePL     .AND. SEAICEuseTEM)
     &     ) THEN
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: More than one rheology '//
     &      'flag = .TRUE.,'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: but only one can be .TRUE. at a time.'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

C--   SEAICE_ALLOW_FREEDRIFT and SEAICEuseFREEDRIFT
#ifndef SEAICE_ALLOW_FREEDRIFT
      IF (SEAICEuseFREEDRIFT) THEN
        WRITE(msgBuf,'(A)')
     &   'need to #define SEAICE_ALLOW_FREEDRIFT for SEAICEuseFREEDRIFT'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
      IF ( LSR_mixIniGuess.GE.0 ) THEN
        WRITE(msgBuf,'(A)')
     &   'need to #define SEAICE_ALLOW_FREEDRIFT to use LSR_mixIniGuess'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif

#ifndef SEAICE_VARIABLE_SALINITY
      IF ( SEAICEadvSalt ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICEadvSalt = .TRUE. but cpp-flag'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: '//
     &   'SEAICE_VARIABLE_SALINITY is undef in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* SEAICE_VARIABLE_SALINITY */

#ifdef SEAICE_ALLOW_JFNK
      IF ( SEAICEuseJFNK ) THEN
       IF ( JFNKres_t.NE.UNSET_RL .AND. JFNKres_tFac.NE.UNSET_RL) THEN
        WRITE(msgBuf,'(3A)') 'S/R SEAICE_CHECK: JFNKres_t and ',
     &       'JFNKres_tFac are both set, so that JFNKres_t will be'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(3A)') 'S/R SEAICE_CHECK: ',
     &       'overwritten by JFNKres_tFac*JFNKresidual ',
     &       'in each initial Newton iteration.'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A)')
     &       'S/R SEAICE_CHECK: For safety we stop here. ',
     &       'Please unset one of the two parameters.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF (JFNKres_t.EQ.UNSET_RL.AND.JFNKres_tFac.EQ.UNSET_RL) THEN
        WRITE(msgBuf,'(2A)') 'S/R SEAICE_CHECK: need to specify ',
     &       'JFNKres_t or JFNKres_tFac for SEAICEuseJFNK=.TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( SEAICEuseJFNK .AND. SEAICEuseEVP ) THEN
       WRITE(msgBuf,'(2A)')
     &      'S/R SEAICE_CHECK: cannot have both SEAICEuseJFNK=.TRUE.',
     &      'and SEAICEuseEVP=.TRUE. (i.e. SEAICE_deltaTevp > 0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#else
      IF ( SEAICEuseJFNK ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICEuseJFNK = .TRUE. but cpp-flag'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: '//
     &   'SEAICE_ALLOW_JFNK is undef in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* SEAICE_ALLOW_JFNK */
#ifndef SEAICE_ALLOW_KRYLOV
      IF ( SEAICEuseKrylov ) THEN
        WRITE(msgBuf,'(A)')
     &      'SEAICE_CHECK: SEAICEuseKRYLOV = .TRUE. but cpp-flag'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: '//
     &   'SEAICE_ALLOW_KRYLOV is undef in SEAICE_OPTIONS.h'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
      ENDIF
#endif /* SEAICE_ALLOW_KRYLOV */

      IF ( SEAICEuseDynamics .AND. .NOT.SEAICEuseJFNK ) THEN
       IF ( SEAICEuseBDF2 ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseBDF2 = .TRUE. '//
     &       'only allowed with SEAICEuseJFNK = .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
       IF ( SEAICEuseIMEX ) THEN
        WRITE(msgBuf,'(A)') 'SEAICE_CHECK: SEAICEuseIMEX = .TRUE. '//
     &       'only allowed with SEAICEuseJFNK = .TRUE.'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
      IF ( SEAICEuseIMEX ) THEN
       WRITE(msgBuf,'(A)') '** WARNING ** SEAICE_CHECK: '//
     &      'SEAICEuseIMEX = .TRUE. '//
     &      'currently has no effect, because the code is missing'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
      ENDIF

      IF ( .NOT.(SEAICEetaZmethod.EQ.0.OR.SEAICEetaZmethod.EQ.3) ) THEN
       WRITE(msgBuf,'(A,I2)')
     &      'SEAICE_CHECK: SEAICEetaZmethod = ', SEAICEetaZmethod
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: '//
     &      'is no longer allowed; allowed values are 0 and 3'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

      IF ( SEAICEpressReplFac .LT. 0. _d 0 .OR.
     &     SEAICEpressReplFac .GT. 1. _d 0 ) THEN
       WRITE(msgBuf,'(A,I2)')
     &      'SEAICE_CHECK: SEAICEpressReplFac = ', SEAICEpressReplFac
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: '//
     &      'cannot < 0 or > 1'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF

C--   Some sanity checks for SEAICEdWatMin
      IF ( SEAICEdWatMin .LT. 0. _d 0 ) THEN
       WRITE(msgBuf,'(A,A,F5.2,A)') 'SEAICE_CHECK: ',
     &      'SEAICEdWatMin (= ', SEAICEdWatMin, ') may not be < 0.'
       CALL PRINT_ERROR( msgBuf , myThid)
       errCount = errCount + 1
      ENDIF
      IF ( SEAICE_waterDrag .EQ. 0. _d 0
     &     .AND. SEAICEdWatMin .GT. 0. _d 0) THEN
       WRITE(msgBuf,'(A,A,F5.2)') '** WARNING ** SEAICE_CHECK: ',
     &      'SEAICE_waterDrag  = ', SEAICE_waterDrag
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,A,F5.2)') '** WARNING ** SEAICE_CHECK: ',
     &      'but SEAICEdWatMin = ', SEAICEdWatMin
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A,A)') '** WARNING ** SEAICE_CHECK: ',
     &      'so that the ice-ocean drag will be non-zero'
       CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF
#ifdef SEAICE_ALLOW_FREEDRIFT
      IF ( SEAICE_waterDrag_South .EQ. 0. _d 0
     &     .OR.  SEAICE_waterDrag .EQ. 0. _d 0 ) THEN
       WRITE(msgBuf,'(A,A,A)') 'SEAICE_CHECK: ',
     &      'When SEAICE_ALLOW_FREEDRIFT is defined, ',
     &      'SEAICE_waterDrag or SEAICE_waterDragSouth may not be zero.'
       CALL PRINT_ERROR( msgBuf , myThid)
       errCount = errCount + 1
      ENDIF
#endif /* SEAICE_ALLOW_FREEDRIFT */

#ifndef SEAICE_ALLOW_BOTTOMDRAG
      IF ( SEAICEbasalDragK2 .GT. 0. _d 0 ) THEN
       WRITE(msgBuf,'(A,I2)')
     &      'SEAICE_CHECK: SEAICEbasalDragK2 = ', SEAICEbasalDragK2
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: is greater than 0, '//
     &      'but SEAICE_ALLOW_BOTTOMDRAG is not defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif /* SEAICE_ALLOW_BOTTOMDRAG */

#ifdef SEAICE_ITD
C     The ice thickness distribution (ITD) module can only be used with
C     the zero-layer thermodynamics of S/R SEAICE_GROWTH and the
C     advection in S/R SEAICE_ADVDIFF
C     If useThSice=.TRUE., do not reset it here, but issue a warning
      IF ( useThSice ) THEN
       WRITE(msgBuf,'(A)') '** WARNING ** SEAICE_CHECK: '//
     &      'SEAICE_ITD is defined, but useThSice = .TRUE.'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') '** WARNING ** SEAICE_CHECK: '//
     &      'avoids the ice thickness distribution code.'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(A)') '** WARNING ** SEAICE_CHECK: '//
     &      'If you want the ITD code, set useThSice=.FALSE.'
       CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
      ENDIF
C
      IF ( .NOT.SEAICEmultiDimAdvection ) THEN
       WRITE(msgBuf,'(A,L1)')
     &      'SEAICE_CHECK: SEAICEmultiDimAdvection = ',
     &      SEAICEmultiDimAdvection
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: is not allowed when '//
     &      'SEAICE_ITD is defined'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A)') 'SEAICE_CHECK: use a multiDimAdvection '//
     &      'scheme instead'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif

      IF ( errCount .GE. 1 ) THEN
       WRITE(msgBuf,'(A,I3,A)') 'SEAICE_CHECK: ', errCount,
     &      ' parameter/CPP-flag combinations are '//
     &      'inconsistent or incomplete'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,I3,A)')
     &       'SEAICE_CHECK: detected', errCount,' fatal error(s)'
       CALL PRINT_ERROR( msgBuf, myThid )
       CALL ALL_PROC_DIE( 0 )
       STOP 'ABNORMAL END: S/R SEAICE_CHECK'
      ENDIF

      _END_MASTER(myThid)

      RETURN
      END
