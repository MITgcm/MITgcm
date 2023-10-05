#include "STIC_OPTIONS.h"

      SUBROUTINE STIC_CHECK( myThid )
C     *==========================================================*
C     | SUBROUTINE STIC_CHECK                                |
C     | o Validate basic package setup and inter-package         |
C     | dependencies.                                            |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STIC.h"

C     === Routine arguments ===
C     myThid   :  Number of this instance of STIC_CHECK
      INTEGER myThid

#ifdef ALLOW_STEEP_ICECAVITY

C     === Local variables ===
C     msgBuf   :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER errCount

      _BEGIN_MASTER(myThid)
      errCount = 0

      WRITE(msgBuf,'(A)') 'STIC_CHECK: #define ALLOW_STEEP_ICECAVITY'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C     print stic summary here before checking the parameter combinations
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(A)')
     &     'STIC_CHECK: start of STIC config. summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

       CALL WRITE_0D_L( STICisOn, INDEX_NONE,
     &   'STICisOn =', ' /* package is turned on */')

       CALL WRITE_0D_L( useISOMIPTD, INDEX_NONE,
     &   'useISOMIPTD =', ' /* use simple isomip thermodynamics */')
       CALL WRITE_0D_L( STICconserve, INDEX_NONE,
     &   'STICconserve =',
     &      ' /* use a conservative form of boundary conditions */')
       CALL WRITE_0D_L( STICboundaryLayer, INDEX_NONE,
     &   'STICboundaryLayer =',
     &   ' /* use simple boundary layer scheme to suppress noise */')
       IF ( STICboundaryLayer ) THEN
        CALL WRITE_0D_L( SHI_withBL_realFWflux, INDEX_NONE,
     &   'SHI_withBL_realFWflux =',
     &   ' /* use real FW Flux in boundary layer scheme */')
       ENDIF
       CALL WRITE_0D_L( STICadvDiffHeatFlux, INDEX_NONE,
     &   'STICadvDiffHeatFlux =',
     &   ' /* use adv.-diff. instead of just diff. heat flux into'//
     &   ' the ice shelf */')
       CALL WRITE_0D_L( STICMassStepping, INDEX_NONE,
     &   'STICMassStepping =',
     &   ' /* step forward ice shelf mass/thickness */')
       CALL WRITE_0D_L( no_slip_stic, INDEX_NONE,
     &   'no_slip_stic =', ' /* use no slip boundary conditions */')
       CALL WRITE_0D_RL( STICdragLinear, INDEX_NONE,
     & 'STICdragLinear    =', ' /* linear drag coefficient */')
       CALL WRITE_0D_RL( STICdragQuadratic, INDEX_NONE,
     & 'STICdragQuadratic =', ' /* quadratic drag coefficient */')
       CALL WRITE_0D_I( STICselectDragQuadr, INDEX_NONE,
     &  'STICselectDragQuadr =',
     &  ' /* select quadratic drag option */')
       CALL WRITE_0D_RL( STIClatentHeat, INDEX_NONE,
     & 'STIClatentHeat      =', ' /* latent heat of ice */')
       CALL WRITE_0D_RL( STICheatCapacity_Cp, INDEX_NONE,
     & 'STICheatCapacity_Cp =', ' /* heat capacity of ice shelf */')
       CALL WRITE_0D_RL( rhoSTIC, INDEX_NONE,
     & 'rhoSTIC   =', ' /* density of ice shelf */')
       CALL WRITE_0D_RL( STICkappa, INDEX_NONE,
     & 'STICkappa =', ' /* thermal conductivity of ice shelf */')
       CALL WRITE_0D_RL( STICthetaSurface, INDEX_NONE,
     & 'STICthetaSurface =',' /* surface temperature above i.s. */')
       CALL WRITE_0D_RL( STICheatTransCoeff, INDEX_NONE,
     & 'STICheatTransCoeff =', ' /* heat transfer coefficient */')
       CALL WRITE_0D_RL( STICsaltTransCoeff, INDEX_NONE,
     & 'STICsaltTransCoeff =', ' /* salt transfer coefficient */')

       CALL WRITE_0D_L( STICuseGammaFrict, INDEX_NONE,
     &   'STICuseGammaFrict =',
     &   ' /* use velocity dependent exchange coefficients */')
       IF ( STICuseGammaFrict ) THEN
        IF ( STICboundaryLayer ) THEN
         CALL WRITE_0D_L( SHI_withBL_uStarTopDz, INDEX_NONE,
     &   'SHI_withBL_uStarTopDz =',
     &   ' /* compute uStar from top Dz averaged uVel,vVel */')
        ELSE
         CALL WRITE_0D_L( STIC_oldCalcUStar, INDEX_NONE,
     &   'STIC_oldCalcUStar =', ' /* use old uStar expression */')
        ENDIF
        CALL WRITE_0D_RL( shiCdrag, INDEX_NONE,
     & 'shiCdrag   =', ' /* quadr. drag coefficient for uStar */')
        CALL WRITE_0D_RL( shiZetaN, INDEX_NONE,
     & 'shiZetaN   =', ' /* parameter for gammaTurb */')
        CALL WRITE_0D_RL( shiRc, INDEX_NONE,
     & 'shiRc      =', ' /* parameter for gammaTurb (not used) */')
        CALL WRITE_0D_RL( shiPrandtl, INDEX_NONE,
     & 'shiPrandtl =', ' /* const. Prandtl No. for gammaTurb */')
        CALL WRITE_0D_RL( shiSchmidt, INDEX_NONE,
     & 'shiSchmidt =', ' /* const. Schmidt No. for gammaTurb */')
        CALL WRITE_0D_RL( shiKinVisc, INDEX_NONE,
     & 'shiKinVisc =', ' /* const. kin. viscosity for gammaTurb */')
       ENDIF

      CALL WRITE_0D_RL( STICremeshFrequency, INDEX_NONE,
     &   'STICremeshFrequency =',
     &   ' /* Frequency (in s) of Remeshing */')
      IF ( STICremeshFrequency .GT. zeroRL ) THEN
        CALL WRITE_0D_RL( STICsplitThreshold, INDEX_NONE,
     &   'STICsplitThreshold =',
     &   ' /* hFac remesh threshold above which cell splits */')
        CALL WRITE_0D_RL( STICmergeThreshold, INDEX_NONE,
     &   'STICmergeThreshold =',
     &   ' /* hFac remesh threshold below which cell merges */')
      ENDIF

      CALL WRITE_0D_C( STICloadAnomalyFile, -1, INDEX_NONE,
     &  'STICloadAnomalyFile =',
     &  ' /* file name of loaded loadAnomaly field */')
      CALL WRITE_0D_C( STICmassFile, -1, INDEX_NONE,
     &  'STICmassFile =', ' /* file name of loaded mass field */')
      CALL WRITE_0D_C( STICtopoFile, -1, INDEX_NONE,
     &  'STICtopoFile =',
     &  ' /* file name of loaded under-ice topography */')
      CALL WRITE_0D_C( STICMassDynTendFile, -1, INDEX_NONE,
     &  'STICMassDynTendFile =',
     &  ' /* file name of loaded dynamic mass tendency field */')
C-    I/O parameters:
       CALL WRITE_0D_L( STICwriteState, INDEX_NONE,
     &   'STICwriteState  =', ' /* do simple standard output */')
       CALL WRITE_0D_L( STIC_dump_mdsio, INDEX_NONE,
     &   'STIC_dump_mdsio =', ' /* use mdsio for snapshots */')
       CALL WRITE_0D_L( STIC_tave_mdsio, INDEX_NONE,
     &   'STIC_tave_mdsio =', ' /* use mdsio for time averages */')
       CALL WRITE_0D_L( STIC_dump_mnc, INDEX_NONE,
     &   'STIC_dump_mnc   =', ' /* use netcdf for snapshots */')
       CALL WRITE_0D_L( STIC_tave_mnc, INDEX_NONE,
     &   'STIC_tave_mnc   =', ' /* use netcdf for time averages */')
       CALL WRITE_0D_RL( STIC_dumpFreq, INDEX_NONE,
     & 'STIC_dumpFreq =', ' /* analoguous to dumpFreq */')
       CALL WRITE_0D_RL( STIC_taveFreq, INDEX_NONE,
     & 'STIC_taveFreq =', ' /* analoguous to taveFreq */')

      WRITE(msgBuf,'(A)')
     &     'STIC_CHECK: end of STIC config. summary'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

C-    Check that CPP option is "defined" when corresponding run-time parameter is on
#ifndef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time control flag useISOMIPTD was used'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A,A)')
     &      'when CPP flag ALLOW_ISOMIP_TD was unset'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#else
      IF ( useISOMIPTD ) THEN
       IF ( STICconserve ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &       'Run-time control flag STICconserve=.TRUE. was used'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(A,A,A)')
     &       'when useISOMIPTD=.TRUE.; this does not work!'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF
#endif /* ALLOW_ISOMIP_TD */

#ifndef SHI_ALLOW_GAMMAFRICT
      IF ( STICuseGammaFrict ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time control flag STICuseGammaFrict was used'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A,A)')
     &      'when CPP flag SHI_ALLOW_GAMMAFRICT was unset'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#else
      WRITE(msgBuf,'(A,A,A)')
     &     'CPP flag SHI_ALLOW_GAMMAFRICT has to be undefined,',
     &     ' because related code is not implemented.'
      CALL PRINT_ERROR( msgBuf, myThid )
      errCount = errCount + 1
#endif /* SHI_ALLOW_GAMMAFRICT */

#ifndef ALLOW_STIC_REMESHING
      IF ( STICremeshFrequency.GT.zeroRL ) THEN
       WRITE(msgBuf,'(A,A,A)')
     &      'Run-time STICremeshFrequency was set ( > 0 )'
       CALL PRINT_ERROR( msgBuf, myThid )
       WRITE(msgBuf,'(A,A,A)')
     &      'when CPP flag ALLOW_STIC_REMESHING is unset'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif /* ALLOW_STIC_REMESHING */

C     Consistent choice of parameter:

      IF ( STICmassFile.NE.' ' .AND. STICloadAnomalyFile.NE.' '
     &   ) THEN
         WRITE(msgBuf,'(2A)') 'STIC_CHECK: cannot specify both',
     &      ' STICmassFile and STICloadAnomalyFile'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

C     So far, STIC works only with oceanic z-coordinates
      IF ( buoyancyRelation .NE. 'OCEANIC' ) THEN
         WRITE(msgBuf,'(A)')
     &      'STIC works only with buoyancyRelation = ''OCEANIC'''
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

      IF ( selectNHfreeSurf.NE.0 ) THEN
C--   Non-Hydrostatic Free-Surf below Ice-Shelf:
C     a) there is a practical implementation issue (kSurf > 1) that has to be
C        fixed before removing this stop.
C     b) In general, should also account for the vertical acceleration of the
C        Ice-shelf mass above Free-Surf in NH.Free-Surf equation (less obvious).
C        Ignore this term for now; as a consequence, might over-estimate
C        d.Eta/dt below Ice-Shelf.
         WRITE(msgBuf,'(2A,I3,A)') 'Current nonHydrostatic',
     &    ' Free-Surf option (selectNHfreeSurf=',selectNHfreeSurf,' )'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A)') ' not compatible with STIC code'
         CALL PRINT_ERROR( msgBuf, myThid )
         errCount = errCount + 1
      ENDIF

      IF ( STICremeshFrequency.GT.zeroRL ) THEN
       IF ( nonlinFreeSurf.LE.0 ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &     'remeshing requires Non-Lin Free-Surf (nonlinFreeSurf > 0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ELSEIF ( select_rStar.GT.0 ) THEN
        WRITE(msgBuf,'(A,A,A)')
     &     'remeshing not implemented for r-Star coordinate ',
     &     '(select_rStar > 0)'
        CALL PRINT_ERROR( msgBuf, myThid )
        errCount = errCount + 1
       ENDIF
      ENDIF

C     STIC may not work with many other packages,
C     e.g. vertical mixing schemes, in particular KPP will not work properly,
C     as KPP requires surface fluxes at the surface, whereas stic will
C     provide interfacial fluxes at some depth. Richardson flux number based
C     schemes such as Packanowski-Philander (PP81) should be no problem.
CML      IF ( useKPP ) THEN
CML         WRITE(msgBuf,'(A)')
CML     &      'STIC and KPP cannot be turned on at the same time'
CML         CALL PRINT_ERROR( msgBuf , 1)
CML         errCount = errCount + 1
CML      ENDIF

      IF ( STICMassStepping .AND. useSEAICE ) THEN
        WRITE(msgBuf,'(2A)') '** WARNING ** STIC_CHECK: ',
     &   'pkg/seaice IGNORES Ice-Shelf retreat/advance'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(2A)') '** WARNING ** STIC_CHECK: ',
     &   ' that might occur with STICMassStepping'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF

C     Currently cannot use STICMassStepping with OpenAD
C     See stic_mass_stepping.F and github.com/MITgcm/MITgcm/pull/495
#ifdef ALLOW_OPENAD
      IF (STICMassStepping) THEN
       WRITE(msgBuf,'(2A)')
     &    'STICMassStepping does not currently work',
     &    ' with #define ALLOW_OPENAD'
       CALL PRINT_ERROR( msgBuf, myThid )
       errCount = errCount + 1
      ENDIF
#endif
      IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &       'STIC_CHECK: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R STIC_CHECK'
      ENDIF

      _END_MASTER(myThid)

#endif /* ALLOW_STEEP_ICECAVITY */

      RETURN
      END
