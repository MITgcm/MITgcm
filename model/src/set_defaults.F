C $Header: /u/gcmpack/MITgcm/model/src/set_defaults.F,v 1.16 2001/03/06 17:25:07 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRdefault, rkFacDefault,
     I   myThid )
C     /==========================================================\
C     | SUBROUTINE SET_DEFAULTS                                  |
C     | o Routine to set model "parameters"                      |
C     |==========================================================|
C     | Notes:                                                   |
C     | ======                                                   |
C     | The present version of this routine is a place-holder.   |
C     | A production version needs to handle parameters from an  |
C     | external file and possibly reading in some initial field |
C     | values.                                                  |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

C     === Routine arguments ===
C     myThid - Number of this instance of INI_PARMS
      INTEGER myThid
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)
      _RS rkFacDefault

C     === Local variables ===
C     K, I, J - Loop counters
      INTEGER K, I, J

C--   Grid parameters
C     Vertical gridding
      rkFacDefault         = 1.D0
      horiVertRatio        = 1.D0
      Ro_SeaLevel = 0.
      DO k=1,Nr
       delRdefault(k) = 0.
      ENDDO

C     Horizontal gridding
C     In cartesian coords distances are in metres
      usingCartesianGrid = .TRUE.
      DO i=1,Nx
       delX(i) = UNSET_RL
      ENDDO
      DO j=1,Ny
       delY(j) = UNSET_RL
      ENDDO
C     In spherical polar distances are in degrees
      usingSphericalPolarGrid = .FALSE.
      phiMin               = 0.0
      thetaMin             = 0.
      rSphere              = 6370. * 1.D3

C--   Set default "physical" parameters
      DO K =1,Nr
       tRef(K) = 30.D0 - FLOAT( K )
      ENDDO
      gravity             = 9.81D0
      rhoNil              = 999.8D0
      f0                  = 1.D-4
      beta                = 1.D-11
      viscAh              = 0.D3
      diffKhT             = 0.D3
      diffKhS             = 0.D3
      viscArDefault       = 0.D-3
      no_slip_sides       = .TRUE.
      no_slip_bottom      = .TRUE.
      diffKrTDefault      = 0.D-3
      diffKrSDefault      = 0.D-3
      viscA4              = 0.D11
      diffK4T             = 0.D11
      diffK4S             = 0.D11
      cosPower            = 0.
      HeatCapacity_Cp     = 3994.D0
      tAlpha              = 2.D-4
      sBeta               = 7.4D-4
      eosType             = 'LINEAR'
      buoyancyRelation    = 'OCEANIC'
      implicitFreeSurface = .TRUE.
      rigidLid            = .FALSE.
      implicSurfPress     = 1.D0
      implicDiv2DFlow     = 1.D0
      hFacMin             = 1.D0
      hFacMinDrDefault    = 0.D0
      staggerTimeStep     = .FALSE.
      momViscosity        = .TRUE.
      momAdvection        = .TRUE.
      momForcing          = .TRUE.
      useCoriolis         = .TRUE.
      momPressureForcing  = .TRUE.
      momStepping         = .TRUE.
      tempStepping        = .TRUE.
      saltStepping        = .TRUE.
      metricTerms         = .TRUE.
      implicitDiffusion   = .FALSE.
      implicitViscosity   = .FALSE.
      nonHydrostatic      = .FALSE.
      globalFiles         = .FALSE.
      allowFreezing       = .FALSE.
      ivdc_kappa          = 0.D0
      groundAtK1          = .FALSE.
      nShap               = 0
      zonal_filt_lat      = 90.
      zonal_filt_sinpow   = 2
      zonal_filt_cospow   = 2
      bottomDragLinear    = 0.
      bottomDragQuadratic = 0.

C--   Elliptic solver parameters
      cg2dMaxIters       = 150
      cg2dTargetResidual = 1.D-7
      cg2dChkResFreq     = 1
      cg2dpcOffDFac      = 0.51D0
      cg3dMaxIters       = 150
      cg3dTargetResidual = 1.D-7
      cg3dChkResFreq     = 1

C--   Time stepping parameters
      deltaT            = 0.
      nIter0            = 0
      startTime         = deltaT*float(nIter0)
      nTimeSteps        = 0
      nEndIter          = nIter0+nTimeSteps
      endTime           = deltaT*float(nEndIter)
      abEps             = 0.01
      pchkPtFreq        = deltaT*0
      chkPtFreq         = deltaT*0
      dumpFreq          = deltaT*0
      taveFreq          = deltaT*0
      writeStatePrec    = precFloat64
      writeBinaryPrec   = precFloat32
      readBinaryPrec    = precFloat32
      nCheckLev         = 1
      checkPtSuff(1)    = 'ckptA'
      checkPtSuff(2)    = 'ckptB'
      cAdjFreq          =  0.D0
      tauCD             =  0.D0
      tauThetaClimRelax =  0.D0
      tauSaltClimRelax  =  0.D0
      periodicExternalForcing = .FALSE.
      externForcingPeriod     = 0.
      externForcingCycle      = 0.

C--   Input files
      bathyFile       = ' '
      hydrogSaltFile  = ' '
      hydrogThetaFile = ' '
      zonalWindFile   = ' '
      meridWindFile   = ' '
      thetaClimFile   = ' '
      saltClimFile    = ' '
      EmPmRfile       = ' '
      surfQfile       = ' '
      surfQswfile     = ' '
      uVelInitFile    = ' '
      vVelInitFile    = ' '
      pSurfInitFile   = ' '

C
      RETURN
      END

