C $Header: /u/gcmpack/MITgcm/model/src/set_defaults.F,v 1.1 1998/12/15 00:20:35 adcroft Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE SET_DEFAULTS(
     O   viscArDefault, diffKrTDefault, diffKrSDefault,
     O   hFacMinDrDefault, delRdefault,
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
#include "CG2D.h"

C     === Routine arguments ===
C     myThid - Number of this instance of INI_PARMS
      INTEGER myThid
      _RL viscArDefault
      _RL diffKrTDefault
      _RL diffKrSDefault
      _RL hFacMinDrDefault
      _RL delRDefault(Nr)

C     === Local variables ===
C     K, I, J - Loop counters
      INTEGER K, I, J

C--   Grid parameters
C     Vertical gridding
      rkFac = 1.D0
      horiVertRatio = 1.D0
      DO k=1,Nr
       delRdefault(k) = 1.D2
      ENDDO

C     Horizontal gridding
C     In cartesian coords distances are in metres
      usingCartesianGrid = .TRUE.
      DO i=1,Nx
       delX(i) = 20.D0 * 1000.D0
      ENDDO
      DO j=1,Ny
       delY(j) = 20.D0 * 1000.D0
      ENDDO
C     In spherical polar distances are in degrees
      usingSphericalPolarGrid = .FALSE.
      phiMin    = -5.0
      thetaMin  = 0.
      rSphere   = 6370. * 1.D3
      IF ( usingSphericalPolarGrid ) THEN
       DO i=1,Nx
        delX(i) = 1.D0
       ENDDO
       DO j=1,Ny
        delY(j) = 1.D0
       ENDDO
      ENDIF 
C

C--   Set default "physical" parameters
      DO K =1,Nr
       tRef(K) = 30.D0 - FLOAT( K )
      ENDDO
      gravity  = 9.81D0
      rhoNil   = 999.8D0
      f0       = 1.D-4 
      beta     = 1.D-11
      viscAh   = 1.D3
      diffKhT  = 1.D3
      diffKhS  = 1.D3
      viscArDefault = 1.D-3
      no_slip_sides = .TRUE.
      no_slip_bottom = .TRUE.
      diffKrTDefault = 1.D-5
      diffKrSDefault = 1.D-5
      viscA4   = 0.
      diffK4T  = 0.
      diffK4S  = 0.
      GMmaxslope   =   1.D-2
      GMlength     = 200.D3
      GMalpha      = 0.D0
      GMdepth      = 1000.D0
      GMkbackground= 0.D0
      GMmaxval     = 2500.D0
      HeatCapacity_Cp = 3994.D0
      tAlpha       = 2.D-4
      sBeta        = 7.4D-4
      eosType      = 'LINEAR'
      buoyancyRelation    = 'OCEANIC'
      implicitFreeSurface = .TRUE.
      rigidLid            = .FALSE.
      hFacMin             = 1.D0
      hFacMinDrDefault    = 0.D0
      momViscosity        = .TRUE.
      momAdvection        = .TRUE.
      momForcing          = .TRUE.
      useCoriolis         = .TRUE.
      momPressureForcing  = .TRUE.
      momStepping         = .TRUE.
      tempStepping        = .TRUE.
      saltStepping        = .TRUE.
      metricTerms         = .FALSE.
      implicitDiffusion   = .FALSE.
      openBoundaries      = .FALSE.

C--   Elliptic solver parameters
      cg2dMaxIters       = 150
      cg2dTargetResidual = 1.D-7
      cg2dChkResFreq     = 1
      cg3dMaxIters       = 150
      cg3dTargetResidual = 1.D-7
      cg3dChkResFreq     = 1
      cg2dpcOffDFac      = 0.51D0

C--   Time stepping parameters
      deltaT            = 0.
      nIter0            = 0
      startTime         = deltaT*float(nIter0)
      nTimeSteps        = 100
      endTime           = deltaT*float(nTimeSteps)
      abEps             = 0.01
      pchkPtFreq        = deltaT*0
      chkPtFreq         = deltaT*0
      dumpFreq          = deltaT*50
      taveFreq          = deltaT*0
      writeStatePrec    = precFloat64
      writeBinaryPrec   = precFloat32
      readBinaryPrec    = precFloat32
      nCheckLev         = 1
      checkPtSuff(1)    = 'ckptA'
      checkPtSuff(2)    = 'ckptB'
      cAdjFreq          = -1.D0
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

C--   OBCS
      DO I=1,Nx
       OB_Jnorth(I)=0
       OB_Jsouth(I)=0
      ENDDO
      DO J=1,Ny
       OB_Ieast(J)=0
       OB_Iwest(J)=0
      ENDDO

C
      RETURN
      END

