C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_init_fixed.F,v 1.14 2012/02/09 03:42:32 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_INIT_FIXED( myThid )
C     *==========================================================*
C     | SUBROUTINE SEAICE_INIT_FIXED
C     | o Initialization of sea ice model.
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      INTEGER kSurface
#ifndef SEAICE_CGRID
      _RS  mask_uice
#endif
#ifdef SHORTWAVE_HEATING
cif   Helper variable for determining the fraction of sw radiation
cif   penetrating the model shallowest layer
      _RL dummyTime
      _RL swfracba(2)
      _RL tmpFac
#endif /* SHORTWAVE_HEATING */

      IF ( buoyancyRelation .EQ. 'OCEANICP' ) THEN
       kSurface        = Nr
      ELSE
       kSurface        = 1
      ENDIF

C     Initialize MNC variable information for SEAICE
      IF ( useMNC .AND.
     &    (seaice_tave_mnc.OR.seaice_dump_mnc.OR.SEAICE_mon_mnc)
     &   ) THEN
        CALL SEAICE_MNC_INIT( myThid )
      ENDIF

      _BEGIN_MASTER(myThid)
#ifdef SHORTWAVE_HEATING
       tmpFac    = -1.0
       dummyTime = 1.0
       swfracba(1) = ABS(rF(1))
       swfracba(2) = ABS(rF(2))
       CALL SWFRAC(
     I       2, tmpFac,
     U       swfracba,
     I       dummyTime, 0, myThid )
       SWFracB = swfracba(2)
#else /* SHORTWAVE_HEATING */
       SWFracB = 0. _d 0
#endif /* SHORTWAVE_HEATING */
      _END_MASTER(myThid)
      

C--   efficiency of ocean-ice turbulent flux ...
      if (SEAICEturbFluxFormula.EQ.1) then
c     ... can be specified as fractions between 0 and 1
        IF ( SEAICE_availHeatFrac .EQ. UNSET_RL )
     &    SEAICE_availHeatFrac = ONE
        IF ( SEAICE_availHeatFracFrz .EQ. UNSET_RL )
     &    SEAICE_availHeatFracFrz = SEAICE_availHeatFrac
      elseif (SEAICEturbFluxFormula.EQ.2) then
c     ... can be specified as time scales (>SEAICE_deltaTtherm)
        IF ( SEAICE_gamma_t .EQ. UNSET_RL )
     &    SEAICE_gamma_t=SEAICE_deltaTtherm   
        IF ( SEAICE_gamma_t_frz .EQ. UNSET_RL )
     &    SEAICE_gamma_t_frz=SEAICE_gamma_t     
        IF ( SEAICE_gamma_t .LE. SEAICE_deltaTtherm ) THEN
            SEAICE_availHeatFracFrz = 1. _d 0
        ELSE
            SEAICE_availHeatFrac = SEAICE_deltaTtherm/SEAICE_gamma_t
        ENDIF
        IF ( SEAICE_gamma_t_frz .LE. SEAICE_deltaTtherm ) THEN
            SEAICE_availHeatFracFrz = 1. _d 0
        ELSE
            SEAICE_availHeatFracFrz = 
     &        SEAICE_deltaTtherm/SEAICE_gamma_t_frz
        ENDIF
      elseif ( (SEAICEturbFluxFormula.EQ.3).OR.
     &         (SEAICEturbFluxFormula.EQ.4) ) then
c     ... is hard-coded (after McPhee)
        SEAICE_availHeatFrac= MCPHEE_TAPER_FAC * STANTON_NUMBER * 
     &    USTAR_BASE / dRf(kSurface) * SEAICE_deltaTtherm
        SEAICE_availHeatFracFrz=ZERO
      endif

C--   convert SEAICE_doOpenWaterGrowth/Melt logical switch to numerical facOpenGrow/Melt
        facOpenGrow=0. _d 0
        facOpenMelt=0. _d 0
        if (SEAICE_doOpenWaterGrowth) facOpenGrow=1. _d 0
        if (SEAICE_doOpenWaterMelt)   facOpenMelt=1. _d 0

C--   Initialize grid info
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM(i,j,bi,bj)       = 0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM(i,j,bi,bj)= 1. _d 0
          IF (_hFacC(i,j,kSurface,bi,bj).eq.0.)
     &         HEFFM(i,j,bi,bj)= 0. _d 0
         ENDDO
        ENDDO
#ifndef SEAICE_CGRID
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          UVM(i,j,bi,bj)=0. _d 0
          mask_uice=HEFFM(i,j,  bi,bj)+HEFFM(i-1,j-1,bi,bj)
     &             +HEFFM(i,j-1,bi,bj)+HEFFM(i-1,j,  bi,bj)
          IF(mask_uice.GT.3.5 _d 0) UVM(i,j,bi,bj)=1. _d 0
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
       ENDDO
      ENDDO

C     coefficients for metric terms
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_CGRID
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          k1AtC(I,J,bi,bj) = 0.0 _d 0
          k1AtZ(I,J,bi,bj) = 0.0 _d 0
          k2AtC(I,J,bi,bj) = 0.0 _d 0
          k2AtZ(I,J,bi,bj) = 0.0 _d 0
         ENDDO
        ENDDO
        IF ( usingSphericalPolarGrid .AND. SEAICEuseMetricTerms ) THEN
C     This is the only case where tan(phi) is not zero. In this case
C     C and U points, and Z and V points have the same phi, so that we
C     only need a copy here. Do not use tan(YC) and tan(YG), because these
C     can be the geographical coordinates and not the correct grid
C     coordinates when the grid is rotated (phi/theta/psiEuler .NE. 0)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtC(I,J,bi,bj) = - _tanPhiAtU(I,J,bi,bj)*recip_rSphere
           k2AtZ(I,J,bi,bj) = - _tanPhiAtV(I,J,bi,bj)*recip_rSphere
          ENDDO
         ENDDO
        ELSEIF ( usingCurvilinearGrid .AND. SEAICEuseMetricTerms ) THEN
C     compute metric term coefficients from finite difference approximation
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
           k1AtC(I,J,bi,bj) = _recip_dyF(I,J,bi,bj)
     &          * ( _dyG(I+1,J,bi,bj) - _dyG(I,J,bi,bj) )
     &          * _recip_dxF(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           k1AtZ(I,J,bi,bj) = _recip_dyU(I,J,bi,bj)
     &          * ( _dyC(I,J,bi,bj) - _dyC(I-1,J,bi,bj) )
     &          * _recip_dxV(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx
           k2AtC(I,J,bi,bj) = _recip_dxF(I,J,bi,bj)
     &          * ( _dxG(I,J+1,bi,bj) - _dxG(I,J,bi,bj) )
     &          * _recip_dyF(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtZ(I,J,bi,bj) = _recip_dxV(I,J,bi,bj)
     &          * ( _dxC(I,J,bi,bj) - _dxC(I,J-1,bi,bj) )
     &          * _recip_dyU(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#else /* not SEAICE_CGRID */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          k1AtC(I,J,bi,bj) = 0.0 _d 0
          k1AtU(I,J,bi,bj) = 0.0 _d 0
          k1AtV(I,J,bi,bj) = 0.0 _d 0
          k2AtC(I,J,bi,bj) = 0.0 _d 0
          k2AtU(I,J,bi,bj) = 0.0 _d 0
          k2AtV(I,J,bi,bj) = 0.0 _d 0
         ENDDO
        ENDDO
        IF ( usingSphericalPolarGrid .AND. SEAICEuseMetricTerms ) THEN
C     This is the only case where tan(phi) is not zero. In this case
C     C and U points, and Z and V points have the same phi, so that we
C     only need a copy here. Do not use tan(YC) and tan(YG), because these
C     can be the geographical coordinates and not the correct grid
C     coordinates when the grid is rotated (phi/theta/psiEuler .NE. 0)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtC(I,J,bi,bj) = - _tanPhiAtU(I,J,bi,bj)*recip_rSphere
           k2AtU(I,J,bi,bj) = - _tanPhiAtU(I,J,bi,bj)*recip_rSphere
           k2AtV(I,J,bi,bj) = - _tanPhiAtV(I,J,bi,bj)*recip_rSphere
          ENDDO
         ENDDO
        ELSEIF ( usingCurvilinearGrid .AND. SEAICEuseMetricTerms ) THEN
C     compute metric term coefficients from finite difference approximation
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
           k1AtC(I,J,bi,bj) = _recip_dyF(I,J,bi,bj)
     &          * ( _dyG(I+1,J,bi,bj) - _dyG(I,J,bi,bj) )
     &          * _recip_dxF(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           k1AtU(I,J,bi,bj) = _recip_dyG(I,J,bi,bj)
     &          * ( _dyF(I,J,bi,bj) - _dyF(I-1,J,bi,bj) )
     &          * _recip_dxC(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
           k1AtV(I,J,bi,bj) = _recip_dyC(I,J,bi,bj)
     &          * ( _dyU(I+1,J,bi,bj) - _dyU(I,J,bi,bj) )
     &          * _recip_dxG(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx
           k2AtC(I,J,bi,bj) = _recip_dxF(I,J,bi,bj)
     &          * ( _dxG(I,J+1,bi,bj) - _dxG(I,J,bi,bj) )
     &          * _recip_dyF(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx
           k2AtU(I,J,bi,bj) = _recip_dxC(I,J,bi,bj)
     &          * ( _dxV(I,J+1,bi,bj) - _dxV(I,J,bi,bj) )
     &          * _recip_dyG(I,J,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtV(I,J,bi,bj) = _recip_dxG(I,J,bi,bj)
     &          * ( _dxF(I,J,bi,bj) - _dxF(I,J-1,bi,bj) )
     &          * _recip_dyC(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* not SEAICE_CGRID */
       ENDDO
      ENDDO

#ifndef SEAICE_CGRID
C--   Choose a proxy level for geostrophic velocity,
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          KGEO(i,j,bi,bj)   = 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
#ifdef SEAICE_BICE_STRESS
          KGEO(i,j,bi,bj) = 1
#else /* SEAICE_BICE_STRESS */
          IF (klowc(i,j,bi,bj) .LT. 2) THEN
           KGEO(i,j,bi,bj) = 1
          ELSE
           KGEO(i,j,bi,bj) = 2
           DO WHILE ( abs(rC(KGEO(i,j,bi,bj))) .LT. 50.0 _d 0 .AND.
     &          KGEO(i,j,bi,bj) .LT. (klowc(i,j,bi,bj)-1) )
              KGEO(i,j,bi,bj) = KGEO(i,j,bi,bj) + 1
           ENDDO
          ENDIF
#endif /* SEAICE_BICE_STRESS */
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* SEAICE_CGRID */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL SEAICE_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

C--   Summarise pkg/seaice configuration
      CALL SEAICE_SUMMARY( myThid )
      
      RETURN
      END
