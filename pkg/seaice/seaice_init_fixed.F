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
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
#ifdef SEAICE_ITD
C     msgBuf      :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
C     k - loop counter for ITD categories
      INTEGER k
      _RL tmpVar
      LOGICAL computeHlimit
#endif
C     i,j, bi,bj  :: Loop counters
      INTEGER i, j, bi, bj
      INTEGER kSrf
#ifdef ALLOW_SITRACER
      INTEGER iTracer
#endif
#ifdef SHORTWAVE_HEATING
cif   Helper variable for determining the fraction of sw radiation
cif   penetrating the model shallowest layer
      _RL dummyTime
      _RL swfracba(2)
      _RL tmpFac
#endif /* SHORTWAVE_HEATING */
C     local copy of surface layer thickness in meters
      _RL dzSurf
#ifndef SEAICE_CGRID
      _RL mask_uice
#endif

      IF ( usingPCoords ) THEN
       kSrf        = Nr
      ELSE
       kSrf        = 1
      ENDIF

C     Initialize MNC variable information for SEAICE
      IF ( useMNC .AND.
     &    (seaice_tave_mnc.OR.seaice_dump_mnc.OR.SEAICE_mon_mnc)
     &   ) THEN
        CALL SEAICE_MNC_INIT( myThid )
      ENDIF

C     Only Master Thread updates parameter in commom block:
      _BEGIN_MASTER(myThid)

C     restart parameter
      SEAICEmomStartBDF = 0
      IF ( SEAICEuseBDF2 ) SEAICEmomStartBDF = nIter0

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

C--   Set mcPheePiston coeff (if still unset)
       dzSurf = drF(kSrf)
       IF ( usingPCoords )
     &      dzSurf = drF(kSrf) * recip_rhoConst * recip_gravity
      IF ( SEAICE_mcPheePiston.EQ.UNSET_RL ) THEN
        IF ( SEAICE_availHeatFrac.NE.UNSET_RL ) THEN
          SEAICE_mcPheePiston = SEAICE_availHeatFrac
     &                        * dzSurf/SEAICE_deltaTtherm
        ELSE
          SEAICE_mcPheePiston = MCPHEE_TAPER_FAC
     &                        * STANTON_NUMBER * USTAR_BASE
          SEAICE_mcPheePiston = MIN( SEAICE_mcPheePiston,
     &                          dzSurf/SEAICE_deltaTtherm )
        ENDIF

      ENDIF

C--   SItracer specifications for basic tracers
#ifdef ALLOW_SITRACER
      DO iTracer = 1, SItrNumInUse
C     "ice concentration" tracer that should remain .EQ.1.
       IF (SItrName(iTracer).EQ.'one') THEN
         SItrFromOcean0(iTracer)    =ONE
         SItrFromFlood0(iTracer)    =ONE
         SItrExpand0(iTracer)       =ONE
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIF
C     age tracer: no age in ocean, or effect from ice cover changes
       IF (SItrName(iTracer).EQ.'age') THEN
         SItrFromOcean0(iTracer)    =ZERO
         SItrFromFlood0(iTracer)    =ZERO
         SItrExpand0(iTracer)       =ZERO
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIf
C     salinity tracer:
       IF (SItrName(iTracer).EQ.'salinity') THEN
         SItrMate(iTracer)          ='HEFF'
         SItrExpand0(iTracer)       =ZERO
         IF ( SEAICE_salinityTracer ) THEN
           SEAICE_salt0    = ZERO
           SEAICE_saltFrac = ZERO
         ENDIF
       ENDIF
C     simple, made up, ice surface roughness index prototype
       IF (SItrName(iTracer).EQ.'ridge') THEN
         SItrMate(iTracer)          ='AREA'
         SItrFromOcean0(iTracer)    =ZERO
         SItrFromFlood0(iTracer)    =ZERO
         SItrExpand0(iTracer)       =ZERO
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIF
#ifdef SEAICE_GREASE
C     grease ice tracer:
c     (Smedrud and Martin, 2014, Ann. Glac.)
       IF (SItrName(iTracer).EQ.'grease') THEN
         SItrMate(iTracer)          ='HEFF'
         SItrFromOcean0(iTracer)    =ZERO
         SItrFromFlood0(iTracer)    =ZERO
         SItrExpand0(iTracer)       =ZERO
         SItrFromOceanFrac(iTracer) =ZERO
         SItrFromFloodFrac(iTracer) =ZERO
       ENDIF
#endif /* SEAICE_GREASE */
      ENDDO
#endif /* ALLOW_SITRACER */

#ifdef SEAICE_ITD
C     zeroth category needs to be zero
      Hlimit(0)    = 0. _d 0
C     thickest category is unlimited
      Hlimit(nITD) = 999.9 _d 0
      computeHlimit=.FALSE.
C     check if Hlimit contains useful values
      DO k = 1, nITD
       IF ( Hlimit(k).EQ.UNSET_RL )    computeHlimit=.TRUE.
       IF ( Hlimit(k).LE.Hlimit(k-1) ) computeHlimit=.TRUE.
      ENDDO
      IF ( computeHlimit ) THEN
       WRITE(msgBuf,'(A,I2,A)') 'SEAICE_INIT_FIXED: Computing ',
     &      nITD, ' thickness category limits with'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       CALL WRITE_0D_RL( Hlimit_c1  ,INDEX_NONE,
     &      'Hlimit_c1  =', ' /* ITD bin parameter */')
       CALL WRITE_0D_RL( Hlimit_c2  ,INDEX_NONE,
     &      'Hlimit_c2  =', ' /* ITD bin parameter */')
       CALL WRITE_0D_RL( Hlimit_c3  ,INDEX_NONE,
     &      'Hlimit_c3  =', ' /* ITD bin parameter */')
       WRITE(msgBuf,'(A)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
C     use Equ. 22 of Lipscomb et al. (2001, JGR) to generate ice
C     thickness category limits:
C     - dependends on given number of categories nITD
C     - choose between original parameters of Lipscomb et al. (2001):
C       c1=3.0/N, c2=15*c1, c3=3.0
C       or emphasize thin end of ITD (in order to enhance ice growth):
C       c1=1.5/N, c2=42*c1, c3=3.3
C       -> HINT: set parameters c1, c2 and c3 in seaice_readparms.F
       IF ( nITD.GT.1 ) THEN
        tmpVar = nITD
        tmpVar = oneRL / tmpVar
        Hlimit_c1 = Hlimit_c1*tmpVar
        Hlimit_c2 = Hlimit_c2*Hlimit_c1
        DO k=1,nITD-1
         Hlimit(k) = Hlimit(k-1)
     &             + Hlimit_c1
     &             + Hlimit_c2
     &    *( oneRL + TANH( Hlimit_c3 *( FLOAT(k-1)*tmpVar - oneRL ) ) )
        ENDDO
       ENDIF
      ENDIF
C     thickest category is unlimited
      Hlimit(nITD) = 999.9 _d 0

#endif /* SEAICE_ITD */

C     Only Master Thread updates parameter in common block:
      _END_MASTER(myThid)

#ifdef SEAICE_ALLOW_JFNK
C     Only Master Thread updates parameter in common block:
      _BEGIN_MASTER(myThid)
C     initialise some diagnostic counters for the JFNK solver
      totalNewtonIters   = 0
      totalNewtonFails   = 0
      totalKrylovIters   = 0
      totalKrylovFails   = 0
      totalJFNKtimeSteps = 0
      _END_MASTER(myThid)
C     this cannot be done here, because globalArea is only defined
C     after S/R PACKAGES_INIT_FIXED, so we move it to S/R SEAICE_INIT_VARIA
CML      CALL SEAICE_MAP2VEC( nVec, rAw, rAs,
CML     &     scalarProductMetric, .TRUE., myThid )
CML      DO bj=myByLo(myThid),myByHi(myThid)
CML       DO bi=myBxLo(myThid),myBxHi(myThid)
CML        DO i=1,nVec
CML         scalarProductMetric(i,1,bi,bj) =
CML     &        scalarProductMetric(i,1,bi,bj)/globalArea
CML        ENDDO
CML       ENDDO
CML      ENDDO
#endif /* SEAICE_ALLOW_JFNK */

C--   all threads wait for master to finish initialisation of shared params
      _BARRIER

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL SEAICE_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

C--   Summarise pkg/seaice configuration
      CALL SEAICE_SUMMARY( myThid )

C--   Initialise grid parameters that do no change during the integration

C--   Initialise grid info
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C-    loops on tile indices bi,bj:
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM  (i,j,bi,bj) = 0. _d 0
          SIMaskU(i,j,bi,bj) = 0. _d 0
          SIMaskV(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFFM  (i,j,bi,bj) = maskC(i,j,kSrf,bi,bj)
          SIMaskU(i,j,bi,bj) = maskW(i,j,kSrf,bi,bj)
          SIMaskV(i,j,bi,bj) = maskS(i,j,kSrf,bi,bj)
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
#endif /* ndef SEAICE_CGRID */

C     coefficients for metric terms
#ifdef SEAICE_CGRID
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          k1AtC(i,j,bi,bj) = 0.0 _d 0
          k1AtZ(i,j,bi,bj) = 0.0 _d 0
          k2AtC(i,j,bi,bj) = 0.0 _d 0
          k2AtZ(i,j,bi,bj) = 0.0 _d 0
         ENDDO
        ENDDO
        IF ( usingSphericalPolarGrid .AND. SEAICEuseMetricTerms ) THEN
C     This is the only case where tan(phi) is not zero. In this case
C     C and U points, and Z and V points have the same phi, so that we
C     only need a copy here. Do not use tan(YC) and tan(YG), because
C     these
C     can be the geographical coordinates and not the correct grid
C     coordinates when the grid is rotated (phi/theta/psiEuler .NE. 0)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtC(i,j,bi,bj) = - _tanPhiAtU(i,j,bi,bj)*recip_rSphere
           k2AtZ(i,j,bi,bj) = - _tanPhiAtV(i,j,bi,bj)*recip_rSphere
          ENDDO
         ENDDO
        ELSEIF ( usingCurvilinearGrid .AND. SEAICEuseMetricTerms ) THEN
C     compute metric term coefficients from finite difference
C     approximation
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
           k1AtC(i,j,bi,bj) = _recip_dyF(i,j,bi,bj)
     &          * ( _dyG(i+1,j,bi,bj) - _dyG(i,j,bi,bj) )
     &          * _recip_dxF(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           k1AtZ(i,j,bi,bj) = _recip_dyU(i,j,bi,bj)
     &          * ( _dyC(i,j,bi,bj) - _dyC(i-1,j,bi,bj) )
     &          * _recip_dxV(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx
           k2AtC(i,j,bi,bj) = _recip_dxF(i,j,bi,bj)
     &          * ( _dxG(i,j+1,bi,bj) - _dxG(i,j,bi,bj) )
     &          * _recip_dyF(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtZ(i,j,bi,bj) = _recip_dxV(i,j,bi,bj)
     &          * ( _dxC(i,j,bi,bj) - _dxC(i,j-1,bi,bj) )
     &          * _recip_dyU(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#else /* not SEAICE_CGRID */
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          k1AtC(i,j,bi,bj) = 0.0 _d 0
          k1AtU(i,j,bi,bj) = 0.0 _d 0
          k1AtV(i,j,bi,bj) = 0.0 _d 0
          k2AtC(i,j,bi,bj) = 0.0 _d 0
          k2AtU(i,j,bi,bj) = 0.0 _d 0
          k2AtV(i,j,bi,bj) = 0.0 _d 0
         ENDDO
        ENDDO
        IF ( usingSphericalPolarGrid .AND. SEAICEuseMetricTerms ) THEN
C     This is the only case where tan(phi) is not zero. In this case
C     C and U points, and Z and V points have the same phi, so that we
C     only need a copy here. Do not use tan(YC) and tan(YG), because
C     these
C     can be the geographical coordinates and not the correct grid
C     coordinates when the grid is rotated (phi/theta/psiEuler .NE. 0)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtC(i,j,bi,bj) = - _tanPhiAtU(i,j,bi,bj)*recip_rSphere
           k2AtU(i,j,bi,bj) = - _tanPhiAtU(i,j,bi,bj)*recip_rSphere
           k2AtV(i,j,bi,bj) = - _tanPhiAtV(i,j,bi,bj)*recip_rSphere
          ENDDO
         ENDDO
        ELSEIF ( usingCurvilinearGrid .AND. SEAICEuseMetricTerms ) THEN
C     compute metric term coefficients from finite difference
C     approximation
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
           k1AtC(i,j,bi,bj) = _recip_dyF(i,j,bi,bj)
     &          * ( _dyG(i+1,j,bi,bj) - _dyG(i,j,bi,bj) )
     &          * _recip_dxF(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           k1AtU(i,j,bi,bj) = _recip_dyG(i,j,bi,bj)
     &          * ( _dyF(i,j,bi,bj) - _dyF(i-1,j,bi,bj) )
     &          * _recip_dxC(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
           k1AtV(i,j,bi,bj) = _recip_dyC(i,j,bi,bj)
     &          * ( _dyU(i+1,j,bi,bj) - _dyU(i,j,bi,bj) )
     &          * _recip_dxG(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx
           k2AtC(i,j,bi,bj) = _recip_dxF(i,j,bi,bj)
     &          * ( _dxG(i,j+1,bi,bj) - _dxG(i,j,bi,bj) )
     &          * _recip_dyF(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx,sNx+OLx
           k2AtU(i,j,bi,bj) = _recip_dxC(i,j,bi,bj)
     &          * ( _dxV(i,j+1,bi,bj) - _dxV(i,j,bi,bj) )
     &          * _recip_dyG(i,j,bi,bj)
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx
           k2AtV(i,j,bi,bj) = _recip_dxG(i,j,bi,bj)
     &          * ( _dxF(i,j,bi,bj) - _dxF(i,j-1,bi,bj) )
     &          * _recip_dyC(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* not SEAICE_CGRID */
C-    end bi,bj loops
       ENDDO
      ENDDO

#ifdef ALLOW_SHELFICE
      IF ( useShelfIce ) THEN
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
C--   Prevent seaice to form where Ice-Shelf is present
          CALL SHELFICE_MASK_SEAICE(
     U                  HEFFM,
     I                  bi, bj, -1, myThid )
          DO j=2-OLy,sNy+OLy
           DO i=2-OLx,sNx+OLx
            IF ( HEFFM(i-1,j,bi,bj).EQ.zeroRL .OR.
     &           HEFFM( i, j,bi,bj).EQ.zeroRL )
     &           SIMaskU(i,j,bi,bj) = zeroRL
            IF ( HEFFM(i,j-1,bi,bj).EQ.zeroRL .OR.
     &           HEFFM(i, j, bi,bj).EQ.zeroRL )
     &           SIMaskV(i,j,bi,bj) = zeroRL
           ENDDO
          ENDDO
        ENDDO
       ENDDO
       CALL EXCH_UV_XY_RL( SIMaskU, SIMaskV, .FALSE., myThid )
      ENDIF
#endif /* ALLOW_SHELFICE */

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
#endif /* ndef SEAICE_CGRID */

      RETURN
      END
