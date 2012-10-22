C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_init_fixed.F,v 1.20 2012/10/22 21:14:06 heimbach Exp $
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
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
CToM<<<
#ifdef SEAICE_ITD
C     msgBuf      :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*15 HlimitMsgFormat
C     k - loop counter for ITD categories
      INTEGER k
#endif
C>>>ToM
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, bi, bj
      INTEGER kSurface
#ifdef ALLOW_SITRACER
      INTEGER iTracer
#endif
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

C--   Set mcPheePiston coeff (if still unset)
      _BEGIN_MASTER(myThid)
      IF ( SEAICE_mcPheePiston.EQ.UNSET_RL ) THEN
        IF ( SEAICE_availHeatFrac.NE.UNSET_RL ) THEN
          SEAICE_mcPheePiston = SEAICE_availHeatFrac
     &                        * drF(kSurface)/SEAICE_deltaTtherm
        ELSE
          SEAICE_mcPheePiston = MCPHEE_TAPER_FAC
     &                        * STANTON_NUMBER * USTAR_BASE
          SEAICE_mcPheePiston = MIN( SEAICE_mcPheePiston,
     &                          drF(kSurface)/SEAICE_deltaTtherm )
        ENDIF
      ENDIF
      _END_MASTER(myThid)

C--   SItracer specifications for basic tracers
#ifdef ALLOW_SITRACER
      _BEGIN_MASTER(myThid)
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
      ENDDO
      _END_MASTER(myThid)
#endif

C--   all threads wait for master to finish initialisation of shared params
      _BARRIER

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

CToM<<<
#ifdef SEAICE_ITD
C     use Equ. 22 of Lipscomb et al. (2001, JGR) to generate ice
C     thickness category limits:
C     - dependends on given number of categories nITD
C     - choose between original parameters of Lipscomb et al. (2001):
C       Lipscomb: c1=3.0/N, c2=15*c1, c3=3.0
C       or emphasize thin end of ITD in order to enhance ice growth:
C       ToM:      c1=1.5/N, c2=42*c1, c3=3.3
C       (parameters c1 to c3 are set in seaice_readparms.F)
C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      Hlimit(0) = 0.0
      Hlimit_c1=Hlimit_c1/float(nITD)
      Hlimit_c2=Hlimit_c2*Hlimit_c1
      IF (nITD .gt. 1) THEN
       DO k=1,nITD-1
        Hlimit(k) = Hlimit(k-1)
     &            + Hlimit_c1
     &            + Hlimit_c2
     &  *( 1.+tanh( Hlimit_c3 *(float(k-1)/float(nITD) -1. ) ) )
       ENDDO
      ENDIF
C     thickest category is unlimited
      Hlimit(nITD) = 999.9

      WRITE(msgBuf,'(A,I2,A)')
     &     ' SEAICE_INIT_FIXED: ', nITD, 
     &     ' sea ice thickness categories'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)
      WRITE(HlimitMsgFormat,'(A,I2,A)') '(A,',nITD,'F6.2,F6.1)'
      WRITE(msgBuf,HlimitMsgFormat)
     &     ' SEAICE_INIT_FIXED: Hlimit = ',
     &     Hlimit(0:nITD-1),Hlimit(nITD)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , myThid)

#endif
C>>>ToM
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
