#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

CStartOfInterface
      SUBROUTINE SEAICE_INIT_VARIA( myThid )
C     *==========================================================*
C     | SUBROUTINE SEAICE_INIT_VARIA                             |
C     | o Initialization of sea ice model.                       |
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"
#include "SEAICE_TAVE.h"
#ifdef OBCS_UVICE_OLD
# include "OBCS_GRID.h"
#endif

C     === Routine arguments ===
C     myThid :: Thread no. that called this routine.
      INTEGER myThid
CEndOfInterface

C     === Local variables ===
C     i,j,k,bi,bj :: Loop counters

      INTEGER i, j, bi, bj
      INTEGER kSrf
      INTEGER k
#ifdef ALLOW_SITRACER
      INTEGER iTr, jTh
#endif
#ifdef OBCS_UVICE_OLD
      INTEGER I_obc, J_obc
#endif /* ALLOW_OBCS */
#ifdef SEAICE_CGRID
      _RS mask_uice
      _RL recip_tensilDepth
#endif

      IF ( usingPCoords ) THEN
       kSrf = Nr
      ELSE
       kSrf = 1
      ENDIF

C--   Initialise all variables in common blocks:
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(i,j,bi,bj)=0. _d 0
          AREA(i,j,bi,bj)=0. _d 0
          HSNOW(i,j,bi,bj)   = 0. _d 0
#ifdef SEAICE_ITD
          DO k=1,nITD
           AREAITD(i,j,k,bi,bj) =0. _d 0
           HEFFITD(i,j,k,bi,bj) =0. _d 0
           HSNOWITD(i,j,k,bi,bj)=0. _d 0
          ENDDO
#endif
          UICE(i,j,bi,bj)=0. _d 0
          VICE(i,j,bi,bj)=0. _d 0
C
          uIceNm1(i,j,bi,bj) = 0. _d 0
          vIceNm1(i,j,bi,bj) = 0. _d 0
          DWATN  (i,j,bi,bj) = 0. _d 0
#ifdef SEAICE_CGRID
          stressDivergenceX(i,j,bi,bj) = 0. _d 0
          stressDivergenceY(i,j,bi,bj) = 0. _d 0
# ifdef SEAICE_ALLOW_EVP
          seaice_sigma1 (i,j,bi,bj) = 0. _d 0
          seaice_sigma2 (i,j,bi,bj) = 0. _d 0
          seaice_sigma12(i,j,bi,bj) = 0. _d 0
# endif /* SEAICE_ALLOW_EVP */
#endif
#if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
          e11    (i,j,bi,bj) = 0. _d 0
          e22    (i,j,bi,bj) = 0. _d 0
          e12    (i,j,bi,bj) = 0. _d 0
          deltaC (i,j,bi,bj) = 0. _d 0
          PRESS  (i,j,bi,bj) = 0. _d 0
          ETA    (i,j,bi,bj) = 0. _d 0
          etaZ   (i,j,bi,bj) = 0. _d 0
          ZETA   (i,j,bi,bj) = 0. _d 0
          FORCEX (i,j,bi,bj) = 0. _d 0
          FORCEY (i,j,bi,bj) = 0. _d 0
          tensileStrFac(i,j,bi,bj) = 0. _d 0
          PRESS0(i,j,bi,bj)  = 0. _d 0
          FORCEX0(i,j,bi,bj) = 0. _d 0
          FORCEY0(i,j,bi,bj) = 0. _d 0
          SEAICE_zMax(i,j,bi,bj) = 0. _d 0
          SEAICE_zMin(i,j,bi,bj) = 0. _d 0
#endif
#ifdef SEAICE_CGRID
          seaiceMassC(i,j,bi,bj)=0. _d 0
          seaiceMassU(i,j,bi,bj)=0. _d 0
          seaiceMassV(i,j,bi,bj)=0. _d 0
# ifdef SEAICE_ALLOW_FREEDRIFT
          uice_fd(i,j,bi,bj) = 0. _d 0
          vice_fd(i,j,bi,bj) = 0. _d 0
# endif
# ifdef SEAICE_ALLOW_BOTTOMDRAG
          CbotC(i,j,bi,bj)   = 0. _d 0
# endif /* SEAICE_ALLOW_BOTTOMDRAG */
# ifdef SEAICE_ALLOW_SIDEDRAG
          sideDragU(i,j,bi,bj) = 0. _d 0
          sideDragV(i,j,bi,bj) = 0. _d 0
# endif /* SEAICE_ALLOW_SIDEDRAG */
#endif /* SEAICE_CGRID */
#ifdef SEAICE_BGRID_DYNAMICS
          uIceB(i,j,bi,bj)   = 0. _d 0
          vIceB(i,j,bi,bj)   = 0. _d 0
          AMASS(i,j,bi,bj)   = 0. _d 0
          DAIRN(i,j,bi,bj)   = 0. _d 0
          WINDX(i,j,bi,bj)   = 0. _d 0
          WINDY(i,j,bi,bj)   = 0. _d 0
          GWATX(i,j,bi,bj)   = 0. _d 0
          GWATY(i,j,bi,bj)   = 0. _d 0
#endif /* SEAICE_BGRID_DYNAMICS */
#ifdef SEAICE_VARIABLE_SALINITY
          HSALT(i,j,bi,bj)  = 0. _d 0
#endif
#ifdef ALLOW_SITRACER
          DO iTr = 1, SItrMaxNum
           SItracer(i,j,bi,bj,iTr) = 0. _d 0
           SItrBucket(i,j,bi,bj,iTr) = 0. _d 0
c "ice concentration" tracer that should remain .EQ.1.
           if (SItrName(iTr).EQ.'one') SItracer(i,j,bi,bj,iTr)=1. _d 0
          ENDDO
          DO jTh = 1, 5
           SItrHEFF (i,j,bi,bj,jTh) = 0. _d 0
          ENDDO
          DO jTh = 1, 3
           SItrAREA (i,j,bi,bj,jTh) = 0. _d 0
          ENDDO
#endif
          DO k=1,nITD
            TICES(i,j,k,bi,bj)=0. _d 0
          ENDDO
          saltWtrIce(i,j,bi,bj) = 0. _d 0
          frWtrIce(i,j,bi,bj)   = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF
C- Note: To simplify dependency & avoid recomputations, when compiling
C        pkg/autodiff, we always call SEAICE_INIT_VARIA to initialise control
C        variables (as done above) without condition on useSEAICE.
C        Therefore, in this case, the "If useSEAICE" is added back here:
      IF ( useSEAICE ) THEN
#endif

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET( FUtave   , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( FVtave   , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( EmPmRtave, 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( QNETtave , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( QSWtave  , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( UICEtave , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( VICEtave , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( HEFFtave , 1, bi, bj, myThid )
        CALL TIMEAVE_RESET( AREAtave , 1, bi, bj, myThid )
        SEAICE_timeAve(bi,bj) = ZERO
       ENDDO
      ENDDO
#endif /* ALLOW_TIMEAVE */

C--   Initialize (variable) grid info. As long as we allow masking of
C--   velocities outside of ice covered areas (in seaice_dynsolver)
C--   we need to re-initialize seaiceMaskU/V here for TAF
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_CGRID
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          seaiceMaskU(i,j,bi,bj)=   0.0 _d 0
          seaiceMaskV(i,j,bi,bj)=   0.0 _d 0
          mask_uice=HEFFM(i,j,bi,bj)+HEFFM(i-1,j  ,bi,bj)
          IF(mask_uice.GT.1.5 _d 0) seaiceMaskU(i,j,bi,bj)=1.0 _d 0
          mask_uice=HEFFM(i,j,bi,bj)+HEFFM(i  ,j-1,bi,bj)
          IF(mask_uice.GT.1.5 _d 0) seaiceMaskV(i,j,bi,bj)=1.0 _d 0
         ENDDO
        ENDDO
# ifdef OBCS_UVICE_OLD
        IF (useOBCS) THEN
C--   If OBCS is turned on, close southern and western boundaries
         DO i=1-OLx,sNx+OLx
C Southern boundary
          J_obc = OB_Js(i,bi,bj)
          IF ( J_obc.NE.OB_indexNone ) THEN
           seaiceMaskU(i,J_obc,bi,bj)=   0.0 _d 0
           seaiceMaskV(i,J_obc,bi,bj)=   0.0 _d 0
          ENDIF
         ENDDO
         DO j=1-OLy,sNy+OLy
C Western boundary
          I_obc = OB_Iw(j,bi,bj)
          IF ( I_obc.NE.OB_indexNone ) THEN
           seaiceMaskU(I_obc,j,bi,bj)=   0.0 _d 0
           seaiceMaskV(I_obc,j,bi,bj)=   0.0 _d 0
          ENDIF
         ENDDO
        ENDIF
# endif /* OBCS_UVICE_OLD */
#endif /* SEAICE_CGRID */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DO k=1,nITD
           TICES(i,j,k,bi,bj)=273.0 _d 0
          ENDDO
#ifdef SEAICE_CGRID
          seaiceMassC(i,j,bi,bj)=1000.0 _d 0
          seaiceMassU(i,j,bi,bj)=1000.0 _d 0
          seaiceMassV(i,j,bi,bj)=1000.0 _d 0
#endif
#ifdef SEAICE_BGRID_DYNAMICS
          AMASS      (i,j,bi,bj)=1000.0 _d 0
#endif
         ENDDO
        ENDDO

       ENDDO
      ENDDO

C--   Update overlap regions
#ifdef SEAICE_CGRID
      CALL EXCH_UV_XY_RL(seaiceMaskU,seaiceMaskV,.FALSE.,myThid)
#endif
#ifdef SEAICE_BGRID_DYNAMICS
      _EXCH_XY_RS(UVM, myThid)
#endif

C--   Now lets look at all these beasts
      IF ( plotLevel.GE.debLevC ) THEN
         CALL PLOT_FIELD_XYRL( HEFFM   , 'Current HEFFM   ' ,
     &        nIter0, myThid )
#ifdef SEAICE_CGRID
         CALL PLOT_FIELD_XYRL( seaiceMaskU, 'Current seaiceMaskU',
     &        nIter0, myThid )
         CALL PLOT_FIELD_XYRL( seaiceMaskV, 'Current seaiceMaskV',
     &        nIter0, myThid )
#endif
#ifdef SEAICE_BGRID_DYNAMICS
         CALL PLOT_FIELD_XYRS( UVM     , 'Current UVM     ' ,
     &        nIter0, myThid )
#endif
      ENDIF

C--   Set model variables to initial/restart conditions
      IF ( .NOT. ( startTime .EQ. baseTime .AND.  nIter0 .EQ. 0
     &     .AND. pickupSuff .EQ. ' ') ) THEN

         CALL SEAICE_READ_PICKUP ( myThid )

      ELSE

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HEFF(i,j,bi,bj)=SEAICE_initialHEFF*HEFFM(i,j,bi,bj)
           UICE(i,j,bi,bj)=ZERO
           VICE(i,j,bi,bj)=ZERO
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea-ice velocity from file (if available)
       IF ( uIceFile .NE. ' ' )
     &  CALL READ_FLD_XY_RL( uIceFile, ' ', uIce, 0, myThid )
       IF ( vIceFile .NE. ' ' )
     &  CALL READ_FLD_XY_RL( vIceFile, ' ', vIce, 0, myThid )
       IF ( uIceFile .NE. ' ' .OR. vIceFile .NE. ' ' ) THEN
#ifdef SEAICE_CGRID
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            uIce(i,j,bi,bj) = uIce(i,j,bi,bj)*seaiceMaskU(i,j,bi,bj)
            vIce(i,j,bi,bj) = vIce(i,j,bi,bj)*seaiceMaskV(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
        CALL EXCH_UV_XY_RL( uIce, vIce, .TRUE., myThid )
       ENDIF

C--   Read initial sea-ice thickness from file if available.
       IF ( HeffFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( HeffFile, ' ', HEFF, 0, myThid )
        _EXCH_XY_RL(HEFF,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HEFF(i,j,bi,bj) = MAX(HEFF(i,j,bi,bj),ZERO)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF(HEFF(i,j,bi,bj).GT.ZERO) AREA(i,j,bi,bj)=ONE
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea-ice area from file if available.
       IF ( AreaFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( AreaFile, ' ', AREA, 0, myThid )
        _EXCH_XY_RL(AREA,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            AREA(i,j,bi,bj) = MAX(AREA(i,j,bi,bj),ZERO)
            AREA(i,j,bi,bj) = MIN(AREA(i,j,bi,bj),ONE)
            IF ( AREA(i,j,bi,bj) .LE. ZERO ) HEFF(i,j,bi,bj) = ZERO
            IF ( HEFF(i,j,bi,bj) .LE. ZERO ) AREA(i,j,bi,bj) = ZERO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HSNOW(i,j,bi,bj) = 0.2 _d 0 * AREA(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial snow thickness from file if available.
       IF ( HsnowFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( HsnowFile, ' ', HSNOW, 0, myThid )
        _EXCH_XY_RL(HSNOW,myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            HSNOW(i,j,bi,bj) = MAX(HSNOW(i,j,bi,bj),ZERO)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

#ifdef SEAICE_ITD
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           AREAITD(i,j,1,bi,bj)   = AREA(i,j,bi,bj)
           HEFFITD(i,j,1,bi,bj)   = HEFF(i,j,bi,bj)
           HSNOWITD(i,j,1,bi,bj)  = HSNOW(i,j,bi,bj)
           opnWtrFrac(i,j,bi,bj)  = 1. _d 0 - AREA(i,j,bi,bj)
           fw2ObyRidge(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
         CALL SEAICE_ITD_REDIST(bi, bj, baseTime, nIter0, myThid)
        ENDDO
       ENDDO
#endif

#ifdef SEAICE_VARIABLE_SALINITY
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           HSALT(i,j,bi,bj)=HEFF(i,j,bi,bj)*salt(i,j,kSrf,bi,bj)*
     &            SEAICE_rhoIce*SEAICE_saltFrac
cif     &            ICE2WATR*rhoConstFresh*SEAICE_saltFrac

          ENDDO
         ENDDO
        ENDDO
       ENDDO

C--   Read initial sea ice salinity from file if available.
       IF ( HsaltFile .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( HsaltFile, ' ', HSALT, 0, myThid )
        _EXCH_XY_RL(HSALT,myThid)
       ENDIF
#endif /* SEAICE_VARIABLE_SALINITY */

#ifdef ALLOW_SITRACER
C--   Read initial sea ice age from file if available.
       DO iTr = 1, SItrMaxNum
        IF ( SItrFile(iTr) .NE. ' ' ) THEN
        CALL READ_FLD_XY_RL( siTrFile(iTr), ' ',
     &   SItracer(1-OLx,1-OLy,1,1,iTr), 0, myThid )
        _EXCH_XY_RL(SItracer(1-OLx,1-OLy,1,1,iTr),myThid)
        ENDIF
       ENDDO
#endif /* ALLOW_SITRACER */

      ENDIF

#ifdef ALLOW_OBCS
C--   In case we use scheme with a large stencil that extends into overlap:
C     no longer needed with the right masking in advection & diffusion S/R.
c     IF ( useOBCS ) THEN
c       DO bj=myByLo(myThid),myByHi(myThid)
c        DO bi=myBxLo(myThid),myBxHi(myThid)
c          CALL OBCS_COPY_TRACER( HEFF(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
c          CALL OBCS_COPY_TRACER( AREA(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
c          CALL OBCS_COPY_TRACER( HSNOW(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
#ifdef SEAICE_VARIABLE_SALINITY
c          CALL OBCS_COPY_TRACER( HSALT(1-OLx,1-OLy,bi,bj),
c    I                            1, bi, bj, myThid )
#endif
c        ENDDO
c       ENDDO
c     ENDIF
#endif /* ALLOW_OBCS */

C---  Complete initialization
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#if ( defined SEAICE_CGRID || defined SEAICE_BGRID_DYNAMICS )
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ZETA(i,j,bi,bj)        = HEFF(i,j,bi,bj)*(1.0 _d 11)
          ETA(i,j,bi,bj)         = ZETA(i,j,bi,bj)/SEAICE_eccen**2
          PRESS0(i,j,bi,bj)      = SEAICE_strength*HEFF(i,j,bi,bj)
     &         *EXP(-SEAICE_cStar*(ONE-AREA(i,j,bi,bj)))
          SEAICE_zMax(i,j,bi,bj) = SEAICE_zetaMaxFac*PRESS0(i,j,bi,bj)
          SEAICE_zMin(i,j,bi,bj) = SEAICE_zetaMin
          PRESS0(i,j,bi,bj)      = PRESS0(i,j,bi,bj)*HEFFM(i,j,bi,bj)
         ENDDO
        ENDDO
#endif
        IF ( useRealFreshWaterFlux .AND. .NOT.useThSIce ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           sIceLoad(i,j,bi,bj) = HEFF(i,j,bi,bj)*SEAICE_rhoIce
     &                         + HSNOW(i,j,bi,bj)*SEAICE_rhoSnow

          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#ifdef SEAICE_CGRID
C     compute tensile strength factor k: tensileStrength = k*PRESS
C     can be done in initialisation phase as long as it depends only
C     on depth
      IF ( SEAICE_tensilFac .NE. 0. _d 0 ) THEN
       recip_tensilDepth = 0. _d 0
       IF ( SEAICE_tensilDepth .GT. 0. _d 0 )
     &      recip_tensilDepth = 1. _d 0 / SEAICE_tensilDepth
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           tensileStrFac(i,j,bi,bj) = SEAICE_tensilFac*HEFFM(i,j,bi,bj)
     &          *exp(-ABS(R_low(i,j,bi,bj))*recip_tensilDepth)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
# if ( defined SEAICE_ALLOW_JFNK || defined SEAICE_ALLOW_KRYLOV )
C     Computing this metric cannot be done in S/R SEAICE_INIT_FIXED
C     where it belongs, because globalArea is only defined later after
C     S/R PACKAGES_INIT_FIXED, so we move this computation here.
      CALL SEAICE_MAP_RS2VEC( nVec, rAw, rAs,
     &     scalarProductMetric, .TRUE., myThid )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO i=1,nVec
         scalarProductMetric(i,1,bi,bj) =
     &        scalarProductMetric(i,1,bi,bj)/globalArea
        ENDDO
       ENDDO
      ENDDO
# endif /* SEAICE_ALLOW_JFNK or KRYLOV */

#endif /* SEAICE_CGRID */

#ifdef ALLOW_AUTODIFF
C-    end if useSEAICE block
      ENDIF
#endif

      RETURN
      END
