#include "THSICE_OPTIONS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: THSICE_ADVDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE THSICE_ADVDIFF(
     U                  uIce, vIce,
     I                  bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE THSICE_ADVDIFF
C     | o driver for different advection routines
C     |   calls an adaption of gad_advection to call different
C     |   advection routines of pkg/generic_advdiff
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
C   oceFWfx   :: fresh water flux to the ocean  [kg/m^2/s]
C   oceSflx   :: salt flux to the ocean         [g/m^2/s]
C   oceQnet   :: heat flux to the ocean         [W/m^2]

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "THSICE_SIZE.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#ifdef ALLOW_GENERIC_ADVDIFF
# include "GAD.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     uIce/vIce :: ice velocity on C-grid [m/s]
C     bi,bj     :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id. number
      _RL     uIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vIce  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_THSICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,      :: Loop counters
C     uTrans    :: sea-ice area transport, x direction
C     vTrans    :: sea-ice area transport, y direction
C     uTrIce    :: sea-ice volume transport, x direction
C     vTrIce    :: sea-ice volume transport, y direction
C     afx       :: horizontal advective flux, x direction
C     afy       :: horizontal advective flux, y direction
C     iceFrc    :: (new) sea-ice fraction
C     iceVol    :: temporary array used in advection S/R
C     oldVol    :: (old) sea-ice volume
C     msgBuf    :: Informational/error message buffer
      INTEGER i, j
      LOGICAL thSIce_multiDimAdv
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uTrIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrIce    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS maskOce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceFrc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceVol    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oldVol    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceTmp    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL r_minArea
      _RL meanCellArea, areaEpsil, vol_Epsil
#ifdef ALLOW_DIAGNOSTICS
      CHARACTER*8 diagName
      CHARACTER*4 THSICE_DIAG_SUFX, diagSufx
      EXTERNAL    THSICE_DIAG_SUFX
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
      _RL tmpFac
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey
#endif
#ifdef ALLOW_DBUG_THSICE
      _RL tmpVar, sumVar1, sumVar2
#endif
      LOGICAL dBugFlag
#include "THSICE_DEBUG.h"
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act1 + 1) + act2*max1
     &                     + act3*max1*max2
     &                     + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C     areaEpsil, vol_Epsil are 2 small numbers for ice area & ice volume:
C     if ice area (=ice fraction * grid-cell area) or ice volume (= effective
C     thickness * grid-cell area) are too small (i.e.: < areaEpsil,vol_Epsil)
C     will assume that ice is gone, and will loose mass or energy.
C     However, if areaEpsil,vol_Epsil are much smaller than minimun ice area
C     (iceMaskMin*rAc) and minimum ice volume (iceMaskMin*hIceMin*rAc),
C     good chance that this will never happen within 1 time step.

      dBugFlag = debugLevel.GE.debLevC

C-    definitively not an accurate computation of mean grid-cell area;
C     but what matter here is just to have the right order of magnitude.
      meanCellArea = Nx*Ny
      meanCellArea = globalArea / meanCellArea
      areaEpsil = 1. _d -10 * meanCellArea
      vol_Epsil = 1. _d -15 * meanCellArea

      r_minArea = 0. _d 0
      IF ( iceMaskMin.GT.0. _d 0 ) r_minArea = 1. _d 0 / iceMaskMin

      thSIce_multiDimAdv = .TRUE.
#ifdef ALLOW_GENERIC_ADVDIFF
      IF ( thSIceAdvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.thSIceAdvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.thSIceAdvScheme.EQ.ENUM_CENTERED_4TH ) THEN
       thSIce_multiDimAdv = .FALSE.
      ENDIF
#endif /* ALLOW_GENERIC_ADVDIFF */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(iceMask,'SI_AdvFr',0,1,1,bi,bj,myThid)
C-     Ice-fraction weighted quantities:
        tmpFac = 1. _d 0
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   iceHeight, iceMask,tmpFac,1,'SI_AdvHi',
     I                   0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FRACT_FILL(
     I                   snowHeight,iceMask,tmpFac,1,'SI_AdvHs',
     I                   0,1,1,bi,bj,myThid)
C-     Ice-Volume weighted quantities:
        IF ( DIAGNOSTICS_IS_ON('SI_AdvQ1',myThid) .OR.
     &       DIAGNOSTICS_IS_ON('SI_AdvQ2',myThid) ) THEN
         DO j=1,sNy
          DO i=1,sNx
           iceTmp(i,j) = iceMask(i,j,bi,bj)*iceHeight(i,j,bi,bj)
          ENDDO
         ENDDO
         CALL DIAGNOSTICS_FRACT_FILL(
     I                   Qice1(1-OLx,1-OLy,bi,bj),
     I                   iceTmp,tmpFac,1,'SI_AdvQ1',
     I                   0,1,2,bi,bj,myThid)
         CALL DIAGNOSTICS_FRACT_FILL(
     I                   Qice2(1-OLx,1-OLy,bi,bj),
     I                   iceTmp,tmpFac,1,'SI_AdvQ2',
     I                   0,1,2,bi,bj,myThid)
        ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   Initialisation (+ build oceanic mask)
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         maskOce(i,j) = 0. _d 0
         IF ( hOceMxL(i,j,bi,bj).GT.0. ) maskOce(i,j) = 1.
         iceVol(i,j) = 0. _d 0
         uTrans(i,j) = 0. _d 0
         vTrans(i,j) = 0. _d 0
         uTrIce(i,j) = 0. _d 0
         vTrIce(i,j) = 0. _d 0
         oceFWfx(i,j,bi,bj) = 0. _d 0
         oceSflx(i,j,bi,bj) = 0. _d 0
         oceQnet(i,j,bi,bj) = 0. _d 0
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE snowHeight(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
#endif
      IF ( thSIce_diffK .GT. 0. ) THEN
        CALL THSICE_DIFFUSION(
     I              maskOce,
     U              uIce, vIce,
     I              bi, bj, myTime, myIter, myThid )
      ENDIF

      IF ( thSIce_multiDimAdv ) THEN

C-    Calculate ice transports through tracer cell faces.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          uTrIce(i,j) = uIce(i,j)*_dyG(i,j,bi,bj)
     &                *maskOce(i-1,j)*maskOce(i,j)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx,sNx+OLx
          vTrIce(i,j) = vIce(i,j)*_dxG(i,j,bi,bj)
     &                *maskOce(i,j-1)*maskOce(i,j)
         ENDDO
        ENDDO

C--   Fractional area
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceFrc(i,j) = iceMask(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE utrice = comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE vtrice = comlev1_bibj, key=ticekey, byte=isbyte
#endif
        CALL THSICE_ADVECTION(
     I       GAD_SI_FRAC,  thSIceAdvScheme, .TRUE.,
     I       uTrIce, vTrIce, maskOce, thSIce_deltaT, areaEpsil,
     U       iceVol, iceFrc,
     O       uTrans, vTrans,
     I       bi, bj, myTime, myIter, myThid )

C--   Snow thickness
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceVol(i,j) = iceMask(i,j,bi,bj)*rA(i,j,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uTrans                = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE vTrans                = comlev1_bibj,key=ticekey,byte=isbyte
#endif
        CALL THSICE_ADVECTION(
     I       GAD_SI_HSNOW, thSIceAdvScheme, .FALSE.,
     I       uTrans, vTrans, maskOce, thSIce_deltaT, areaEpsil,
     U       iceVol, snowHeight(1-OLx,1-OLy,bi,bj),
     O       afx, afy,
     I       bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE iceMask   (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
#endif
C--   sea-ice Thickness
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceVol(i,j) = iceMask(i,j,bi,bj)*rA(i,j,bi,bj)
          oldVol(i,j) = iceVol(i,j)*iceHeight(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL THSICE_ADVECTION(
     I       GAD_SI_HICE,  thSIceAdvScheme, .FALSE.,
     I       uTrans, vTrans, maskOce, thSIce_deltaT, areaEpsil,
     U       iceVol, iceHeight(1-OLx,1-OLy,bi,bj),
     O       uTrIce, vTrIce,
     I       bi, bj, myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE utrice      = comlev1_bibj, key=ticekey, byte=isbyte
CADJ STORE vtrice      = comlev1_bibj, key=ticekey, byte=isbyte
#endif

#ifdef ALLOW_DBUG_THSICE
        IF ( dBugFlag ) THEN
         sumVar1 = 0.
         sumVar2 = 0.
         DO j=1,sNy
          DO i=1,sNx
C-      Check that updated iceVol = iceFrc*rA
           tmpVar = ABS(iceVol(i,j)-iceFrc(i,j)*rA(i,j,bi,bj))
           IF ( tmpVar.GT.0. ) THEN
             sumVar1 = sumVar1 + 1.
             sumVar2 = sumVar2 + tmpVar
           ENDIF
           IF ( tmpVar.GT.vol_Epsil ) THEN
            WRITE(6,'(A,2I4,2I2,I12)') 'ARE_ADV: ij,bij,it=',
     &                                  i,j,bi,bj,myIter
            WRITE(6,'(2(A,1P2E14.6))') 'ARE_ADV: iceVol,iceFrc*rA=',
     &                        iceVol(i,j),iceFrc(i,j)*rA(i,j,bi,bj),
     &            ' , diff=', tmpVar
           ENDIF
           IF ( dBug(i,j,bi,bj) ) THEN
            WRITE(6,'(A,2I4,2I2,I12)') 'ICE_ADV: ij,bij,it=',
     &                                  i,j,bi,bj,myIter
            WRITE(6,'(2(A,1P2E14.6))')
     &       'ICE_ADV: uIce=', uIce(i,j), uIce(i+1,j),
     &             ' , vIce=', vIce(i,j), vIce(i,j+1)
            WRITE(6,'(2(A,1P2E14.6))')
     &       'ICE_ADV: area_b,a=', iceMask(i,j,bi,bj), iceFrc(i,j),
     &       ' , Heff_b,a=', oldVol(i,j)*recip_rA(i,j,bi,bj),
     &                       iceHeight(i,j,bi,bj)*iceFrc(i,j)
           ENDIF
          ENDDO
         ENDDO
         IF ( sumVar2.GT.vol_Epsil )
     &   WRITE(6,'(A,2I2,I10,A,I4,1P2E14.6)') 'ARE_ADV: bij,it:',
     &                    bi,bj,myIter, ' ; Npts,aveDiff,Epsil=',
     &                    INT(sumVar1),sumVar2/sumVar1,vol_Epsil
        ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
C--     Diagnosse advective fluxes (ice-fraction, snow & ice thickness):
        IF ( useDiagnostics ) THEN
          diagSufx = THSICE_DIAG_SUFX( GAD_SI_FRAC, myThid )
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL( uTrans, diagName, 1,1,2,bi,bj, myThid )
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL( vTrans, diagName, 1,1,2,bi,bj, myThid )

          diagSufx = THSICE_DIAG_SUFX( GAD_SI_HSNOW, myThid )
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL( afx, diagName, 1,1,2,bi,bj, myThid )
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL( afy, diagName, 1,1,2,bi,bj, myThid )

          diagSufx = THSICE_DIAG_SUFX( GAD_SI_HICE, myThid )
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL( uTrIce, diagName, 1,1,2,bi,bj, myThid )
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL( vTrIce, diagName, 1,1,2,bi,bj, myThid )
        ENDIF
#endif

C--   Enthalpy in layer 1
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceVol(i,j) = oldVol(i,j)
         ENDDO
        ENDDO
        CALL THSICE_ADVECTION(
     I       GAD_SI_QICE1, thSIceAdvScheme, .FALSE.,
     I       uTrIce, vTrIce, maskOce, thSIce_deltaT, vol_Epsil,
     U       iceVol, Qice1(1-OLx,1-OLy,bi,bj),
     O       afx, afy,
     I       bi, bj, myTime, myIter, myThid )
#ifdef ALLOW_DBUG_THSICE
        IF ( dBugFlag ) THEN
         DO j=1,sNy
          DO i=1,sNx
           IF ( dBug(i,j,bi,bj) ) THEN
c           WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: Qice1_b,a=',
c    &             Qice1(i,j,bi,bj),
c    &        ( iceFld(i,j) + thSIce_deltaT * gFld(i,j)
c    &          ) * recip_heff(i,j)
c           WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: q1Fx=', gFld(i,j)
           ENDIF
          ENDDO
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagSufx = THSICE_DIAG_SUFX( GAD_SI_QICE1, myThid )
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL( afx, diagName, 1,1,2,bi,bj, myThid )
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL( afy, diagName, 1,1,2,bi,bj, myThid )
        ENDIF
#endif

C--   Enthalpy in layer 2
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          iceVol(i,j) = oldVol(i,j)
         ENDDO
        ENDDO
        CALL THSICE_ADVECTION(
     I       GAD_SI_QICE2, thSIceAdvScheme, .FALSE.,
     I       uTrIce, vTrIce, maskOce, thSIce_deltaT, vol_Epsil,
     U       iceVol, Qice2(1-OLx,1-OLy,bi,bj),
     O       afx, afy,
     I       bi, bj, myTime, myIter, myThid )
#ifdef ALLOW_DBUG_THSICE
        IF ( dBugFlag ) THEN
         sumVar1 = 0.
         sumVar2 = 0.
         DO j=1,sNy
          DO i=1,sNx
C-      Check that updated iceVol = Hic*Frc*rA
           tmpVar = ABS(iceVol(i,j)
     &             -iceHeight(i,j,bi,bj)*iceFrc(i,j)*rA(i,j,bi,bj))
           IF ( tmpVar.GT.0. ) THEN
             sumVar1 = sumVar1 + 1.
             sumVar2 = sumVar2 + tmpVar
           ENDIF
           IF ( tmpVar.GT.vol_Epsil ) THEN
            WRITE(6,'(A,2I4,2I2,I12)') 'VOL_ADV: ij,bij,it=',
     &                                  i,j,bi,bj,myIter
            WRITE(6,'(2(A,1P2E14.6))') 'VOL_ADV: iceVol,Hic*Frc*rA=',
     &      iceVol(i,j),iceHeight(i,j,bi,bj)*iceFrc(i,j)*rA(i,j,bi,bj),
     &             ' , diff=', tmpVar
           ENDIF
           IF ( dBug(i,j,bi,bj) ) THEN
c           WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: Qice2_b,a=',
c    &             Qice2(i,j,bi,bj),
c    &        ( iceFld(i,j) + thSIce_deltaT * gFld(i,j)
c    &          ) * recip_heff(i,j)
c           WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: q2Fx=', gFld(i,j)
           ENDIF
          ENDDO
         ENDDO
         IF ( sumVar2.GT.vol_Epsil )
     &   WRITE(6,'(A,2I2,I10,A,I4,1P2E14.6)') 'VOL_ADV: bij,it:',
     &                    bi,bj,myIter, ' ; Npts,aveDiff,Epsil=',
     &                    INT(sumVar1),sumVar2/sumVar1,vol_Epsil
        ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          diagSufx = THSICE_DIAG_SUFX( GAD_SI_QICE2, myThid )
          diagName = 'ADVx'//diagSufx
          CALL DIAGNOSTICS_FILL( afx, diagName, 1,1,2,bi,bj, myThid )
          diagName = 'ADVy'//diagSufx
          CALL DIAGNOSTICS_FILL( afy, diagName, 1,1,2,bi,bj, myThid )
        ENDIF
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE snowHeight(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE iceFrc                = comlev1_bibj,key=ticekey,byte=isbyte
#endif

C--   Update Ice Fraction: ensure that fraction is > iceMaskMin & < 1
C      and adjust Ice thickness and snow thickness accordingly
        DO j=1,sNy
         DO i=1,sNx
          IF ( iceFrc(i,j) .GT. 1. _d 0 ) THEN
c           IF ( dBug(i,j,bi,bj) )
            iceMask(i,j,bi,bj)    = 1. _d 0
            iceHeight(i,j,bi,bj)  = iceHeight(i,j,bi,bj) *iceFrc(i,j)
            snowHeight(i,j,bi,bj) = snowHeight(i,j,bi,bj)*iceFrc(i,j)
          ELSEIF ( iceFrc(i,j) .LT. iceMaskMin ) THEN
c           IF ( dBug(i,j,bi,bj) )
            iceMask(i,j,bi,bj)    = iceMaskMin
            iceHeight(i,j,bi,bj)  = iceHeight(i,j,bi,bj)
     &                             *iceFrc(i,j)*r_minArea
            snowHeight(i,j,bi,bj) = snowHeight(i,j,bi,bj)
     &                             *iceFrc(i,j)*r_minArea
          ELSE
            iceMask(i,j,bi,bj)    = iceFrc(i,j)
          ENDIF
         ENDDO
        ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     Store directives to avoid recomputing of this routine in ad-code
CADJ STORE iceHeight (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE snowHeight(:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE iceMask   (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE qIce1     (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
CADJ STORE qIce2     (:,:,bi,bj) = comlev1_bibj,key=ticekey,byte=isbyte
#endif
C-     adjust sea-ice state if ice is too thin.
        DO j=1,sNy
         DO i=1,sNx
          IF ( iceHeight(i,j,bi,bj).LT.hIceMin ) THEN
           iceVol(i,j) = iceMask(i,j,bi,bj)*iceHeight(i,j,bi,bj)
c          IF ( dBug(i,j,bi,bj) )
           IF ( iceVol(i,j).GE.hIceMin*iceMaskMin ) THEN
            iceMask(i,j,bi,bj)    = iceVol(i,j)/hIceMin
            snowHeight(i,j,bi,bj) = snowHeight(i,j,bi,bj)
     &                             *hIceMin/iceHeight(i,j,bi,bj)
            iceHeight(i,j,bi,bj)  = hIceMin
           ELSE
C-    Not enough ice, melt the tiny amount of snow & ice:
C     and return fresh-water, salt & energy to the ocean (flx > 0 = into ocean)
C- -  Note: using 1rst.Order Upwind, I can get the same results as when
C     using seaice_advdiff (with SEAICEadvScheme=1) providing I comment
C     out the following lines (and then loose conservation).
C- -
            oceFWfx(i,j,bi,bj) =  ( rhos*snowHeight(i,j,bi,bj)
     &                             +rhoi*iceHeight(i,j,bi,bj)
     &                            )*iceMask(i,j,bi,bj)/thSIce_deltaT
            oceSflx(i,j,bi,bj) =    rhoi*iceHeight(i,j,bi,bj)*saltIce
     &                             *iceMask(i,j,bi,bj)/thSIce_deltaT
            oceQnet(i,j,bi,bj) = -( rhos*snowHeight(i,j,bi,bj)*qsnow
     &                             +rhoi*iceHeight(i,j,bi,bj)
     &                                  *( Qice1(i,j,bi,bj)
     &                                    +Qice2(i,j,bi,bj) )*0.5 _d 0
     &                            )*iceMask(i,j,bi,bj)/thSIce_deltaT
C- -
c           flx2oc (i,j) = flx2oc (i,j) +
c           frw2oc (i,j) = frw2oc (i,j) +
c           fsalt  (i,j) = fsalt  (i,j) +
            iceMask   (i,j,bi,bj) = 0. _d 0
            iceHeight (i,j,bi,bj) = 0. _d 0
            snowHeight(i,j,bi,bj) = 0. _d 0
            Qice1     (i,j,bi,bj) = 0. _d 0
            Qice2     (i,j,bi,bj) = 0. _d 0
            snowAge   (i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_DBUG_THSICE
        IF ( dBugFlag ) THEN
         DO j=1,sNy
          DO i=1,sNx
           IF ( dBug(i,j,bi,bj) ) THEN
            WRITE(6,'(2(A,1P2E14.6))')
c    &       'ICE_ADV: area_b,a=', AREA(i,j,2,bi,bj),AREA(i,j,1,bi,bj)
c           WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: mFx=', gFld(i,j)
           ENDIF
          ENDDO
         ENDDO
        ENDIF
#endif

      ELSE
C---  if not multiDimAdvection

        WRITE(msgBuf,'(2A)') 'S/R THSICE_ADVDIFF: ',
     &       'traditional advection/diffusion not yet implemented'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') '                    ',
     &       'for ThSice variable Qice1, Qice2, SnowHeight. Sorry!'
        CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL: END: S/R THSICE_ADVDIFF'

C---  end if multiDimAdvection
      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
