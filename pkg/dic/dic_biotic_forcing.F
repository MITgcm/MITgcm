#include "DIC_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C !ROUTINE: DIC_BIOTIC_FORCING

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_BIOTIC_FORCING(
     U                      PTR_DIC, PTR_ALK, PTR_PO4, PTR_DOP,
#ifdef ALLOW_O2
     U                      PTR_O2,
#endif
#ifdef ALLOW_FE
     U                      PTR_FE,
#endif
     I                      bi, bj, iMin, iMax, jMin, jMax,
     I                      myTime, myIter, myThid )

C !DESCRIPTION:
C updates all the tracers for the effects of air-sea exchange, biological
c activity and remineralization

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "DIC_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT/OUTPUT PARAMETERS: ===================================================
C  PTR_DIC              :: dissolced inorganic carbon
C  PTR_ALK              :: alkalinity
C  PTR_PO4              :: phosphate
c  PTR_DOP              :: dissolve organic phosphurous
c  PTR_O2               :: oxygen
C  PTR_FE               :: iron
c  bi, bj               :: current tile indices
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_O2
      _RL  PTR_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ALLOW_FE
      _RL  PTR_FE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER bi, bj, iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef DIC_BIOTIC
C !FUNCTIONS:       ====================================================
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
C  G*                     :: tendency term for the tracers
C  SURA                   :: tendency of alkalinity due to freshwater
C  SURC                   :: tendency of DIC due to air-sea exchange
C                            and virtual flux
C  SURO                   :: tendency of O2 due to air-sea exchange
C  GPO4                   :: tendency of PO4 due to biological productivity,
C                            exchange with DOP pool and reminerization
C  CAR                    :: carbonate changes due to biological
C                             productivity and remineralization
C  BIOac                  :: biological productivity
C  RDOP                   :: DOP sink due to remineralization
C  pflux                  :: changes to PO4 due to flux and remineralization
C  CAR_S                  :: carbonate sink
C  cflux                  :: carbonate changes due to flux and remineralization
C  freefe                 :: iron not bound to ligand
      _RL  GDIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GPO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  SURA(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SURC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  SURO(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  CAR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  BIOac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  RDOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  pflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  exportflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  CAR_S(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ALLOW_O2
      _RL  GO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ALLOW_FE
      _RL  GFE(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  freefe(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
      INTEGER i,j,k
#ifdef ALLOW_FE
# ifdef SEDFE
      INTEGER kBottom
# endif
#endif
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key TAF-AD simulations (depends on tiles)
      INTEGER tkey
#endif
CEOP

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('DIC_BIOTIC_FORCING',myThid)
#endif

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif
      IF ( useThSIce .OR. useSEAICE .OR. useCoupler ) THEN
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('DIC_FIELDS_UPDATE',myThid)
#endif
        CALL DIC_FIELDS_UPDATE(
     I                    bi, bj, myTime, myIter, myThid )
      ENDIF

       DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           RDOP(i,j,k) =0. _d 0
           GDIC(i,j,k) =0. _d 0
           GALK(i,j,k) =0. _d 0
           GPO4(i,j,k) =0. _d 0
           GDOP(i,j,k) =0. _d 0
           CAR(i,j,k)  =0. _d 0
           BIOac(i,j,k)   =0. _d 0
           pflux(i,j,k)   =0. _d 0
           exportflux(i,j,k)=0. _d 0
           cflux(i,j,k)   =0. _d 0
           CAR_S(i,j,k)   =0. _d 0
#ifdef ALLOW_O2
           GO2(i,j,k)     =0. _d 0
#endif
#ifdef ALLOW_FE
           GFE(i,j,k)     =0. _d 0
C  no longer needed after adding full initialisation of freefe in S/R FE_CHEM
c          freefe(i,j,k)  =0. _d 0
#endif
          ENDDO
         ENDDO
       ENDDO
       DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           SURA(i,j)   =0. _d 0
           SURC(i,j)   =0. _d 0
           SURO(i,j)   =0. _d 0
         ENDDO
       ENDDO

C carbon air-sea interaction
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('DIC_SURFFORCING',myThid)
#endif
       CALL DIC_SURFFORCING(
     I                    PTR_DIC, PTR_ALK, PTR_PO4,
     O                    SURC,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE Kwexch_pre(:,:,bi,bj) = comlev1_bibj, key=tkey, kind=isbyte
#endif

C alkalinity air-sea interaction
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('ALK_SURFFORCING',myThid)
#endif
       CALL ALK_SURFFORCING(
     I                    PTR_ALK,
     O                    SURA,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )

#ifdef ALLOW_O2
C oxygen air-sea interaction
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('O2_SURFFORCING',myThid)
#endif
       CALL O2_SURFFORCING(
     I                    PTR_O2,
     O                    SURO,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )
#endif

#ifdef ALLOW_FE
C find free iron
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('FE_CHEM',myThid)
#endif
       CALL FE_CHEM( bi, bj, iMin, iMax, jMin, jMax,
     U               PTR_FE,
     O               freefe,
     I               myIter, myThid )
#endif

C biological activity
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('BIO_EXPORT',myThid)
#endif
       CALL BIO_EXPORT(
     I                    PTR_PO4,
#ifdef ALLOW_FE
     I                    PTR_FE,
#endif
     O                    BIOac,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE BIOac = comlev1_bibj, key = tkey, kind = isbyte
#endif

C flux of po4 from layers with biological activity
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('PHOS_FLUX',myThid)
#endif
       CALL PHOS_FLUX(
     I                    BIOac,
     U                    pflux, exportflux,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )

C- Carbonate sink
       DO k=1,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
             CAR_S(i,j,k)=BIOac(i,j,k)*R_CP*rain_ratio(i,j,bi,bj)*
     &                    (1. _d 0-DOPfraction)
          ENDDO
         ENDDO
       ENDDO

C carbonate
#ifdef DIC_CALCITE_SAT
       IF ( useCalciteSaturation ) THEN
C calcite dissolution occurs only below saturation horizon
C    code following method by Karsten Friis
C could be expensive, so find out if it is time to update the omega calcite
C    field (requires 3-d computation of pH).
        IF ( myIter.EQ.(nIter0 + 1) .OR.
     &       DIFFERENT_MULTIPLE( calcOmegaCalciteFreq,
     &                           myTime, deltaTClock ) ) THEN
#ifdef ALLOW_DEBUG
          IF (debugMode) CALL DEBUG_CALL('CALCITE_SATURATION',myThid)
#endif
          CALL CALCITE_SATURATION(
     I                    PTR_DIC, PTR_ALK, PTR_PO4,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )
        ENDIF
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CAR_FLUX_OMEGA_TOP',myThid)
#endif
        CALL CAR_FLUX_OMEGA_TOP(
     I                    BIOac,
     O                    cflux,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )
       ELSE
#endif /* DIC_CALCITE_SAT */
C calcite dissolution occurs according to a power law scaled by zca
C    code follwing the old OCMIP way
#ifdef ALLOW_DEBUG
        IF (debugMode) CALL DEBUG_CALL('CAR_FLUX',myThid)
#endif
         CALL CAR_FLUX(
     I                    CAR_S,
     U                    cflux,
     I                    bi, bj, iMin, iMax, jMin, jMax,
     I                    myTime, myIter, myThid )
#ifdef DIC_CALCITE_SAT
       ENDIF
#endif /* DIC_CALCITE_SAT */

C add all tendencies for PO4, DOP, ALK, DIC
       DO k=1,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
#ifdef DIC_NO_NEG
           RDOP(i,j,k)= MAX(maskC(i,j,k,bi,bj)*KDOPRemin*PTR_DOP(i,j,k)
     &                     ,0. _d 0)
#else
           RDOP(i,j,k)= maskC(i,j,k,bi,bj)*KDOPRemin*PTR_DOP(i,j,k)
#endif
           GPO4(i,j,k)=-BIOac(i,j,k)+pflux(i,j,k) + RDOP(i,j,k)

           car(i,j,k) = cflux(i,j,k) - CAR_S(i,j,k)

           GDOP(i,j,k)=+BIOac(i,j,k)*DOPfraction - RDOP(i,j,k)

           GALK(i,j,k)=+2. _d 0 *car(i,j,k)-R_NP*GPO4(i,j,k)

           GDIC(i,j,k)=car(i,j,k)+R_CP*GPO4(i,j,k)

#ifdef ALLOW_O2
           if (PTR_O2(i,j,k).GT.O2crit) then
             GO2(i,j,k)= R_OP*GPO4(i,j,k)
           else
             GO2(i,j,k)= 0. _d 0
           endif
#endif
#ifdef ALLOW_FE
           GFE(i,j,k) = R_FeP*GPO4(i,j,k)
     &                 -Kscav*freefe(i,j,k)
#endif
          ENDDO
         ENDDO
       ENDDO

       DO j=jMin,jMax
         DO i=iMin,iMax
               GALK(i,j,1)=GALK(i,j,1)+SURA(i,j)
               GDIC(i,j,1)=GDIC(i,j,1)+SURC(i,j)
#ifdef ALLOW_O2
               GO2(i,j,1) =GO2(i,j,1)+SURO(i,j)
#endif
#ifdef ALLOW_FE
               GFE(i,j,1)=GFE(i,j,1)+alpfe*
     &                    InputFe(i,j,bi,bj)*recip_drF(1)
     &                       *recip_hFacC(i,j,1,bi,bj)
# ifdef SEDFE
C include iron sediment source using the flux of po4 into bottom layer
               kBottom   = MAX(kLowC(i,j,bi,bj),1)
               GFE(i,j,kBottom)=GFE(i,j,kBottom)
     &              +( fesedflux_pcm*pflux(i,j,kBottom) + FeIntSec )
     &              *recip_drF(kBottom)*recip_hFacC(i,j,kBottom,bi,bj)
# endif
#endif
         ENDDO
       ENDDO

       IF ( useOBCS ) THEN
        DO k=1,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
            GDIC(i,j,k) = GDIC(i,j,k)*maskInC(i,j,bi,bj)
            GALK(i,j,k) = GALK(i,j,k)*maskInC(i,j,bi,bj)
            GPO4(i,j,k) = GPO4(i,j,k)*maskInC(i,j,bi,bj)
            GDOP(i,j,k) = GDOP(i,j,k)*maskInC(i,j,bi,bj)
#ifdef ALLOW_O2
            GO2(i,j,k)  =  GO2(i,j,k)*maskInC(i,j,bi,bj)
#endif
#ifdef ALLOW_FE
            GFE(i,j,k)  =  GFE(i,j,k)*maskInC(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDIF

C update
       DO k=1,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
           PTR_DIC(i,j,k)=
     &      PTR_DIC(i,j,k)+GDIC(i,j,k)*PTRACERS_dTLev(k)
           PTR_ALK(i,j,k)=
     &      PTR_ALK(i,j,k)+GALK(i,j,k)*PTRACERS_dTLev(k)
           PTR_PO4(i,j,k)=
     &      PTR_PO4(i,j,k)+GPO4(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOP(i,j,k)=
     &      PTR_DOP(i,j,k)+GDOP(i,j,k)*PTRACERS_dTLev(k)
#ifdef ALLOW_O2
           PTR_O2(i,j,k)=
     &      PTR_O2(i,j,k)+GO2(i,j,k)*PTRACERS_dTLev(k)
#endif
#ifdef ALLOW_FE
           PTR_FE(i,j,k)=
     &      PTR_FE(i,j,k)+GFE(i,j,k)*PTRACERS_dTLev(k)
#endif
          ENDDO
         ENDDO
       ENDDO

#ifdef ALLOW_FE
#ifdef MINFE
c find free iron and get rid of insoluble part
#ifdef ALLOW_DEBUG
       IF (debugMode) CALL DEBUG_CALL('FE_CHEM',myThid)
#endif
       CALL FE_CHEM( bi, bj, iMin, iMax, jMin, jMax,
     U               PTR_FE,
     O               freefe,
     I               myIter, myThid )
#endif
#endif

#ifdef ALLOW_TIMEAVE
C save averages
       IF ( PTRACERS_taveFreq.GT.0. ) THEN
        DO k=1,Nr
         DO j=jMin,jMax
          DO i=iMin,iMax
            BIOave(i,j,k,bi,bj)   =BIOave(i,j,k,bi,bj)+
     &                             BIOac(i,j,k)*deltaTClock
            CARave(i,j,k,bi,bj)   =CARave(i,j,k,bi,bj)+
     &                             CAR(i,j,k)*deltaTClock
            pfluxave(i,j,k,bi,bj) =pfluxave(i,j,k,bi,bj) +
     &                             pflux(i,j,k)*deltaTClock
            epfluxave(i,j,k,bi,bj)=epfluxave(i,j,k,bi,bj) +
     &                             exportflux(i,j,k)*deltaTClock
            cfluxave(i,j,k,bi,bj) =cfluxave(i,j,k,bi,bj) +
     &                             cflux(i,j,k)*deltaTClock
          ENDDO
         ENDDO
        ENDDO
         DO j=jMin,jMax
          DO i=iMin,iMax
              SURave(i,j,bi,bj)    =SURave(i,j,bi,bj)+
     &                              SURC(i,j)*deltaTClock
#ifdef ALLOW_O2
              SUROave(i,j,bi,bj)   =SUROave(i,j,bi,bj)+
     &                              SURO(i,j)*deltaTClock
#endif
              pCO2ave(i,j,bi,bj)   =pCO2ave(i,j,bi,bj)+
     &                              pCO2(i,j,bi,bj)*deltaTClock
              pHave(i,j,bi,bj)     =pHave(i,j,bi,bj)+
     &                              pH(i,j,bi,bj)*deltaTClock
              fluxCO2ave(i,j,bi,bj)=fluxCO2ave(i,j,bi,bj)+
     &                           fluxCO2(i,j,bi,bj)*deltaTClock
          ENDDO
         ENDDO
         DIC_timeAve(bi,bj) = DIC_timeAve(bi,bj)+deltaTClock
       ENDIF
#endif /* ALLOW_TIMEAVE*/

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(BIOac  ,'DICBIOA ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(CAR    ,'DICCARB ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(pCO2   ,'DICPCO2 ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxCO2,'DICCFLX ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(pH     ,'DICPHAV ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(SURC   ,'DICTFLX ',0,1 ,2,bi,bj,myThid)
#ifdef ALLOW_O2
        CALL DIAGNOSTICS_FILL(SURO   ,'DICOFLX ',0,1 ,2,bi,bj,myThid)
#endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('DIC_BIOTIC_FORCING',myThid)
#endif

#endif /* DIC_BIOTIC */

       RETURN
       END
