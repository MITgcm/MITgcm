#include "BLING_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
      SUBROUTINE BLING_MAIN( PTR_DIC, PTR_ALK, PTR_O2,
#ifndef USE_BLING_V1
     U           PTR_NO3, PTR_PO4, PTR_FE,
     U           PTR_DON, PTR_DOP,
#ifdef USE_SIBLING
     U           PTR_SI,
#endif
#ifdef ADVECT_PHYTO
     U           PTR_PHY,
#endif
#else
     U           PTR_PO4, PTR_DOP, PTR_FE,
#endif
     I           bi, bj, imin, imax, jmin, jmax,
     I           myTime, myIter, myThid)

C     ==================================================================
C     | subroutine bling_main
C     | o Updates all the tracers for the effects of air-sea exchange,
C     |   biological production, and remineralization.
C     | - The basic model includes 8 tracers
C     | - There is an optional tracer for phytoplankton biomass
C     | - River runoff is included here
C     ==================================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_INTERP_SIZE.h"
# include "EXF_FIELDS.h"
#endif
#include "BLING_VARS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     bi,bj         :: tile indices
C     iMin,iMax     :: computation domain: 1rst index range
C     jMin,jMax     :: computation domain: 2nd  index range
C     myTime        :: current time
C     myIter        :: current timestep
C     myThid        :: thread Id. number
      INTEGER bi, bj, imin, imax, jmin, jmax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
C     === Input ===
C     PTR_DIC       :: dissolved inorganic carbon
C     PTR_ALK       :: alkalinity
C     PTR_O2        :: oxygen concentration
C     PTR_FE        :: iron concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_NO3       :: nitrate concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_PHY       :: total phytoplankton biomass
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifndef USE_BLING_V1
      _RL  PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef USE_SIBLING
      _RL  PTR_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef ADVECT_PHYTO
      _RL  PTR_PHY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#endif

C     === Local variables ===
C     i,j,k                :: loop indices
C     G_xxx                :: tendency term for tracer xxx
C     surf_DIC             :: tendency of DIC due to air-sea exchange
C     surf_Fe              :: tendency of Fe due to dust deposition
C     surf_O2              :: tendency of O2 due to air-sea exchange
C     FluxO2               :: air-sea O2 flux
C     runoff_PO4           :: tendency due to river runoff

       INTEGER i,j,k
      _RL  G_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  surf_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  surf_Fe(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  surf_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  FluxO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  runoff_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifndef USE_BLING_V1
      _RL  G_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef USE_SIBLING
      _RL  G_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#endif
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (tile dependent)
      INTEGER tkey
#endif
CEOP

c-----------------------------------------------------------
c  Initialize local variables

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj - 1)*nSx + (ikey_dynamics - 1)*nSx*nSy
#endif
      DO j=jmin,jmax
       DO i=imin,imax
        surf_DIC(i,j)        = 0. _d 0
        surf_O2(i,j)         = 0. _d 0
        surf_Fe(i,j)         = 0. _d 0
        fluxO2(i,j)          = 0. _d 0
        runoff_PO4(i,j)      = 0. _d 0
       ENDDO
      ENDDO
      DO j=jmin,jmax
       DO i=imin,imax
        DO k=1,Nr
         G_DIC(i,j,k)        = 0. _d 0
         G_ALK(i,j,k)        = 0. _d 0
         G_O2(i,j,k)         = 0. _d 0
         G_FE(i,j,k)         = 0. _d 0
         G_PO4(i,j,k)        = 0. _d 0
         G_DOP(i,j,k)        = 0. _d 0
#ifndef USE_BLING_V1
         G_NO3(i,j,k)        = 0. _d 0
         G_DON(i,j,k)        = 0. _d 0
#ifdef USE_SIBLING
         G_SI(i,j,k)         = 0. _d 0
#endif
#endif
        ENDDO
       ENDDO
      ENDDO

c-----------------------------------------------------------
c  carbon and oxygen air-sea interaction
       CALL BLING_AIRSEAFLUX(
     I                       PTR_DIC, PTR_ALK, PTR_O2, PTR_PO4,
#ifdef USE_SIBLING
     I                       PTR_SI,
#endif
     U                       surf_DIC, surf_O2, fluxO2,
     I                       bi, bj, imin, imax, jmin, jmax,
     I                       myTime, myIter, myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ph(:,:,:,bi,bj), ak0(:,:,bi,bj), fugf(:,:,bi,bj)
CADJ &     = comlev1_bibj, key = tkey, kind = isbyte
#endif

c-----------------------------------------------------------
c  determine calcite saturation for remineralization
       CALL BLING_CARBONATE_SYS(
     I                         PTR_DIC, PTR_ALK, PTR_PO4,
#ifdef USE_SIBLING
     I                         PTR_SI,
#endif
     I                         bi, bj, imin, imax, jmin, jmax,
     I                         myTime, myIter, myThid)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE omegaC(:,:,:,bi,bj) = comlev1_bibj, key=tkey, kind=isbyte
#endif

C-----------------------------------------------------------
C  biological activity
C  call either "BLING" or "BLING + nitrogen"
#ifndef USE_BLING_V1
       CALL BLING_BIO_NITROGEN(
     I                 PTR_O2, PTR_FE, PTR_PO4, PTR_DOP,
     I                 PTR_NO3, PTR_DON,
#ifdef USE_SIBLING
     I                 PTR_SI,
#endif
#ifdef ADVECT_PHYTO
     I                 PTR_PHY,
#endif
     U                 G_DIC, G_ALK, G_O2, G_FE,
     U                 G_PO4, G_DOP, G_NO3, G_DON,
#ifdef USE_SIBLING
     U                 G_SI,
#endif
     I                 bi, bj, imin, imax, jmin, jmax,
     I                 myTime, myIter, myThid)
#else
       CALL BLING_BIO(
     I                 PTR_O2, PTR_FE, PTR_PO4, PTR_DOP,
     U                 G_DIC, G_ALK, G_O2, G_FE,
     U                 G_PO4, G_DOP,
     I                 bi, bj, imin, imax, jmin, jmax,
     I                 myTime, myIter, myThid)
#endif

C-----------------------------------------------------------
C  Calculate river runoff source
C  Tracers are already diluted by freswater input, P-E+R
C  This accounts for tracer concentration in river runoff
C  no input south of 60S (it is ice melt)

#if (defined ALLOW_EXF && defined ALLOW_RUNOFF)
       DO j=jmin,jmax
        IF (YC(i,j,bi,bj).gt.-60.0) THEN
         DO i=imin,imax
          runoff_PO4(i,j) = river_conc_po4*runoff(i,j,bi,bj)
     &                       *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
         ENDDO
        ENDIF
       ENDDO
#endif

C-----------------------------------------------------------
C   adding surface tendencies due to air-sea exchange
C   adding surface tendencies due to river runoff
C   (assuming no dilution / runoff input for O2, Fe)
C   adding aeolian iron source

         DO j=jmin,jmax
          DO i=imin,imax

               G_DIC(i,j,1) = G_DIC(i,j,1) + surf_DIC(i,j)
     &                    + runoff_PO4(i,j)*CtoN*NtoP
               G_ALK(i,j,1) = G_ALK(i,j,1)
     &                    - runoff_PO4(i,j)*NtoP
               G_O2(i,j,1)  = G_O2(i,j,1) + surf_O2(i,j)
               surf_Fe(i,j) = alpfe*InputFe(i,j,bi,bj)
               G_FE(i,j,1)  = G_FE(i,j,1) + surf_Fe(i,j)
     &                    * recip_drF(1) * recip_hFacC(i,j,1,bi,bj)
               G_PO4(i,j,1) = G_PO4(i,j,1) + runoff_PO4(i,j)
               G_DOP(i,j,1) = G_DOP(i,j,1)
     &                    + runoff_PO4(i,j)*river_dom_to_nut
#ifndef USE_BLING_V1
               G_NO3(i,j,1) = G_NO3(i,j,1) + runoff_PO4(i,j)*NtoP
               G_DON(i,j,1) = G_DON(i,j,1)
     &                    + runoff_PO4(i,j)*NtoP*river_dom_to_nut

#endif
          ENDDO
         ENDDO

C-----------------------------------------------------------
C update
       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
           PTR_DIC(i,j,k)=PTR_DIC(i,j,k)+G_DIC(i,j,k)*PTRACERS_dTLev(k)
           PTR_ALK(i,j,k)=PTR_ALK(i,j,k)+G_ALK(i,j,k)*PTRACERS_dTLev(k)
           PTR_O2 (i,j,k)=PTR_O2 (i,j,k)+G_O2 (i,j,k)*PTRACERS_dTLev(k)
           PTR_FE (i,j,k)=PTR_FE (i,j,k)+G_FE (i,j,k)*PTRACERS_dTLev(k)
           PTR_PO4(i,j,k)=PTR_PO4(i,j,k)+G_PO4(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOP(i,j,k)=PTR_DOP(i,j,k)+G_DOP(i,j,k)*PTRACERS_dTLev(k)
#ifndef USE_BLING_V1
           PTR_NO3(i,j,k)=PTR_NO3(i,j,k)+G_NO3(i,j,k)*PTRACERS_dTLev(k)
           PTR_DON(i,j,k)=PTR_DON(i,j,k)+G_DON(i,j,k)*PTRACERS_dTLev(k)
#ifdef USE_SIBLING
           PTR_SI (i,j,k)=PTR_SI (i,j,k)+G_SI (i,j,k)*PTRACERS_dTLev(k)
#endif
#endif
          ENDDO
         ENDDO
       ENDDO

C-----------------------------------------------------------
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(pH      ,'BLGPH3D ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(OmegaAr ,'BLGOMAR ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(pCO2    ,'BLGPCO2 ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxCO2 ,'BLGCFLX ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxO2  ,'BLGOFLX ',0,1 ,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(surf_Fe ,'BLGSURFE',0,1, 2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(apco2,'BLGapco2',0,1, 1,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

       RETURN
       END
