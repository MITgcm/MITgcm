C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_main.F,v 1.6 2016/10/19 21:41:02 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_MAIN( PTR_DIC, PTR_ALK, PTR_O2, PTR_NO3,
     &                      PTR_PO4, PTR_FE, PTR_DON, PTR_DOP,
#ifdef ADVECT_PHYTO
     &                      PTR_PHY,
#endif
     &                      bi, bj, imin, imax, jmin, jmax,
     &                      myIter, myTime, myThid)

C     ==================================================================
C     | subroutine bling_main
C     | o Updates all the tracers for the effects of air-sea exchange,
C     |   biological production, and remineralization.
C     | - The basic model includes 8 tracers
C     | - There is an optional tracer for phytoplankton biomass 
C     | - River runoff is included here
C     ==================================================================

      implicit none

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "tamc.h"
#endif
#include "BLING_VARS.h"

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
C     PTR_NO3       :: nitrate concentration
C     PTR_PO4       :: phosphate concentration
C     PTR_DON       :: dissolved organic nitrogen concentration
C     PTR_DOP       :: dissolved organic phosphorus concentration
C     PTR_O2        :: oxygen concentration
C     PTR_FE        :: iron concentration
C     PTR_PHY       :: total phytoplankton biomass
      _RL  PTR_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  PTR_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef ADVECT_PHYTO
      _RL  PTR_PHY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif

C     === Local variables ===
C     i,j,k                :: loop indices
C     G_xx                 :: tendency term for the tracers
C     surf_DIC             :: tendency of DIC due to air-sea exchange
C     surf_O2              :: tendency of O2 due to air-sea exchange
C     runoff_bgc           :: tendency due to river runoff

       INTEGER i,j,k
      _RL  G_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_ALK(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_NO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_PO4(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_FE (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_O2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DON(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_DOP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  G_CaCO3(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  NCP(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  bio_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  surf_DIC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  surf_O2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  FluxO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  irr_eff(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  runoff_bgc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,8)
CEOP

c-----------------------------------------------------------
c  Initialize local variables

      DO j=jmin,jmax
       DO i=imin,imax
        DO k=1,Nr
         G_DIC(i,j,k)        = 0. _d 0
         G_ALK(i,j,k)        = 0. _d 0
         G_NO3(i,j,k)        = 0. _d 0
         G_PO4(i,j,k)        = 0. _d 0
         G_FE(i,j,k)         = 0. _d 0
         G_O2(i,j,k)         = 0. _d 0
         G_DON(i,j,k)        = 0. _d 0
         G_DOP(i,j,k)        = 0. _d 0
         G_CaCO3(i,j,k)      = 0. _d 0
         NCP(i,j,k)          = 0. _d 0
         irr_eff(i,j,k)      = 0. _d 0
        ENDDO
        DO k=1,8
         runoff_bgc(i,j,k)   = 0. _d 0
        ENDDO
        bio_DIC(i,j,k)       = 0. _d 0
        surf_DIC(i,j)        = 0. _d 0
        surf_O2(i,j)         = 0. _d 0
        fluxO2(i,j)          = 0. _d 0
       ENDDO
      ENDDO
c-----------------------------------------------------------
c Get Atmospheric carbon dioxide concentration from EXF 
c if decide to use exf for multiple input fields should put this
c in subroutine
#ifdef USE_EXFCO2
c     Atmospheric carbon dioxide concentration
      call EXF_SET_GEN  (
     &     apco2file, apco2startdate, apco2period,
     &     exf_inscal_apco2,
     &     apco2_exfremo_intercept, apco2_exfremo_slope,
     &     apco2, apco20, apco21, apco2mask,
# ifdef USE_EXF_INTERPOLATION
     &     apco2_lon0, apco2_lon_inc, apco2_lat0, apco2_lat_inc,
     &     apco2_nlon, apco2_nlat, xC, yC, apco2_interpMethod,
# endif
     &     mytime, myiter, mythid )
#endif

c-----------------------------------------------------------
c  carbon and oxygen air-sea interaction
       CALL BLING_AIRSEAFLUX(
     I                       PTR_DIC, PTR_ALK, PTR_O2,
     I                       PTR_NO3, PTR_PO4,
     U                       surf_DIC, surf_O2, fluxO2,
     I                       bi, bj, imin, imax, jmin, jmax,
     I                       myIter, myTime, myThid)

CADJ STORE ph = comlev1, key = ikey_dynamics,
CADJ &    kind = isbyte

c-----------------------------------------------------------
c  determine calcite saturation for remineralization
       CALL BLING_CARBONATE_SYS(
     I                         PTR_DIC, PTR_ALK, PTR_PO4,
     I                         bi, bj, imin, imax, jmin, jmax,
     I                         myIter, myTime, myThid)

C-----------------------------------------------------------
C  biological activity
       CALL BLING_PROD(
     I                 PTR_NO3, PTR_PO4, PTR_FE,
     I                 PTR_O2, PTR_DON, PTR_DOP,
#ifdef ADVECT_PHYTO
     I                 PTR_PHY, 
#endif
     U                 G_NO3, G_PO4, G_FE,
     U                 G_O2, G_DON, G_DOP, 
     U                 G_CACO3, NCP,
     I                 bi, bj, imin, imax, jmin, jmax,
     I                 myIter, myTime, myThid)

C-----------------------------------------------------------
C  Calculate river runoff source
C  Tracers are already diluted by freswater input, P-E+R
C  This accounts for tracer concentration in river runoff

c      DO k=1,8
c       DO j=jmin,jmax
c        DO i=imin,imax
c#ifdef ALLOW_EXF
c         runoff_bgc(i,j,k) = river_conc_trac(k)*runoff(i,j,bi,bj)
c     &                       *recip_drF(1)*recip_hFacC(i,j,1,bi,bj)
c#else
c         runoff_bgc(i,j,k) = 0. _d 0
c#endif
c        ENDDO
c       ENDDO
c      ENDDO

c ---------------------------------------------------------------------

c  Carbon system

      DO j=jmin,jmax
       DO i=imin,imax
        DO k=1,Nr

         IF (hFacC(i,j,k,bi,bj) .gt. 0. _d 0) THEN

              G_ALK(i,j,k) = - G_NO3(i,j,k)
     &              + 2. _d 0*G_CaCO3(i,j,k)

              G_DIC(i,j,k) = -NCP(i,j,k) + G_CaCO3(i,j,k)

c  For diagnostics
              bio_DIC(i,j,k) = G_DIC(i,j,k)

         ENDIF

        ENDDO
       ENDDO
      ENDDO

C-----------------------------------------------------------
C   adding surface tendencies due to air-sea exchange
C   adding surface tendencies due to river runoff
C   adding aeolian iron source

         DO j=jmin,jmax
          DO i=imin,imax
               G_DIC(i,j,1) = G_DIC(i,j,1) + runoff_bgc(i,j,1)
     &                    + surf_DIC(i,j)
               G_ALK(i,j,1) = G_ALK(i,j,1) + runoff_bgc(i,j,2)
               G_NO3(i,j,1) = G_NO3(i,j,1) + runoff_bgc(i,j,3)
               G_PO4(i,j,1) = G_PO4(i,j,1) + runoff_bgc(i,j,4)
               G_FE(i,j,1)  = G_FE(i,j,1)  + runoff_bgc(i,j,5)
     &                    + alpfe*InputFe(i,j,bi,bj)*recip_drF(1)
     &                    * recip_hFacC(i,j,1,bi,bj)
               G_O2(i,j,1)  = G_O2(i,j,1)  + runoff_bgc(i,j,6)
     &                    + surf_O2(i,j)
               G_DON(i,j,1) = G_DON(i,j,1) + runoff_bgc(i,j,7)
               G_DOP(i,j,1) = G_DOP(i,j,1) + runoff_bgc(i,j,8)
          ENDDO
         ENDDO

C-----------------------------------------------------------
C update
       DO k=1,Nr
         DO j=jmin,jmax
          DO i=imin,imax
           PTR_DIC(i,j,k)=PTR_DIC(i,j,k)+G_DIC(i,j,k)*PTRACERS_dTLev(k)
           PTR_ALK(i,j,k)=PTR_ALK(i,j,k)+G_ALK(i,j,k)*PTRACERS_dTLev(k)
           PTR_NO3(i,j,k)=PTR_NO3(i,j,k)+G_NO3(i,j,k)*PTRACERS_dTLev(k)
           PTR_PO4(i,j,k)=PTR_PO4(i,j,k)+G_PO4(i,j,k)*PTRACERS_dTLev(k)
           PTR_FE (i,j,k)=PTR_FE (i,j,k)+G_FE (i,j,k)*PTRACERS_dTLev(k)
           PTR_O2 (i,j,k)=PTR_O2 (i,j,k)+G_O2 (i,j,k)*PTRACERS_dTLev(k)
           PTR_DON(i,j,k)=PTR_DON(i,j,k)+G_DON(i,j,k)*PTRACERS_dTLev(k)
           PTR_DOP(i,j,k)=PTR_DOP(i,j,k)+G_DOP(i,j,k)*PTRACERS_dTLev(k)
          ENDDO
         ENDDO
       ENDDO

C-----------------------------------------------------------
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(bio_DIC ,'BLGBIOA ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(pH      ,'BLGPH3D ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(OmegaAr ,'BLGOMAR ',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(pCO2    ,'BLGPCO2 ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxCO2 ,'BLGCFLX ',0,1 ,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(fluxO2  ,'BLGOFLX ',0,1 ,2,bi,bj,myThid)
#ifdef USE_EXFCO2
         CALL DIAGNOSTICS_FILL(apco2  ,'BLGapco2',0,1,0,1,1,myThid)
#endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

       RETURN
       END
