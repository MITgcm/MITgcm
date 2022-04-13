#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: ECCO_PHYS

C     !INTERFACE:
      SUBROUTINE ECCO_PHYS( myTime, myIter, myThid )

C     !DESCRIPTION:
C     Compute some derived quantities and averages
C     for GenCost and ECCO cost function.

C     !USES:
      IMPLICIT NONE
c     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif
#if (defined ALLOW_GENCOST_CONTRIBUTION) && (defined ALLOW_SHELFICE)
# include "SHELFICE.h"
#endif

C     !INPUT PARAMETERS:
C     myTime    :: Current time in simulation
C     myIter    :: Current time-step number
C     myThid    :: my Thread Id number
      _RL     myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     bi, bj    :: tile indices
C     i, j, k   :: loop indices
      INTEGER bi, bj
      INTEGER i, j, k
#ifdef ALLOW_GENCOST_CONTRIBUTION
      INTEGER kgen, kgen3d, itr
      _RL areavolTile(nSx,nSy), areavolGlob
      _RL tmpfld, tmpvol, tmpmsk, tmpmsk2, tmpmskW, tmpmskS
      _RL tmp_sigmsk, tmpsig, tmpsig_lower, tmpsig_upper
#endif

C- note: defined with overlap here, not needed, but more efficient
      _RL trVolW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trVolS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trHeatW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trHeatS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trSaltW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trSaltS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

      _RL tmpfac
      _RL sIceLoadFacLoc
#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
      _RL ploadbar, AREAsumGlob, PLOADsumGlob
      _RL AREAsumTile(nSx,nSy), PLOADsumTile(nSx,nSy)
      _RL m_eta_ib(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL sterht  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#endif
      _RL rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_PSBAR_STERIC
      _RL VOLsumTile(nSx,nSy), RHOsumTile(nSx,nSy)
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
C     Mload     :: total mass load (kg/m**2)
c     _RL Mload(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
CEOP

      tmpfac = recip_rhoConst*recip_gravity
      sIceLoadFacLoc = zeroRL
      IF ( useRealFreshWaterFlux ) sIceLoadFacLoc = recip_rhoConst

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        IF ( myIter .EQ. -1 ) THEN
          DO k = 1,Nr
            CALL FIND_RHO_2D(
     I                1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I                theta(1-OLx,1-OLy,k,bi,bj),
     I                salt (1-OLx,1-OLy,k,bi,bj),
     O                rhoLoc(1-OLx,1-OLy,k,bi,bj),
     I                k, bi, bj, myThid )
          ENDDO
        ELSE
          DO k = 1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              rhoLoc(i,j,k,bi,bj) = rhoInSitu(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
        ENDIF
       ENDDO
      ENDDO

#ifdef ALLOW_PSBAR_STERIC
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        RHOsumTile(bi,bj) = 0. _d 0
        VOLsumTile(bi,bj) = 0. _d 0
        DO k = 1,Nr
         DO j = 1,sNy
          DO i =  1,sNx
           RHOsumTile(bi,bj) = RHOsumTile(bi,bj)
     &          + ( rhoConst + rhoLoc(i,j,k,bi,bj) )
     &           *hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
           VOLsumTile(bi,bj) = VOLsumTile(bi,bj)
     &          + hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL GLOBAL_SUM_TILE_RL( VOLsumTile, VOLsumGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( RHOsumTile, RHOsumGlob, myThid )
      RHOsumGlob = RHOsumGlob/VOLsumGlob

      IF (RHOsumGlob_0.GT.0. _d 0) THEN
        sterGloH = VOLsumGlob_0/globalArea
     &        *(1. _d 0 - RHOsumGlob/RHOsumGlob_0)
      ELSE
        sterGloH = 0. _d 0
      ENDIF

c     WRITE(msgBuf,'(A,1PE21.14)') ' sterGloH= ', sterGloH
c     CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                       SQUEEZE_RIGHT, myThid )

#endif /* ALLOW_PSBAR_STERIC */

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        PLOADsumTile(bi,bj) = 0. _d 0
        AREAsumTile(bi,bj) = 0. _d 0
        DO j = 1,sNy
         DO i =  1,sNx
          PLOADsumTile(bi,bj) = PLOADsumTile(bi,bj)
     &          + pLoad(i,j,bi,bj)
     &           *maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
          AREAsumTile(bi,bj) = AREAsumTile(bi,bj)
     &          + maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL GLOBAL_SUM_TILE_RL( AREAsumTile, AREAsumGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( PLOADsumTile, PLOADsumGlob, myThid )
      ploadbar = PLOADsumGlob/AREAsumGlob
#endif /* ALLOW_IB_CORR */
#endif /* ATMOSPHERIC_LOADING */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx

C calculate total sea level including inverse barometer (IB) effect if
C  there is air pressure forcing
           m_eta(i,j,bi,bj) =
     &           ( etaN(i,j,bi,bj)
     &           + sIceLoad(i,j,bi,bj)*sIceLoadFacLoc
#ifdef ALLOW_PSBAR_STERIC
C apply Greatbatch correction
     &           + sterGloH
#endif /* ALLOW_PSBAR_STERIC */
     &           ) * maskC(i,j,1,bi,bj)

C Model equivalent of ocean bottom pressure gauge data (in m^2/s^2)
C  that are NOT corrected for global ocean mean atmospheric pressure variations
C  = Ocean mass + sea-ice & snow load + air-pressure load + Greatbatch corr.
C  (all terms on RHS are converted to m^2/s^2). It is
C  essentially Mload (as in pkg/sbo/sbo_calc.F) plus air-pressure load
           m_bp(i,j,bi,bj) =
     &           ( etaN(i,j,bi,bj)
c    &           + Ro_surf(i,j,bi.bj)
     &           - R_low(i,j,bi,bj)
#ifdef ALLOW_PSBAR_STERIC
C add back the correction due to the global mean steric ssh change,
C     i.e. sterGloH computed above (units converted from m to m2/s2)
     &           + sterGloH
#endif /* ALLOW_PSBAR_STERIC */
     &           ) * gravity
C sIceLoad in kg/m^2
     &         + sIceLoad(i,j,bi,bj) * gravity * sIceLoadFacLoc
C pLoad in N/m^2
     &         + pLoad(i,j,bi,bj) * recip_rhoConst
         ENDDO
        ENDDO
C integrate rho_anomaly through water column
        DO k = 1, Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           m_bp(i,j,bi,bj) = m_bp(i,j,bi,bj)
     &         + rhoLoc(i,j,k,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
     &                              * gravity * recip_rhoConst
          ENDDO
         ENDDO
        ENDDO
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
           m_bp(i,j,bi,bj) = m_bp(i,j,bi,bj) * maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
C calculate IB correction m_eta_ib (in m)
           m_eta_ib(i,j,bi,bj) =
     &           ( ploadbar - pLoad(i,j,bi,bj) )*tmpfac
     &           * maskC(i,j,1,bi,bj)
C calculte dynamic sea level for comparison with altimetry data (in m)
           m_eta_dyn(i,j,bi,bj) =
     &           ( m_eta(i,j,bi,bj) - m_eta_ib(i,j,bi,bj) )
     &           * maskC(i,j,1,bi,bj)

C calculate GRACE-equvivalent ocean bottom pressure (in m2/s2)
C  by removing global ocean mean atmospheric pressure variations
           m_bp_nopabar(i,j,bi,bj) =
     &           ( m_bp(i,j,bi,bj)
     &           - ploadbar * recip_rhoConst
     &           ) * maskC(i,j,1,bi,bj)
C calculate steric height
C (in m; = m_eta_dyn - (m_bp_nopabr * recip_gravity + R_low))
C R_low (<0) is Depth in m.
           sterht(i,j,bi,bj) = m_eta_dyn(i,j,bi,bj)
     &          - ( m_bp_nopabar(i,j,bi,bj) * recip_gravity
c    &            - Ro_surf(i,j,bi,bj)
     &            + R_low(i,j,bi,bj)  )
         ENDDO
        ENDDO
#endif /* ALLOW_IB_CORR */
#endif /* ATMOSPHERIC_LOADING */
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. myIter.GE.0 ) THEN
        CALL DIAGNOSTICS_FILL( m_eta, 'SSHNOIBC', 0,1, 0,1,1, myThid )
        CALL DIAGNOSTICS_SCALE_FILL( m_bp, recip_gravity, 1,
     &                         'OBPGMAP ', 0,1, 0,1,1, myThid )
#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
        CALL DIAGNOSTICS_FILL( m_eta_ib,
     &                         'SSHIBC  ', 0,1, 0,1,1, myThid )
        CALL DIAGNOSTICS_FILL( m_eta_dyn,
     &                         'SSH     ', 0,1, 0,1,1, myThid )
        CALL DIAGNOSTICS_FILL( sterht,
     &                         'STERICHT', 0,1, 0,1,1, myThid )
        CALL DIAGNOSTICS_SCALE_FILL( m_bp_nopabar, recip_gravity, 1,
     &                         'OBP     ', 0,1, 0,1,1, myThid )
#endif /* ALLOW_IB_CORR */
#endif /* ATMOSPHERIC_LOADING */
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
           m_UE(i,j,k,bi,bj) = 0. _d 0
           m_VN(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL ROTATE_UV2EN_RL(
     U          uVel, vVel, m_UE, m_VN,
     I          .TRUE., .TRUE., .FALSE., Nr, myThid )

c--   trVol : volume flux    --- [m^3/sec] (order of 10^6 = 1 Sv)
c--   trHeat: heat transport --- [Watt] (order of 1.E15 = PW)
c--   trSalt: salt transport --- [kg/sec] (order 1.E9 equiv. 1 Sv in vol.)
c--       convert from [ppt*m^3/sec] via rhoConst/1000.
c--       ( 1ppt = 1000*[mass(salt)]/[mass(seawater)] )

c-- init
      CALL ECCO_ZERO( trVol,  Nr, zeroRL, myThid )
      CALL ECCO_ZERO( trHeat, Nr, zeroRL, myThid )
      CALL ECCO_ZERO( trSalt, Nr, zeroRL, myThid )

#ifdef ALLOW_GENCOST_CONTRIBUTION

cts ---
c First: Fill the following SCALAR masks & weights for each (i,j,k,bi,bj) grid cell
c   tmpvol - 3D cell volume
c   tmpmsk - mask for the gencost_barfile field (e.g. theta)
c            Either: expand from 2D mask gencost_mskCsurf across nonzero
c            entries of gencost_mskVertical (Nr x NGENCOST array)
c            or
c            copy from 3D mask gencost_mskC
cts ---
      DO kgen=1,NGENCOST

       itr = gencost_itracer(kgen)

       CALL ECCO_ZERO( gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &                 1, zeroRL, myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         areavolTile(bi,bj)=0. _d 0
        ENDDO
       ENDDO
       areavolGlob=0. _d 0

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i =  1,sNx
c---------
           DO k = 1,Nr
            tmpvol=hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)

            tmpmsk=0. _d 0
            IF (.NOT.gencost_msk_is3d(kgen)) THEN
             tmpmsk=gencost_mskCsurf(i,j,bi,bj,kgen)*
     &              gencost_mskVertical(k,kgen)
#ifdef ALLOW_GENCOST3D
            ELSE
             kgen3d=gencost_msk_pointer3d(kgen)
             tmpmsk=gencost_mskC(i,j,k,bi,bj,kgen3d)
#endif /* ALLOW_GENCOST3D */
            ENDIF

C ---- If density mask is enabled, use it here ----
            IF ( maskC(i,j,k,bi,bj).EQ.oneRS .AND.
     &           gencost_useDensityMask(kgen) ) THEN
C            - first, calculate the scalar density
             CALL FIND_RHO_SCALAR(
     I              theta(i,j,k,bi,bj),
     I              salt(i,j,k,bi,bj),
     I              gencost_refPressure(kgen),
     O              tmpsig,
     I              myThid )
C            - subtract 1000 to get sigma
             tmpsig = tmpsig - 1000. _d 0
C            - now, tmpmsk is sigmoid times this value
             tmpsig_lower = 0.5 + 0.5*tanh(gencost_tanhScale(kgen)
     &           *(tmpsig-gencost_sigmaLow(kgen)))
             tmpsig_upper = 0.5 - 0.5*tanh(gencost_tanhScale(kgen)
     &           *(tmpsig-gencost_sigmaHigh(kgen)))
C             - update mask value based on the sigmoid function
             tmp_sigmsk = tmpsig_lower*tmpsig_upper
             tmpmsk = tmpmsk*tmp_sigmsk
            ENDIF
C ---- end of density mask (but tmpmsk is used below)

cts ---
c Now: at each (i,j,k,bi,bj) fill the SCALAR variables
c   tmpfld - from 3D field theta, salt, ptracer
c            or
c            from 2D field with eta, shelfice
c
c   tmpmsk2 - 1 or 0 weighting for areavolTile
cts ---
            tmpfld=0. _d 0
            tmpmsk2=0. _d 0
            IF (gencost_barfile(kgen)(1:15).EQ.'m_boxmean_theta') THEN
             tmpfld=theta(i,j,k,bi,bj)
             IF (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
            ELSEIF (gencost_barfile(kgen)(1:14).EQ.'m_boxmean_salt')
     &        THEN
             tmpfld=salt(i,j,k,bi,bj)
             IF (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
#ifdef ALLOW_PTRACERS
            ELSEIF (gencost_barfile(kgen)(1:17).EQ.'m_boxmean_ptracer')
     &        THEN
             tmpfld=pTracer(i,j,k,bi,bj,itr)
             IF (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
#endif /* ALLOW_PTRACERS */
            ENDIF

cts ---
c Fill 3D field
c       gencost_store - masked field of interest * grid cell volume
c                       note: this accumulates along z dim
c
c Fill tile field (1 val per tile)
c       areavolTile - volume of each tile, this gets summed to a global
c                     value
cts ---
            gencost_storefld(i,j,bi,bj,kgen) =
     &          gencost_storefld(i,j,bi,bj,kgen)
     &          +tmpmsk*tmpfld*tmpvol
            areavolTile(bi,bj)=areavolTile(bi,bj)
     &          +tmpmsk2*eccoVol_0(i,j,k,bi,bj)

           ENDDO ! Ends do k=1,Nr

           tmpmsk  = 0. _d 0
           tmpfld  = 0. _d 0
           tmpmsk2 = 0. _d 0
           IF (gencost_barfile(kgen)(1:13).EQ.'m_boxmean_eta') THEN
            tmpmsk=maskC(i,j,1,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)
            tmpfld = m_eta(i,j,bi,bj)
#if (defined ATMOSPHERIC_LOADING) && (defined ALLOW_IB_CORR)
            IF (gencost_barfile(kgen)(1:17).EQ.'m_boxmean_eta_dyn') THEN
             tmpfld = m_eta_dyn(i,j,bi,bj)
            ENDIF
#endif
            IF (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
           ENDIF
#ifdef ALLOW_SHELFICE
cts ---
c Shelfice:
c   Simply accumulate shelfice FWF or HF into tmpfld here
c   This will fill gencost_storefld with this value *rA
c   For FreshWaterFlux
c           gencost_storefld = shelficefreshwaterflux / rho * rA
c                            = [kg/m^2/s] / [kg/m^3] * [m^2]
c                            = [m^3/s]
c
c   For heatflux
c           gencost_storefld = shelficeheatflux * rA
c                            = [W/m^2] *[m^2]
c                            = [W]
cts ---
           IF((gencost_barfile(kgen)(1:16).EQ.'m_boxmean_shifwf').OR.
     &        (gencost_barfile(kgen)(1:16).EQ.'m_boxmean_shihtf')) THEN

            tmpmsk=maskSHI(i,j,1,bi,bj)*
     &             gencost_mskCsurf(i,j,bi,bj,kgen)

            IF (gencost_barfile(kgen)(11:16).EQ.'shifwf') THEN
             tmpfld=shelficeFreshWaterFlux(i,j,bi,bj) / rhoConstFresh
            ELSEIF (gencost_barfile(kgen)(11:16).EQ.'shihtf') THEN
             tmpfld=shelficeHeatFlux(i,j,bi,bj)
            ENDIF
            IF (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
           ENDIF
#endif /* ALLOW_SHELFICE */

cts ---
c Fill 2D field
c   gencost_store - masked field of interest * rA
c
c Fill tile field (1 val per tile)
c       areavolTile - total rA on each tile for mskC != 0
cts ---
           gencost_storefld(i,j,bi,bj,kgen) =
     &        gencost_storefld(i,j,bi,bj,kgen)
     &        +tmpmsk*tmpfld*rA(i,j,bi,bj)
           areavolTile(bi,bj)=areavolTile(bi,bj)
     &        +tmpmsk2*rA(i,j,bi,bj)
c---------
           DO k = 1,Nr

            tmpmskW=0. _d 0
            tmpmskS=0. _d 0
            IF (.NOT.gencost_msk_is3d(kgen)) THEN
              tmpmskW=gencost_mskWsurf(i,j,bi,bj,kgen)
     &          *gencost_mskVertical(k,kgen)
              tmpmskS=gencost_mskSsurf(i,j,bi,bj,kgen)
     &          *gencost_mskVertical(k,kgen)
#ifdef ALLOW_GENCOST3D
            ELSE
              kgen3d=gencost_msk_pointer3d(kgen)
              tmpmskW=gencost_mskW(i,j,k,bi,bj,kgen3d)
              tmpmskS=gencost_mskS(i,j,k,bi,bj,kgen3d)
#endif /* ALLOW_GENCOST3D */
            ENDIF
            tmpmskW=tmpmskW*hFacW(i,j,k,bi,bj)*dyG(i,j,bi,bj)*drF(k)
            tmpmskS=tmpmskS*hFacS(i,j,k,bi,bj)*dxG(i,j,bi,bj)*drF(k)

            IF (gencost_barfile(kgen)(1:13).EQ.'m_horflux_vol') THEN
              gencost_storefld(i,j,bi,bj,kgen) =
     &          gencost_storefld(i,j,bi,bj,kgen)
     &          +uVel(i,j,k,bi,bj)*tmpmskW
     &          +vVel(i,j,k,bi,bj)*tmpmskS

            ! Only compute tr[Vol,Heat,Salt] if necessary, use
            ! gencost_mask[W/S] rather than old msktrVol
            ELSEIF ( gencost_barfile(kgen)(1:7).EQ.'m_trVol' .OR.
     &               gencost_barfile(kgen)(1:8).EQ.'m_trHeat'.OR.
     &               gencost_barfile(kgen)(1:8).EQ.'m_trSalt'    ) THEN

             trVolW(i,j,k) =
     &                 uVel(i,j,k,bi,bj)*tmpmskW
     &                *maskInW(i,j,bi,bj)
             trVolS(i,j,k) =
     &                 vVel(i,j,k,bi,bj)*tmpmskS
     &                *maskInS(i,j,bi,bj)

             trHeatW(i,j,k) = trVolW(i,j,k)
     &                *(theta(i,j,k,bi,bj)+theta(i-1,j,k,bi,bj))*halfRL
     &                *HeatCapacity_Cp*rhoConst
             trHeatS(i,j,k) = trVolS(i,j,k)
     &                *(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))*halfRL
     &                *HeatCapacity_Cp*rhoConst

             trSaltW(i,j,k) = trVolW(i,j,k)
     &                *(salt(i,j,k,bi,bj)+salt(i-1,j,k,bi,bj))*halfRL
     &                *rhoConst/1000.
             trSaltS(i,j,k) = trVolS(i,j,k)
     &                *(salt(i,j,k,bi,bj)+salt(i,j-1,k,bi,bj))*halfRL
     &                *rhoConst/1000.
c now summing
             trVol(i,j,k,bi,bj)=trVolW(i,j,k)+trVolS(i,j,k)
             trHeat(i,j,k,bi,bj)=trHeatW(i,j,k)+trHeatS(i,j,k)
             trSalt(i,j,k,bi,bj)=trSaltW(i,j,k)+trSaltS(i,j,k)

            ENDIF
C     end k-loop
           ENDDO
c---------
          ENDDO
         ENDDO
        ENDDO
       ENDDO

cts ---
c Divide all values in gencost_storefld by
c   areavolGlob: scalar representing global volume of
c                quantity of interest.
c
c Note: for shelfice, do not take this final average to make
c       comparable to shelfice_cost_final.
cts ---
       IF (gencost_barfile(kgen)(1:9).EQ.'m_boxmean' .AND.
     &    gencost_barfile(kgen)(11:13).NE.'shi') THEN
        CALL GLOBAL_SUM_TILE_RL( areavolTile, areavolGlob, myThid )
        CALL ECCO_DIV( gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &                 areavolGlob, 1, 1, myThid )
       ENDIF

C     end kgen-loop
      ENDDO

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      END
