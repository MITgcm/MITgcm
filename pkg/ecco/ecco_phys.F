#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif

      subroutine ecco_phys( myThid )

c     ==================================================================
c     SUBROUTINE ecco_phys
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE ecco_phys
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "GRID.h"
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

c     == routine arguments ==

      integer myThid

c     == local variables ==

      integer bi,bj
      integer i,j,k
#ifdef ALLOW_GENCOST_CONTRIBUTION
      integer kgen, kgen3d, itr
      _RL areavolTile(nSx,nSy), areavolGlob
      _RL tmpfld, tmpvol, tmpmsk, tmpmsk2, tmpmskW, tmpmskS
      _RL tmp_sigmsk, tmpsig, tmpsig_lower, tmpsig_upper
#endif

c- note defined with overlap here, not needed, but more efficient
      _RL trVolW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trVolS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trHeatW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trHeatS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trSaltW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trSaltS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
      _RL AREAsumTile(nSx,nSy),PLOADsumTile(nSx,nSy)
      _RL tmpfac
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
      _RL sIceLoadFacLoc
#endif
#ifdef ALLOW_PSBAR_STERIC
      _RL RHOInSituLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VOLsumTile(nSx,nSy),RHOsumTile(nSx,nSy)
#endif

#ifdef ALLOW_PSBAR_STERIC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 1,Nr
         CALL FIND_RHO_2D(
     I                1-OLx, sNx+OLx, 1-OLy, sNy+OLy, k,
     I                theta(1-OLx,1-OLy,k,bi,bj),
     I                salt (1-OLx,1-OLy,k,bi,bj),
     O                RHOInSituLoc(1-OLx,1-OLy,k,bi,bj),
     I                k, bi, bj, myThid )
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        RHOsumTile(bi,bj)=0. _d 0
        VOLsumTile(bi,bj)=0. _d 0
        VOLsumGlob=0. _d 0
        RHOsumGlob=0. _d 0
        DO k = 1,Nr
         DO j = 1,sNy
          DO i =  1,sNx
           RHOsumTile(bi,bj)=RHOsumTile(bi,bj)+
     &          (rhoConst+RHOInSituLoc(i,j,k,bi,bj))*
     &          hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
           VOLsumTile(bi,bj)=VOLsumTile(bi,bj)+
     &          hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL GLOBAL_SUM_TILE_RL( VOLsumTile, VOLsumGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( RHOsumTile, RHOsumGlob, myThid )
      RHOsumGlob=RHOsumGlob/VOLsumGlob

      IF (RHOsumGlob_0.GT.0. _d 0) THEN
        sterGloH=VOLsumGlob_0/globalArea
     &        *(1. _d 0 - RHOsumGlob/RHOsumGlob_0)
      ELSE
        sterGloH=0. _d 0
      ENDIF

c     WRITE(msgBuf,'(A,1PE21.14)') ' sterGloH= ', sterGloH
c        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                       SQUEEZE_RIGHT, myThid )

#endif

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
      tmpfac = recip_rhoConst*recip_gravity
      ploadbar = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        PLOADsumTile(bi,bj)=0. _d 0
        AREAsumTile(bi,bj)=0. _d 0
        AREAsumGlob=0. _d 0
        PLOADsumGlob=0. _d 0
        ploadbar = 0. _d 0
        DO j = 1,sNy
         DO i =  1,sNx
          PLOADsumTile(bi,bj)=PLOADsumTile(bi,bj)+
     &         pload(i,j,bi,bj)*
     &         maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
          AREAsumTile(bi,bj)=AREAsumTile(bi,bj)+
     &         maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL GLOBAL_SUM_TILE_RL( AREAsumTile, AREAsumGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( PLOADsumTile, PLOADsumGlob, myThid )
      ploadbar=PLOADsumGlob/AREAsumGlob
#endif
      sIceLoadFacLoc=zeroRL
      IF ( useRealFreshWaterFlux ) sIceLoadFacLoc=recip_rhoConst
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
C calculte total sea level including inverse barometer (IB) effect if
C  there is air pressure forcing
          m_eta(i,j,bi,bj)=
     &                (etan(i,j,bi,bj)
#ifdef ATMOSPHERIC_LOADING
     &                +sIceLoad(i,j,bi,bj)*sIceLoadFacLoc
#endif
#ifdef ALLOW_PSBAR_STERIC
     &                +sterGloH
#endif
     &                ) * maskC(i,j,1,bi,bj)

C calculte total ocean bottom pressure including air pressure if
C  there is any.
          m_bp(i,j,bi,bj)=
     &                (phiHydLow(i,j,bi,bj)
#ifdef ALLOW_PSBAR_STERIC
C add back the correction due to the global mean steric ssh change,
C     i.e. sterGloH computed in ecco_phys.F (units converted from m to m2/s2)
     &                 +sterGloH * gravity
#endif
     &                ) * maskC(i,j,1,bi,bj)

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
C calculate IB correction m_eta_ib (in m)
          m_eta_ib(i,j,bi,bj)=
     &           (ploadbar-pload(i,j,bi,bj))*tmpfac
     &           * maskC(i,j,1,bi,bj)
C calculte dynamic sea level for comparison with altimetry data (in m)
          m_eta_dyn(i,j,bi,bj)=
     &           (m_eta(i,j,bi,bj) - m_eta_ib(i,j,bi,bj))
     &           * maskC(i,j,1,bi,bj)

C calculate GRACE-equvivalent ocean bottom pressure (in m2/s2)
          m_bp_nopabar(i,j,bi,bj)=
     &                (phiHydLow(i,j,bi,bj)
#ifdef ALLOW_PSBAR_STERIC
C add back the correction due to the global mean steric ssh change,
C     i.e. sterGloH computed in ecco_phys.F (units converted from m to m2/s2)
     &                 +sterGloH * gravity
     &                 - ploadbar * recip_rhoConst
#endif
     &                ) * maskC(i,j,1,bi,bj)
#endif
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 1,Nr
         DO j = 1,sNy
          DO i =  1,sNx
           m_UE(i,j,k,bi,bj)=0. _d 0
           m_VN(i,j,k,bi,bj)=0. _d 0
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
      CALL ECCO_ZERO(trVol,Nr,zeroRL,myThid)
      CALL ECCO_ZERO(trHeat,Nr,zeroRL,myThid)
      CALL ECCO_ZERO(trSalt,Nr,zeroRL,myThid)

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

       CALL ECCO_ZERO(gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &      1,zeroRL,myThid)

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
c
            tmpmsk=0. _d 0
            IF (.NOT.gencost_msk_is3d(kgen)) THEN
             tmpmsk=gencost_mskCsurf(i,j,bi,bj,kgen)*
     &              gencost_mskVertical(k,kgen)
#ifdef ALLOW_GENCOST3D
            ELSE
             kgen3d=gencost_msk_pointer3d(kgen)
             tmpmsk=gencost_mskC(i,j,k,bi,bj,kgen3d)
#endif
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
#endif
            ENDIF
c
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
c
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
c
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
#endif
            ENDIF
            tmpmskW=tmpmskW*hFacW(i,j,k,bi,bj)*dyG(i,j,bi,bj)*drF(k)
            tmpmskS=tmpmskS*hFacS(i,j,k,bi,bj)*dxG(i,j,bi,bj)*drF(k)
c
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
        CALL ecco_div( gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &                 areavolGlob, 1, 1, myThid )
       ENDIF

C     end kgen-loop
      ENDDO

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      END
