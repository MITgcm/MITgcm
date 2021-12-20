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

      IMPLICIT NONE

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
      integer jmin,jmax
      integer imin,imax
#ifdef ALLOW_GENCOST_CONTRIBUTION
      integer kgen, kgen3d, itr
      _RL areavolTile(nSx,nSy), areavolGlob
      _RL tmpfld, tmpvol, tmpmsk, tmpmsk2, tmpmskW, tmpmskS
#endif

c- note defined with overlap here, not needed, but more efficient
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
      _RL AREAsumTile(nSx,nSy),PLOADsumTile(nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
#endif
      _RL RHOInSituLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#ifdef ALLOW_PSBAR_STERIC
      _RL VOLsumTile(nSx,nSy),RHOsumTile(nSx,nSy)
#endif
C     Mload     :: total mass load (kg/m**2)
      _RL Mload(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

c need to include halos for find_rho_2d
      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

      tmpfac = recip_rhoConst*recip_gravity
      sIceLoadFacLoc=zeroRL
      IF ( useRealFreshWaterFlux ) sIceLoadFacLoc=recip_rhoConst

c#ifdef ALLOW_PSBAR_STERIC
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,Nr
            CALL FIND_RHO_2D(
     I                iMin, iMax, jMin, jMax, k,
     I                theta(1-OLx,1-OLy,k,bi,bj),
     I                salt (1-OLx,1-OLy,k,bi,bj),
     O                RHOInSituLoc(1-OLx,1-OLy,k,bi,bj),
     I                k, bi, bj, myThid )
          enddo
        enddo
      enddo
c#endif /* ALLOW_PSBAR_STERIC */

#ifdef ALLOW_PSBAR_STERIC
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          RHOsumTile(bi,bj)=0. _d 0
          VOLsumTile(bi,bj)=0. _d 0
          VOLsumGlob=0. _d 0
          RHOsumGlob=0. _d 0
          do k = 1,Nr
            do j = 1,sNy
              do i =  1,sNx
                RHOsumTile(bi,bj)=RHOsumTile(bi,bj)+
     &            (rhoConst+RHOInSituLoc(i,j,k,bi,bj))*
     &            hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
                VOLsumTile(bi,bj)=VOLsumTile(bi,bj)+
     &            hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
              enddo
            enddo
          enddo
        enddo
      enddo
      CALL GLOBAL_SUM_TILE_RL( VOLsumTile, VOLsumGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( RHOsumTile, RHOsumGlob, myThid )
      RHOsumGlob=RHOsumGlob/VOLsumGlob

      if (RHOsumGlob_0.GT.0. _d 0) then
        sterGloH=VOLsumGlob_0/globalArea
     &        *(1. _d 0 - RHOsumGlob/RHOsumGlob_0)
      else
        sterGloH=0. _d 0
      endif

c     WRITE(msgBuf,'(A,1PE21.14)') ' sterGloH= ', sterGloH
c        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
c    &                       SQUEEZE_RIGHT, myThid )

#endif /* ALLOW_PSBAR_STERIC */

#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
      ploadbar = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          PLOADsumTile(bi,bj)=0. _d 0
          AREAsumTile(bi,bj)=0. _d 0
          AREAsumGlob=0. _d 0
          PLOADsumGlob=0. _d 0
          ploadbar = 0. _d 0
          do j = 1,sNy
            do i =  1,sNx
              PLOADsumTile(bi,bj)=PLOADsumTile(bi,bj)+
     &          pload(i,j,bi,bj)*
     &          maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
              AREAsumTile(bi,bj)=AREAsumTile(bi,bj)+
     &          maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo
      CALL GLOBAL_SUM_TILE_RL( AREAsumTile, AREAsumGlob, myThid )
      CALL GLOBAL_SUM_TILE_RL( PLOADsumTile, PLOADsumGlob, myThid )
      ploadbar=PLOADsumGlob/AREAsumGlob
#endif /* ALLOW_IB_CORR */
#endif /* ATMOSPHERIC_LOADING */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
            do j = jmin,jmax
              do i =  imin,imax
C calculte total sea level including inverse barometer (IB) effect if
C  there is air pressure forcing
                m_eta(i,j,bi,bj)=
     &                (etan(i,j,bi,bj)
     &                +sIceLoad(i,j,bi,bj)*sIceLoadFacLoc
#ifdef ALLOW_PSBAR_STERIC
C apply Greatbatch correction
     &                +sterGloH
#endif /* ALLOW_PSBAR_STERIC */
     &                ) * maskC(i,j,1,bi,bj)

C Model equivalent of ocean bottom pressure gauge data (in m^2/s^2)
C  that are NOT corrected for global ocean mean atmospheric pressure variations
C  = Ocean mass + sea-ice & snow load + air-pressure load + Greatbatch corr.
C  (all terms on RHS are converted to m^2/s^2). It is
C  essentially Mload (as in pkg/sbo/sbo_calc.F) plus air-pressure load
                m_bp(i,j,bi,bj) =  gravity * (etaN(i,j,bi,bj)
     &           - R_low(i,j,bi,bj)
#ifdef ALLOW_PSBAR_STERIC
C add back the correction due to the global mean steric ssh change,
C     i.e. sterGloH computed above (units converted from m to m2/s2)
     &           + sterGloH
#endif /* ALLOW_PSBAR_STERIC */
     &           )
C sIceLoad in kg/m^2
     &           + sIceLoad(i,j,bi,bj) * gravity * sIceLoadFacLoc
C pLoad in N/m^2
     &           + pLoad(i,j,bi,bj) * recip_rhoConst
C integrate rho_anomaly through water column
                DO k = 1, Nr
                   m_bp(i,j,bi,bj) = m_bp(i,j,bi,bj) +
     &               RHOInSituLoc(i,j,k,bi,bj)*drF(k)*hFacC(i,j,k,bi,bj)
     &               * gravity * recip_rhoConst
                ENDDO
                m_bp(i,j,bi,bj) = m_bp(i,j,bi,bj) * maskC(i,j,1,bi,bj)

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
C  by removing global ocean mean atmospheric pressure variations
                m_bp_nopabar(i,j,bi,bj)=
     &           (m_bp(i,j,bi,bj)
     &            - ploadbar * recip_rhoConst
     &           ) * maskC(i,j,1,bi,bj)
C calculate steric height
C (in m; = m_eta_dyn - (m_bp_nopabr * recip_gravity + R_low))
C R_low (<0) is Depth in m.
                sterht(i,j,bi,bj)=
     &           m_eta_dyn(i,j,bi,bj)
     &           - (m_bp_nopabar(i,j,bi,bj) * recip_gravity
     &              + R_low(i,j,bi,bj))
#endif /* ALLOW_IB_CORR */
#endif /* ATMOSPHERIC_LOADING */
              enddo
            enddo
        enddo
      enddo

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
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
          do k = 1,Nr
            do j = 1,sNy
              do i =  1,sNx
                m_UE(i,j,k,bi,bj)=0. _d 0
                m_VN(i,j,k,bi,bj)=0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

      CALL ROTATE_UV2EN_RL(
     U          uVel, vVel, m_UE, m_VN,
     I          .TRUE., .TRUE., .FALSE., Nr, myThid )

c--   trVol : volume flux    --- [m^3/sec] (order of 10^6 = 1 Sv)
c--   trHeat: heat transport --- [Watt] (order of 1.E15 = PW)
c--   trSalt: salt transport --- [kg/sec] (order 1.E9 equiv. 1 Sv in vol.)
c--       convert from [ppt*m^3/sec] via rhoConst/1000.
c--       ( 1ppt = 1000*[mass(salt)]/[mass(seawater)] )

c-- init
      call ecco_zero(trVol,Nr,zeroRL,myThid)
      call ecco_zero(trHeat,Nr,zeroRL,myThid)
      call ecco_zero(trSalt,Nr,zeroRL,myThid)

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
      do kgen=1,NGENCOST

      itr = gencost_itracer(kgen)

      call ecco_zero(gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &     1,zeroRL,myThid)

      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
         areavolTile(bi,bj)=0. _d 0
       enddo
      enddo
      areavolGlob=0. _d 0

      do bj=myByLo(myThid),myByHi(myThid)
       do bi=myBxLo(myThid),myBxHi(myThid)
        do j = 1,sNy
         do i =  1,sNx
c---------
          do k = 1,Nr
            tmpvol=hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
c
            tmpmsk=0. _d 0
            if (.NOT.gencost_msk_is3d(kgen)) then
              tmpmsk=gencost_mskCsurf(i,j,bi,bj,kgen)*
     &               gencost_mskVertical(k,kgen)
#ifdef ALLOW_GENCOST3D
            else
              kgen3d=gencost_msk_pointer3d(kgen)
              tmpmsk=gencost_mskC(i,j,k,bi,bj,kgen3d)
#endif /* ALLOW_GENCOST3D */
            endif
c
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
            if (gencost_barfile(kgen)(1:15).EQ.'m_boxmean_theta') then
              tmpfld=theta(i,j,k,bi,bj)
              if (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
            elseif (gencost_barfile(kgen)(1:14).EQ.'m_boxmean_salt')
     &        then
              tmpfld=salt(i,j,k,bi,bj)
              if (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
#ifdef ALLOW_PTRACERS
            elseif (gencost_barfile(kgen)(1:17).EQ.'m_boxmean_ptracer')
     &        then
              tmpfld=pTracer(i,j,k,bi,bj,itr)
              if (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
#endif /* ALLOW_PTRACERS */
            endif
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
          enddo ! Ends do k=1,Nr

          tmpmsk  = 0. _d 0
          tmpfld  = 0. _d 0
          tmpmsk2 = 0. _d 0
          if (gencost_barfile(kgen)(1:13).EQ.'m_boxmean_eta') then
            tmpmsk=maskC(i,j,1,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)
            tmpfld = m_eta(i,j,bi,bj)
#if (defined ATMOSPHERIC_LOADING) && (defined ALLOW_IB_CORR)
            if (gencost_barfile(kgen)(1:17).EQ.'m_boxmean_eta_dyn') then
              tmpfld = m_eta_dyn(i,j,bi,bj)
            endif
#endif
            if (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
          endif
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
          if((gencost_barfile(kgen)(1:16).EQ.'m_boxmean_shifwf').or.
     &       (gencost_barfile(kgen)(1:16).EQ.'m_boxmean_shihtf')) then

            tmpmsk=maskSHI(i,j,1,bi,bj)*
     &             gencost_mskCsurf(i,j,bi,bj,kgen)

            if (gencost_barfile(kgen)(11:16).EQ.'shifwf') then
              tmpfld=shelficeFreshWaterFlux(i,j,bi,bj) / rhoConstFresh
            elseif (gencost_barfile(kgen)(11:16).EQ.'shihtf') then
              tmpfld=shelficeHeatFlux(i,j,bi,bj)
            endif
            if (tmpmsk.NE.0. _d 0) tmpmsk2=1. _d 0
          endif
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
          do k = 1,Nr
c
            tmpmskW=0. _d 0
            tmpmskS=0. _d 0
            if (.NOT.gencost_msk_is3d(kgen)) then
              tmpmskW=gencost_mskWsurf(i,j,bi,bj,kgen)
     &          *gencost_mskVertical(k,kgen)
              tmpmskS=gencost_mskSsurf(i,j,bi,bj,kgen)
     &          *gencost_mskVertical(k,kgen)
#ifdef ALLOW_GENCOST3D
            else
              kgen3d=gencost_msk_pointer3d(kgen)
              tmpmskW=gencost_mskW(i,j,k,bi,bj,kgen3d)
              tmpmskS=gencost_mskS(i,j,k,bi,bj,kgen3d)
#endif /* ALLOW_GENCOST3D */
            endif
            tmpmskW=tmpmskW*hFacW(i,j,k,bi,bj)*dyG(i,j,bi,bj)*drF(k)
            tmpmskS=tmpmskS*hFacS(i,j,k,bi,bj)*dxG(i,j,bi,bj)*drF(k)
c
            if (gencost_barfile(kgen)(1:13).EQ.'m_horflux_vol') then
              gencost_storefld(i,j,bi,bj,kgen) =
     &          gencost_storefld(i,j,bi,bj,kgen)
     &          +uVel(i,j,k,bi,bj)*tmpmskW
     &          +vVel(i,j,k,bi,bj)*tmpmskS

            ! Only compute tr[Vol,Heat,Salt] if necessary, use
            ! gencost_mask[W/S] rather than old msktrVol
            elseif ( gencost_barfile(kgen)(1:7).eq.'m_trVol' .or.
     &               gencost_barfile(kgen)(1:8).eq.'m_trHeat'.or.
     &               gencost_barfile(kgen)(1:8).eq.'m_trSalt'    ) then

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

            endif
          enddo
c---------
         enddo
        enddo
       enddo
      enddo

cts ---
c Divide all values in gencost_storefld by
c   areavolGlob: scalar representing global volume of
c                quantity of interest.
c
c Note: for shelfice, do not take this final average to make
c       comparable to shelfice_cost_final.
cts ---
      if (gencost_barfile(kgen)(1:9).EQ.'m_boxmean' .and.
     &    gencost_barfile(kgen)(11:13).NE.'shi') then
        CALL GLOBAL_SUM_TILE_RL( areavolTile, areavolGlob, myThid )
        CALL ecco_div( gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &                 areavolGlob, 1, 1, myThid )
      endif

      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      END
