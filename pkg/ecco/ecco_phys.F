C $Header: /u/gcmpack/MITgcm/pkg/ecco/ecco_phys.F,v 1.14 2016/09/20 13:29:11 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

      subroutine ecco_phys( mythid )

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
# include "ecco.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer jmin,jmax
      integer imin,imax
#ifdef ALLOW_GENCOST_CONTRIBUTION
      integer kgen
      _RL areavolTile(nSx,nSy)
      _RL areavolGlob, tmpmsk, tmpmsk2
      _RL tmpVol(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

c- note defined with overlap here, not needed, but more efficient
      _RL trVolW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trVolS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trHeatW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trHeatS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trSaltW(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL trSaltS(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ATMOSPHERIC_LOADING
      _RL sIceLoadFac
#endif
#ifdef ALLOW_PSBAR_STERIC
      _RL RHOInSituLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL VOLsumTile(nSx,nSy),RHOsumTile(nSx,nSy)
      _RL sterGloH
#endif

c need to include halos for find_rho_2d
      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy

#ifdef ALLOW_PSBAR_STERIC

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,nr
            CALL FIND_RHO_2D(
     I                iMin, iMax, jMin, jMax, k,
     I                theta(1-OLx,1-OLy,k,bi,bj),
     I                salt (1-OLx,1-OLy,k,bi,bj),
     O                RHOInSituLoc(1-OLx,1-OLy,k,bi,bj),
     I                k, bi, bj, myThid )
          enddo
        enddo
      enddo

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          RHOsumTile(bi,bj)=0. _d 0
          VOLsumTile(bi,bj)=0. _d 0
          VOLsumGlob=0. _d 0
          RHOsumGlob=0. _d 0
          do k = 1,nr
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

#endif

#ifdef ATMOSPHERIC_LOADING
      sIceLoadFac=zeroRL
      IF ( useRealFreshWaterFlux ) sIceLoadFac=recip_rhoConst
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
            do j = jmin,jmax
              do i =  imin,imax
                m_eta(i,j,bi,bj)=
     &                 etan(i,j,bi,bj)
#ifdef ATMOSPHERIC_LOADING
     &                +sIceLoad(i,j,bi,bj)*sIceLoadFac
#endif
#ifdef ALLOW_PSBAR_STERIC
     &                +sterGloH
#endif
              enddo
            enddo
        enddo
      enddo

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,nr
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
     I          .TRUE., .TRUE., .FALSE., Nr, mythid )


c--   trVol : volume flux    --- [m^3/sec] (order of 10^6 = 1 Sv)
c--   trHeat: heat transport --- [Watt] (order of 1.E15 = PW)
c--   trSalt: salt transport --- [kg/sec] (order 1.E9 equiv. 1 Sv in vol.)
c--       convert from [ppt*m^3/sec] via rhoConst/1000.
c--       ( 1ppt = 1000*[mass(salt)]/[mass(seawater)] )

c-- init
      call ecco_zero(trVol,Nr,zeroRL,myThid)
      call ecco_zero(trHeat,Nr,zeroRL,myThid)
      call ecco_zero(trSalt,Nr,zeroRL,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c-- init: if done with overlap, more efficient. But since overwritten
c immediately afterward, init is probably not needed.
          do k = 1,nr
            do j = 1-OLy,sNy+Oly
              do i = 1-OLx,sNx+OLx
                trVolW(i,j,k)=0. _d 0
                trVolS(i,j,k)=0. _d 0
                trHeatW(i,j,k)=0. _d 0
                trHeatS(i,j,k)=0. _d 0
                trSaltW(i,j,k)=0. _d 0
                trSaltS(i,j,k)=0. _d 0
              enddo
            enddo
          enddo
          do k = 1,nr
            do j = 1,sNy
              do i =  1,sNx
                trVolW(i,j,k) =
     &                 uVel(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj)
     &                *dyG(i,j,bi,bj)*drF(k)*msktrVolW(i,j,bi,bj)
     &                *maskInW(i,j,bi,bj)
                trVolS(i,j,k) =
     &                 vVel(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj)
     &                *dxG(i,j,bi,bj)*drF(k)*msktrVolS(i,j,bi,bj)
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
                
              enddo
            enddo
          enddo
        enddo
      enddo

#ifdef ALLOW_GENCOST_CONTRIBUTION

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do k = 1,nr
            do j = 1-OLy,sNy+Oly
              do i = 1-OLx,sNx+OLx
                tmpVol(i,j,k,bi,bj)=
     &          hFacC(i,j,k,bi,bj)*drF(k)*rA(i,j,bi,bj)
              enddo
            enddo
          enddo
        enddo
      enddo

      do kgen=1,NGENCOST

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

         tmpmsk=0. _d 0
         if (gencost_mskCsurf(i,j,bi,bj,kgen).NE.0. _d 0)
     &      tmpmsk=1. _d 0

         if (gencost_barfile(kgen)(1:15).EQ.'m_boxmean_theta') then
          do k = 1,nr
           tmpmsk2=0. _d 0
           if (gencost_mskVertical(k,kgen).NE.0. _d 0) tmpmsk2=1. _d 0
           gencost_storefld(i,j,bi,bj,kgen) =
     &      gencost_storefld(i,j,bi,bj,kgen)+theta(i,j,k,bi,bj)
     &      *tmpVol(i,j,k,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)
     &      *gencost_mskVertical(k,kgen)
           areavolTile(bi,bj)=areavolTile(bi,bj)
     &       +tmpmsk*tmpmsk2*eccoVol_0(i,j,k,bi,bj)
          enddo
         elseif (gencost_barfile(kgen)(1:14).EQ.'m_boxmean_salt') then
          do k = 1,nr
           tmpmsk2=0. _d 0
           if (gencost_mskVertical(k,kgen).NE.0. _d 0) tmpmsk2=1. _d 0
           gencost_storefld(i,j,bi,bj,kgen) =
     &      gencost_storefld(i,j,bi,bj,kgen)+salt(i,j,k,bi,bj)
     &      *tmpVol(i,j,k,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)
     &      *gencost_mskVertical(k,kgen)
           areavolTile(bi,bj)=areavolTile(bi,bj)
     &       +tmpmsk*tmpmsk2*eccoVol_0(i,j,k,bi,bj)
          enddo
         elseif (gencost_barfile(kgen)(1:13).EQ.'m_boxmean_eta') then
           gencost_storefld(i,j,bi,bj,kgen) =
     &      gencost_storefld(i,j,bi,bj,kgen)+m_eta(i,j,bi,bj)
     &      *maskC(i,j,1,bi,bj)*gencost_mskCsurf(i,j,bi,bj,kgen)
     &      *rA(i,j,bi,bj)
           areavolTile(bi,bj)=areavolTile(bi,bj)
     &       +tmpmsk*maskC(i,j,1,bi,bj)*rA(i,j,bi,bj)

         elseif (gencost_barfile(kgen)(1:13).EQ.'m_horflux_vol') then
          do k = 1,nr
           gencost_storefld(i,j,bi,bj,kgen) =
     &      gencost_storefld(i,j,bi,bj,kgen)
     &      +uVel(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj)*dyG(i,j,bi,bj)*drF(k)
     &           *gencost_mskWsurf(i,j,bi,bj,kgen)
     &           *gencost_mskVertical(k,kgen)
     &      +vVel(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj)*dxG(i,j,bi,bj)*drF(k)
     &           *gencost_mskSsurf(i,j,bi,bj,kgen)
     &           *gencost_mskVertical(k,kgen)
          enddo
         endif

         enddo
        enddo
       enddo
      enddo

      if (gencost_barfile(kgen)(1:9).EQ.'m_boxmean') then
      CALL GLOBAL_SUM_TILE_RL( areavolTile, areavolGlob, myThid )
      CALL ecco_div( gencost_storefld(1-OLx,1-OLy,1,1,kgen),
     &     1, areavolGlob, myThid )
      endif

      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */


      return
      end
