#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

      subroutine cost_gencost_customize( mythid )

c     ==================================================================
c     SUBROUTINE cost_gencost_customize
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_FIELDS.h"
#endif
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer bi,bj
      integer i,j,k
#ifdef ALLOW_GENCOST3D
      integer k2,kk
      integer itr
#endif
      integer kLev
#ifdef ALLOW_EXF
      _RL uBarC, vBarC
      _RL zontau        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL mertau        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL zonwind        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL merwind        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
#endif
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

#ifdef ALLOW_EXF
c rotated to EW/NS tracer point
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do j = jmin,jmax
              do i = imin,imax
                uBarC = 0.5 _d 0
     &           *(ustress(i,j,bi,bj)+ustress(i+1,j,bi,bj))
                vBarC = 0.5 _d 0
     &           *(vstress(i,j,bi,bj)+vstress(i,j+1,bi,bj))
                zontau(i,j,bi,bj) = angleCosC(i,j,bi,bj)*uBarC
     &                           -angleSinC(i,j,bi,bj)*vBarC
                mertau(i,j,bi,bj) = angleSinC(i,j,bi,bj)*uBarC
     &                           +angleCosC(i,j,bi,bj)*vBarC
              enddo
            enddo
          enddo
        enddo

c the following should be identical to the above
c     CALL ROTATE_UV2EN_RL(ustress,vstress,zontau,mertau,
c    &     .TRUE.,.TRUE.,.TRUE.,1,myThid)

      CALL ROTATE_UV2EN_RL(uwind,vwind,zonwind,merwind,
     &     .TRUE.,.FALSE.,.TRUE.,1,myThid)
#endif

      do k=1,NGENCOST
      itr=gencost_itracer(k)
      kLev = MAX( 1, MIN( Nr, gencost_kLev_select(k) ) )
      do bj = jtlo,jthi
       do bi = itlo,ithi
        do j = jmin,jmax
         do i =  imin,imax

         if (gencost_barfile(k)(1:5).EQ.'m_eta' .and.
     &       gencost_barfile(k)(1:9).NE.'m_eta_dyn') then
           gencost_modfld(i,j,bi,bj,k) =
     &      m_eta(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:9).EQ.'m_boxmean') then
           gencost_modfld(i,j,bi,bj,k) =
     &      gencost_storefld(i,j,bi,bj,k)
         elseif (gencost_barfile(k)(1:9).EQ.'m_horflux') then
           gencost_modfld(i,j,bi,bj,k) =
     &      gencost_storefld(i,j,bi,bj,k)
         elseif (gencost_barfile(k)(1:5).EQ.'m_sst') then
           gencost_modfld(i,j,bi,bj,k) =
     &      THETA(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:5).EQ.'m_sss') then
           gencost_modfld(i,j,bi,bj,k) =
     &      SALT(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:11).EQ.'m_drifterUE') then
           gencost_modfld(i,j,bi,bj,k) =
     &      m_UE(i,j,kLev,bi,bj)*maskC(i,j,kLev,bi,bj)
         elseif (gencost_barfile(k)(1:11).EQ.'m_drifterVN') then
           gencost_modfld(i,j,bi,bj,k) =
     &      m_VN(i,j,kLev,bi,bj)*maskC(i,j,kLev,bi,bj)
         elseif (gencost_barfile(k)(1:4).EQ.'m_bp' .and.
     &           gencost_barfile(k)(1:12).NE.'m_bp_nopabar') then
           gencost_modfld(i,j,bi,bj,k) =
     &      m_bp(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#ifdef ATMOSPHERIC_LOADING
#ifdef ALLOW_IB_CORR
         elseif (gencost_barfile(k)(1:9).EQ.'m_eta_dyn') then
           gencost_modfld(i,j,bi,bj,k) =
     &      m_eta_dyn(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:12).EQ.'m_bp_nopabar') then
           gencost_modfld(i,j,bi,bj,k) =
     &      m_bp_nopabar(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
         elseif (gencost_barfile(k)(1:16).EQ.'m_geothermalflux') then
           gencost_modfld(i,j,bi,bj,k) =
     &      geothermalFlux(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_EXF
         elseif (gencost_barfile(k)(1:9).EQ.'m_ustress') then
           gencost_modfld(i,j,bi,bj,k) =
     &      zontau(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:9).EQ.'m_vstress') then
           gencost_modfld(i,j,bi,bj,k) =
     &      mertau(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:7).EQ.'m_uwind') then
           gencost_modfld(i,j,bi,bj,k) =
     &      zonwind(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:7).EQ.'m_vwind') then
           gencost_modfld(i,j,bi,bj,k) =
     &      merwind(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#ifdef ALLOW_ATM_TEMP
         elseif (gencost_barfile(k)(1:7).EQ.'m_atemp') then
           gencost_modfld(i,j,bi,bj,k) =
     &      atemp(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:5).EQ.'m_aqh') then
           gencost_modfld(i,j,bi,bj,k) =
     &      aqh(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:8).EQ.'m_precip') then
           gencost_modfld(i,j,bi,bj,k) =
     &      precip(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_DOWNWARD_RADIATION
         elseif (gencost_barfile(k)(1:8).EQ.'m_swdown') then
           gencost_modfld(i,j,bi,bj,k) =
     &      swdown(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:8).EQ.'m_lwdown') then
           gencost_modfld(i,j,bi,bj,k) =
     &      lwdown(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
         elseif (gencost_barfile(k)(1:8).EQ.'m_wspeed') then
           gencost_modfld(i,j,bi,bj,k) =
     &      wspeed(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif /* ALLOW_EXF */
#ifdef ALLOW_CTRL
#ifdef ALLOW_BOTTOMDRAG_CONTROL
         elseif (gencost_barfile(k)(1:12).EQ.'m_bottomdrag') then
           gencost_modfld(i,j,bi,bj,k) =
     &      bottomDragFld(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
#endif
#ifdef ALLOW_SEAICE
         elseif ( (gencost_name(k).EQ.'siv4-conc').OR.
     &            (gencost_barfile(k)(1:8).EQ.'m_siarea') ) then
           gencost_modfld(i,j,bi,bj,k) =
     &      area(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_name(k).EQ.'siv4-deconc') then
           gencost_modfld(i,j,bi,bj,k) =
     &      theta(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
         elseif ( (gencost_name(k).EQ.'siv4-exconc').OR.
     &            (gencost_barfile(k)(1:8).EQ.'m_siheff') ) then
           gencost_modfld(i,j,bi,bj,k) =
     &      heff(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:9).EQ.'m_sihsnow') then
           gencost_modfld(i,j,bi,bj,k) =
     &      hsnow(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_GENCOST3D
         elseif (gencost_barfile(k)(1:7).EQ.'m_theta') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       theta(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
         elseif (gencost_barfile(k)(1:6).EQ.'m_salt') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       salt(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
#ifdef ALLOW_PTRACERS
         elseif (gencost_barfile(k)(1:9).EQ.'m_ptracer') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       pTracer(i,j,k2,bi,bj,itr)*maskC(i,j,k2,bi,bj)
           enddo
#endif
         elseif (gencost_barfile(k)(1:4).EQ.'m_UE') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       m_UE(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
         elseif (gencost_barfile(k)(1:4).EQ.'m_VN') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       m_VN(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
         elseif (gencost_barfile(k)(1:7).EQ.'m_trVol') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
             gencost_mod3d(i,j,k2,bi,bj,kk) = trVol(i,j,k2,bi,bj)
           enddo
         elseif (gencost_barfile(k)(1:8).EQ.'m_trHeat') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
             gencost_mod3d(i,j,k2,bi,bj,kk) = trHeat(i,j,k2,bi,bj)
           enddo
         elseif (gencost_barfile(k)(1:8).EQ.'m_trSalt') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
             gencost_mod3d(i,j,k2,bi,bj,kk) = trSalt(i,j,k2,bi,bj)
           enddo
#if (defined (ALLOW_3D_DIFFKR) || defined (ALLOW_DIFFKR_CONTROL))
         elseif (gencost_barfile(k)(1:8).EQ.'m_diffkr') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       diffkr(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
#endif
#ifdef ALLOW_CTRL
#if ( defined ALLOW_KAPGM_CONTROL && defined GM_READ_K3D_GM )
         elseif (gencost_barfile(k)(1:7).EQ.'m_kapgm') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       GM_inpK3dGM(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
#endif
#if ( defined ALLOW_KAPREDI_CONTROL && defined GM_READ_K3D_REDI )
         elseif (gencost_barfile(k)(1:9).EQ.'m_kapredi') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       GM_inpK3dRedi(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
#endif
#endif
#endif /* ALLOW_GENCOST3D */
         endif

         enddo
        enddo
       enddo
      enddo
      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      RETURN
      END
