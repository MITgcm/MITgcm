C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_customize.F,v 1.11 2014/10/22 13:28:27 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
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
# include "ecco.h"
#endif
#ifdef ALLOW_SEAICE
#  include "SEAICE_SIZE.h"
#  include "SEAICE.h"
#endif
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GENCOST_CONTRIBUTION
c     == local variables ==

      integer bi,bj
      integer i,j,k
#ifdef ALLOW_GENCOST3D
      integer k2,kk
#endif
#ifdef ALLOW_EXF
      _RL uBarC, vBarC
#endif

      _RL tauZonC        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL tauMerC        (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
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
                tauZonC(i,j,bi,bj) = angleCosC(i,j,bi,bj)*uBarC
     &                           -angleSinC(i,j,bi,bj)*vBarC
                tauMerC(i,j,bi,bj) = angleSinC(i,j,bi,bj)*uBarC
     &                           +angleCosC(i,j,bi,bj)*vBarC
              enddo
            enddo
          enddo
        enddo

#endif



      do k=1,NGENCOST
      do bj = jtlo,jthi
       do bi = itlo,ithi
        do j = jmin,jmax
         do i =  imin,imax

         if (gencost_barfile(k)(1:3).EQ.'eta') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      etanFull(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:3).EQ.'sst') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      THETA(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:3).EQ.'sss') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      SALT(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:2).EQ.'bp') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      phiHydLow(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:6).EQ.'tauZon') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      tauZonC(i,j,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:6).EQ.'tauMer') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      tauMerC(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#ifdef ALLOW_SEAICE
         elseif (gencost_name(k).EQ.'siv4-conc') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      area(i,j,bi,bj)*maskC(i,j,1,bi,bj) 
         elseif (gencost_name(k).EQ.'siv4-deconc') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      theta(i,j,1,bi,bj)*maskC(i,j,1,bi,bj)
         elseif (gencost_name(k).EQ.'siv4-exconc') then  
           gencost_modfld(i,j,bi,bj,k) = 
     &      heff(i,j,bi,bj)*maskC(i,j,1,bi,bj)
#endif
#ifdef ALLOW_GENCOST3D
         elseif (gencost_barfile(k)(1:5).EQ.'theta') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       theta(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
         elseif (gencost_barfile(k)(1:4).EQ.'salt') then
           kk=gencost_pointer3d(k)
           do k2=1,nr
            gencost_mod3d(i,j,k2,bi,bj,kk) =
     &       salt(i,j,k2,bi,bj)*maskC(i,j,k2,bi,bj)
           enddo
#endif
         endif

         enddo
        enddo
       enddo
      enddo
      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
