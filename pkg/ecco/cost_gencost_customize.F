C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_gencost_customize.F,v 1.8 2014/09/29 16:46:43 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif

      subroutine cost_gencost_customize( mythid )

c     ==================================================================
c     SUBROUTINE cost_gencost_assignperiod
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
#ifdef ALLOW_EXF
         elseif (gencost_barfile(k)(1:4).EQ.'taux') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      ustress(i,j,bi,bj)*maskW(i,j,1,bi,bj)
         elseif (gencost_barfile(k)(1:4).EQ.'tauy') then
           gencost_modfld(i,j,bi,bj,k) = 
     &      vstress(i,j,bi,bj)*maskS(i,j,1,bi,bj)
#endif
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
       endif

         enddo
        enddo
       enddo
      enddo
      enddo

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      end
