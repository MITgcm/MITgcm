C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_wind.F,v 1.7 2007/03/08 15:09:52 mlosch Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      subroutine exf_wind(mytime, myiter, mythid)

c     ==================================================================
c     SUBROUTINE exf_wind
c     ==================================================================
c
c     o Prepare wind speed and stress fields
c
c     ==================================================================
c     SUBROUTINE exf_wind
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
c #include "GRID.h"

#include "exf_param.h"
#include "exf_fields.h"
#include "exf_constants.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif

c     == routine arguments ==

      integer mythid
      integer myiter
      _RL     mytime

c     == local variables ==

      integer bi,bj
      integer i,j

      _RL     ustmp
      _RL     ustar
      _RL     tmp1, tmp2, tmp3, tmp4, tmp5

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

c--   Use atmospheric state to compute surface fluxes.

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
       do bi = mybxlo(mythid),mybxhi(mythid)
        do j = 1,sny
         do i = 1,snx
c
c--   Initialise
          us(i,j,bi,bj) = 0. _d 0
          cw(i,j,bi,bj) = 0. _d 0
          sw(i,j,bi,bj) = 0. _d 0
          sh(i,j,bi,bj) = 0. _d 0
c
#ifdef ALLOW_ATM_WIND
c             Wind speed and direction.
          ustmp = uwind(i,j,bi,bj)*uwind(i,j,bi,bj) +
     &         vwind(i,j,bi,bj)*vwind(i,j,bi,bj)
          if ( ustmp .ne. 0. _d 0 ) then
             us(i,j,bi,bj) = sqrt(ustmp)
             cw(i,j,bi,bj) = uwind(i,j,bi,bj)/us(i,j,bi,bj)
             sw(i,j,bi,bj) = vwind(i,j,bi,bj)/us(i,j,bi,bj)
          else
             us(i,j,bi,bj) = 0. _d 0
             cw(i,j,bi,bj) = 0. _d 0
             sw(i,j,bi,bj) = 0. _d 0
          endif
#else  /* ifndef ALLOW_ATM_WIND */
c
c             The variables us, sh and rdn have to be computed from
c             given wind stresses inverting relationship for neutral
c             drag coeff. cdn.
c             The inversion is based on linear and quadratic form of
c             cdn(umps); ustar can be directly computed from stress;
C-   no need for wind-stress inversion since everything
C    (ustar, ... etc ...) is derived directly from wind-stress

          ustmp = 0.5*
     &         (ustress(i,  j,bi,bj)*ustress(i  ,j,bi,bj)
     &         +ustress(i+1,j,bi,bj)*ustress(i+1,j,bi,bj)
     &         +vstress(i,j,  bi,bj)*vstress(i,j  ,bi,bj)
     &         +vstress(i,j+1,bi,bj)*vstress(i,j+1,bi,bj))
          if ( ustmp .ne. 0. _d 0 ) then
             ustar = sqrt(ustmp/atmrho)
             cw(i,j,bi,bj) = ustress(i,j,bi,bj)/sqrt(ustmp)
             sw(i,j,bi,bj) = vstress(i,j,bi,bj)/sqrt(ustmp)
          else
             ustar            = 0. _d 0
             cw(i,j,bi,bj)    = 0. _d 0
             sw(i,j,bi,bj)    = 0. _d 0
          endif
 
          if ( ustar .eq. 0. _d 0 ) then
             us(i,j,bi,bj) = 0. _d 0
          else if ( ustar .lt. ustofu11 ) then
             tmp1 = -cquadrag_2/cquadrag_1/2.
             tmp2 = sqrt(tmp1*tmp1 + ustar*ustar/cquadrag_1)
             us(i,j,bi,bj) = sqrt(tmp1 + tmp2)
          else
             tmp3 = clindrag_2/clindrag_1/3.
             tmp4 = ustar*ustar/clindrag_1/2. - tmp3**3
             tmp5 = sqrt(ustar*ustar/clindrag_1*
     &            (ustar*ustar/clindrag_1/4. - tmp3**3))
             us(i,j,bi,bj)   = (tmp4 + tmp5)**(1./3.) +
     &            tmp3**2 * (tmp4 + tmp5)**(-1./3.) - tmp3
          endif
          uwind(i,j,bi,bj) = us(i,j,bi,bj)*cw(i,j,bi,bj)
          vwind(i,j,bi,bj) = us(i,j,bi,bj)*sw(i,j,bi,bj)
#endif /* ifndef ALLOW_ATM_WIND */

c--   set lower limit
          sh(i,j,bi,bj)    = max(us(i,j,bi,bj),umin)

c--   if wspeed available, overwrite sh,
c--   otherwise fill wspeed array for later use
          if ( wspeedfile .NE. ' ' ) then
             us(i,j,bi,bj) = wspeed(i,j,bi,bj)
             sh(i,j,bi,bj) = max(wspeed(i,j,bi,bj),umin)
          else
             wspeed(i,j,bi,bj) = sh(i,j,bi,bj)
          endif

         enddo
        enddo
       enddo
      enddo

      return
      end
