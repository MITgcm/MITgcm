C $Header: /u/gcmpack/MITgcm/pkg/fizhi/update_earth_exports.F,v 1.31 2012/03/21 21:08:12 jmc Exp $
C $Name:  $
#include "FIZHI_OPTIONS.h"

C--  File update_earth_exports.F:
C--   Contents
C--   o UPDATE_EARTH_EXPORTS
C--   o SIBALB
C--   o GETLGR
C--   o GETALB
C--   o GETEMISS
C--   o EMISSIVITY
C--   o GET_LANDFRAC

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE UPDATE_EARTH_EXPORTS (myTime, myIter, myThid)
C----------------------------------------------------------------------
C  Subroutine update_earth_exports - 'Wrapper' routine to update
C        the fields related to the earth surface that are needed
C        by fizhi.
C
C Call:  getlgr    (Set the leaf area index and surface greenness,
C                              based on veg type and month)
C        getalb    (Set the 4 albedos based on veg type, snow and time)
C        getemiss  (Set the surface emissivity based on the veg type
C                              and the snow depth)
C-----------------------------------------------------------------------
       IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_SIZE.h"
#include "fizhi_coms.h"
#include "chronos.h"
#include "gridalt_mapping.h"
#include "fizhi_land_coms.h"
#include "fizhi_earth_coms.h"
#include "fizhi_ocean_coms.h"
#include "EEPARAMS.h"

      INTEGER myIter, myThid
      _RL myTime

      LOGICAL alarm
      EXTERNAL alarm
      _RL lats(sNx,sNy), lons(sNx,sNy), cosz(sNx,sNy)
      _RL fraci(sNx,sNy), fracl(sNx,sNy)
      _RL ficetile(nchp)
      _RL radius
      _RL tmpij(sNx,sNy)
      _RL tmpchp(nchp)
      INTEGER i, j, n, bi, bj
      INTEGER im1, im2, jm1, jm2, idim1, idim2, jdim1, jdim2
      INTEGER sec, day, month
      INTEGER nmonf,ndayf,nsecf
      nsecf(n) = n/10000*3600 + mod(n,10000)/100* 60 + mod(n,100)
      nmonf(n) = mod(n,10000)/100
      ndayf(n) = mod(n,100)

      idim1 = 1-OLx
      idim2 = sNx+OLx
      jdim1 = 1-OLy
      jdim2 = sNy+OLy
      im1 = 1
      im2 = sNx
      jm1 = 1
      jm2 = sNy
      month = nmonf(nymd)
      day = ndayf(nymd)
      sec = nsecf(nhms)

      do bj = myByLo(myThid), myByHi(myThid)
      do bi = myBxLo(myThid), myBxHi(myThid)
       do j = jm1,jm2
       do i = im1,im2
        lons(i,j) = xC(i,j,bi,bj)
        lats(i,j) = yC(i,j,bi,bj)
       enddo
       enddo

       call get_landfrac(im2,jm2,nSx,nSy,bi,bj,maxtyp,surftype,tilefrac,
     &                                                            fracl)

       do j = jm1,jm2
       do i = im1,im2
        if(sice(i,j,bi,bj).gt.0.) then
           fraci(i,j) = 1.
        else
           fraci(i,j) = 0.
        endif
       enddo
       enddo

C***********************************************************************
C*              Get Leaf-Area-Index and Greenness Index                *
C***********************************************************************

       if( alarm('turb') .or. alarm('radsw') ) then
       call getlgr (sec,month,day,chlt,ityp,nchpland(bi,bj),
     &       nchp,nSx,nSy,bi,bj,alai,agrn )
       endif

C **********************************************************************
C                      Compute Surface Albedo
C **********************************************************************

       if( alarm('radsw') ) then
#ifdef FIZHI_USE_FIXED_DAY
        call astro(20040321,nhms,lats,lons,im2*jm2,cosz,radius)
#else
        call astro(nymd,nhms,lats,lons,im2*jm2,cosz,radius)
#endif
        call getalb(sec,month,day,cosz,snodep,fraci,fracl,im2,jm2,nchp,
     &    nchptot(bi,bj),nchpland(bi,bj),nSx,nSy,bi,bj,igrd,ityp,
     &    chfr,chlt,alai,agrn,
     &    albvisdr,albvisdf,albnirdr,albnirdf )
       endif

C **********************************************************************
C                      Compute Surface Emissivity
C **********************************************************************

       if( alarm('radlw') ) then
        call grd2msc(fraci,im2,jm2,igrd(1,bi,bj),ficetile,
     &               nchp,nchptot(bi,bj))
        call getemiss(fracl,im2,jm2,nchp,nchptot(bi,bj),nSx,nSy,bi,bj,
     &   igrd,ityp,chfr,snodep,ficetile,emiss)
       endif

C*********************************************************************
C            Ground Temperature Over Ocean is from SST array,
C               Over land is from tcanopy
C*********************************************************************

       do j = jm1,jm2
       do i = im1,im2
        tmpij(i,j) = 0.
       enddo
       enddo
       do i = 1,nchptot(bi,bj)
        tmpchp(i) = tcanopy(i,bi,bj)
       enddo
       call msc2grd(igrd(1,bi,bj),chfr(1,bi,bj),tmpchp,
     &                    nchp,nchptot(bi,bj),fracl,tmpij,im2,jm2)
       do j = jm1,jm2
       do i = im1,im2
        tgz(i,j,bi,bj) = tmpij(i,j)
        if(fracl(i,j).lt.0.3.and.sice(i,j,bi,bj).eq.0.0)
     &                                 tgz(i,j,bi,bj) = sst(i,j,bi,bj)
       enddo
       enddo

      enddo
      enddo

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE SIBALB ( AVISDR, ANIRDR, AVISDF, ANIRDF,
     &                    VLAI, VGRN, ZTH, SNW, ITYP, IRUN )

C*********************************************************************
C  The input list is as follows:
C     VLAI:     the leaf area index.
C     VGRN:     the greenness index.
C     ZTH:      The cosine of the solar zenith angle.
C     SNW:      Snow cover in meters water equivalent.
C     ITYP:     The surface type (grass, bare soil, etc.)
C     IRUN:     Number of tiles (same as used for SUBROUTINE TILE).
C
C   The output list is as follows:
C
C     AVISDR:   visible, direct albedo.
C     ANIRDR:   near infra-red, direct albedo.
C     AVISDF:   visible, diffuse albedo.
C     ANIRDF:   near infra-red, diffuse albedo.
C*******************************************************************

      IMPLICIT NONE

      INTEGER IRUN
      _RL AVISDR  (IRUN), ANIRDR (IRUN), AVISDF (IRUN), ANIRDF (IRUN)
      _RL VLAI(IRUN),VGRN (IRUN), SNW(IRUN)
      _RL ZTH(IRUN)
      INTEGER ITYP (IRUN)

      _RL ALVDRS,  ALIDRS
      _RL ALVDRDL, ALIDRDL
      _RL ALVDRDD, ALIDRDD
      _RL ALVDRI,  ALIDRI
      _RL minval
      EXTERNAL     minval

C Albedo of soil         for visible   direct solar radiation.
      PARAMETER (  ALVDRS  = 0.100 )
C Albedo of soil         for infra-red direct solar radiation.
      PARAMETER (  ALIDRS  = 0.200 )
C Albedo of light desert for visible   direct solar radiation.
      PARAMETER (  ALVDRDL = 0.300 )
C Albedo of light desert for infra-red direct solar radiation.
      PARAMETER (  ALIDRDL = 0.350 )
C Albedo of dark  desert for visible   direct solar radiation.
      PARAMETER (  ALVDRDD = 0.250 )
C Albedo of dark  desert for infra-red direct solar radiation.
      PARAMETER (  ALIDRDD = 0.300 )
C Albedo of ice          for visible   direct solar radiation.
      PARAMETER (  ALVDRI  = 0.800 )
C Albedo of ice          for infra-red direct solar radiation.
      PARAMETER (  ALIDRI  = 0.800 )

* ----------------------------------------------------------------------

      INTEGER NTYPS
      INTEGER NLAI
      _RL ZERO, ONE
      _RL EPSLN, BLAI, DLAI
      _RL ALATRM
      PARAMETER (NLAI = 14 )
      PARAMETER (EPSLN = 1.E-6)
      PARAMETER (BLAI = 0.5)
      PARAMETER (DLAI = 0.5)
      PARAMETER (ZERO=0., ONE=1.0)
      PARAMETER (ALATRM = BLAI + (NLAI - 1) * DLAI - EPSLN)
      PARAMETER (NTYPS=10)

C ITYP: Vegetation type as follows:
C                  1:  BROADLEAF EVERGREEN TREES
C                  2:  BROADLEAF DECIDUOUS TREES
C                  3:  NEEDLELEAF TREES
C                  4:  GROUND COVER
C                  5:  BROADLEAF SHRUBS
C                  6:  DWARF TREES (TUNDRA)
C                  7:  BARE SOIL
C                  8:  LIGHT DESERT
C                  9:  GLACIER
C                 10:  DARK DESERT
C

      INTEGER I, LAI
      _RL FAC,GAMMA,BETA,ALPHA,DX,DY,ALA,GRN (2),SNWALB(4,NTYPS)
      _RL COEFF

      _RL ALVDR (NLAI, 2, NTYPS)
      _RL BTVDR (NLAI, 2, NTYPS)
      _RL GMVDR (NLAI, 2, NTYPS)
      _RL ALIDR (NLAI, 2, NTYPS)
      _RL BTIDR (NLAI, 2, NTYPS)
      _RL GMIDR (NLAI, 2, NTYPS)

C  (Data statements for ALVDR described in full; data statements for
C   other constants follow same framework.)

C    BROADLEAF EVERGREEN (ITYP=4); GREEN=0.33; LAI: .5-7
        DATA (ALVDR (I, 1, 1), I = 1, 14)
     &    /0.0808, 0.0796, 0.0792, 0.0790, 10*0.0789/

C    BROADLEAF EVERGREEN (ITYP=4); GREEN=0.67; LAI: .5-7
        DATA (ALVDR (I, 2, 1), I = 1, 14)
     &    /0.0788, 0.0775, 0.0771, 0.0769, 10*0.0768/

C    BROADLEAF DECIDUOUS (ITYP=1); GREEN=0.33; LAI: .5-7
        DATA (ALVDR (I, 1, 2), I = 1, 14)
     &    /0.0803, 0.0790, 0.0785, 0.0784, 3*0.0783, 7*0.0782/

C    BROADLEAF DECIDUOUS (ITYP=1); GREEN=0.67; LAI: .5-7
        DATA (ALVDR (I, 2, 2), I = 1, 14)
     &    /0.0782, 0.0770, 0.0765, 0.0763, 10*0.0762/

C    NEEDLELEAF (ITYP=3); GREEN=0.33; LAI=.5-7
        DATA (ALVDR (I, 1, 3), I = 1, 14)
     &    /0.0758, 0.0746, 0.0742, 0.0740, 10*0.0739/

C    NEEDLELEAF (ITYP=3); GREEN=0.67; LAI=.5-7
        DATA (ALVDR (I, 2, 3), I = 1, 14)
     &    /0.0683, 0.0672, 0.0667, 2*0.0665, 9*0.0664/

C    GROUNDCOVER (ITYP=2); GREEN=0.33; LAI=.5-7
        DATA (ALVDR (I, 1, 4), I = 1, 14)
     &    /0.2436, 0.2470, 0.2486, 0.2494, 0.2498, 0.2500, 2*0.2501,
     &     6*0.2502
     &    /
C    GROUNDCOVER (ITYP=2); GREEN=0.67; LAI=.5-7
        DATA (ALVDR (I, 2, 4), I = 1, 14) /14*0.1637/

C    BROADLEAF SHRUBS (ITYP=5); GREEN=0.33,LAI=.5-7
        DATA (ALVDR (I, 1, 5), I = 1, 14)
     &    /0.0807, 0.0798, 0.0794, 0.0792, 0.0792, 9*0.0791/

C    BROADLEAF SHRUBS (ITYP=5); GREEN=0.67,LAI=.5-7
        DATA (ALVDR (I, 2, 5), I = 1, 14)
     &    /0.0787, 0.0777, 0.0772, 0.0771, 10*0.0770/

C    DWARF TREES, OR TUNDRA (ITYP=6); GREEN=0.33,LAI=.5-7
        DATA (ALVDR (I, 1, 6), I = 1, 14)
     &    /0.0802, 0.0791, 0.0787, 0.0786, 10*0.0785/

C    DWARF TREES, OR TUNDRA (ITYP=6); GREEN=0.67,LAI=.5-7
        DATA (ALVDR (I, 2, 6), I = 1, 14)
     &    /0.0781, 0.0771, 0.0767, 0.0765, 0.0765, 9*0.0764/

C    BARE SOIL
        DATA (ALVDR (I, 1, 7), I = 1, 14) /14*ALVDRS/
        DATA (ALVDR (I, 2, 7), I = 1, 14) /14*ALVDRS/

C    LIGHT DESERT (SAHARA, EG)
        DATA (ALVDR (I, 1, 8), I = 1, 14) /14*ALVDRDL/
        DATA (ALVDR (I, 2, 8), I = 1, 14) /14*ALVDRDL/

C    ICE
        DATA (ALVDR (I, 1, 9), I = 1, 14) /14*ALVDRI/
        DATA (ALVDR (I, 2, 9), I = 1, 14) /14*ALVDRI/

C    DARK DESERT (AUSTRALIA, EG)
        DATA (ALVDR (I, 1, 10), I = 1, 14) /14*ALVDRDD/
        DATA (ALVDR (I, 2, 10), I = 1, 14) /14*ALVDRDD/
C**** -------------------------------------------------
        DATA (BTVDR (I, 1, 1), I = 1, 14)
     &    /0.0153, 0.0372, 0.0506, 0.0587, 0.0630, 0.0652, 0.0663,
     &     0.0668, 0.0671, 0.0672, 4*0.0673
     &    /
        DATA (BTVDR (I, 2, 1), I = 1, 14)
     &    /0.0135, 0.0354, 0.0487, 0.0568, 0.0611, 0.0633, 0.0644,
     &     0.0650, 0.0652, 0.0654, 0.0654, 3*0.0655
     &    /
        DATA (BTVDR (I, 1, 2), I = 1, 14)
     &    /0.0148, 0.0357, 0.0462, 0.0524, 0.0554, 0.0569, 0.0576,
     &     0.0579, 0.0580, 0.0581, 0.0581, 3*0.0582
     &    /
        DATA (BTVDR (I, 2, 2), I = 1, 14)
     &    /0.0131, 0.0342, 0.0446, 0.0508, 0.0539, 0.0554, 0.0560,
     &     0.0564, 0.0565, 5*0.0566
     &    /
        DATA (BTVDR (I, 1, 3), I = 1, 14)
     &    /0.0108, 0.0334, 0.0478, 0.0571, 0.0624, 0.0652, 0.0666,
     &     0.0673, 0.0677, 0.0679, 4*0.0680
     &    /
        DATA (BTVDR (I, 2, 3), I = 1, 14)
     &    /0.0034, 0.0272, 0.0408, 0.0501, 0.0554, 0.0582, 0.0597,
     &     0.0604, 0.0608, 0.0610, 4*0.0611
     &    /
        DATA (BTVDR (I, 1, 4), I = 1, 14)
     &    /0.2050, 0.2524, 0.2799, 0.2947, 0.3022, 0.3059, 0.3076,
     &     0.3085, 0.3088, 0.3090, 4*0.3091
     &    /
        DATA (BTVDR (I, 2, 4), I = 1, 14)
     &    /0.1084, 0.1404, 0.1617, 0.1754, 0.1837, 0.1887, 0.1915,
     &     0.1931, 0.1940, 0.1946, 0.1948, 0.1950, 2*0.1951
     &    /
        DATA (BTVDR (I, 1, 5), I = 1, 14)
     &    /0.0203, 0.0406, 0.0548, 0.0632, 0.0679, 0.0703, 0.0716,
     &     0.0722, 0.0726, 0.0727, 0.0728, 0.0728, 0.0728, 0.0729
     &    /

        DATA (BTVDR (I, 2, 5), I = 1, 14)
     &    /0.0184, 0.0385, 0.0526, 0.0611,  0.0658, 0.0683, 0.0696,
     &     0.0702, 0.0705, 0.0707, 4*0.0708
     &    /

        DATA (BTVDR (I, 1, 6), I = 1, 14)
     &    /0.0199, 0.0388, 0.0494,  0.0554, 0.0584, 0.0599, 0.0606,
     &     0.0609, 0.0611, 5*0.0612
     &    /

        DATA (BTVDR (I, 2, 6), I = 1, 14)
     &    /0.0181, 0.0371, 0.0476, 0.0537,  0.0568, 0.0583, 0.0590,
     &     0.0593, 0.0595, 0.0595, 4*0.0596
     &    /

        DATA (BTVDR (I, 1, 7), I = 1, 14) /14*0./
        DATA (BTVDR (I, 2, 7), I = 1, 14) /14*0./

        DATA (BTVDR (I, 1, 8), I = 1, 14) /14*0./
        DATA (BTVDR (I, 2, 8), I = 1, 14) /14*0./

        DATA (BTVDR (I, 1, 9), I = 1, 14) /14*0./
        DATA (BTVDR (I, 2, 9), I = 1, 14) /14*0./

        DATA (BTVDR (I, 1, 10), I = 1, 14) /14*0./
        DATA (BTVDR (I, 2, 10), I = 1, 14) /14*0./

C**** -----------------------------------------------------------
        DATA (GMVDR (I, 1, 1), I = 1, 14)
     &    /0.0814, 0.1361, 0.2078, 0.2650, 0.2986, 0.3169,  0.3265,
     &     0.3313, 0.3337, 0.3348, 0.3354, 0.3357, 2*0.3358
     &    /
        DATA (GMVDR (I, 2, 1), I = 1, 14)
     &    /0.0760, 0.1336, 0.2034, 0.2622, 0.2969, 0.3159,  0.3259,
     &     0.3309, 0.3333, 0.3346, 0.3352, 0.3354, 2*0.3356
     &    /
        DATA (GMVDR (I, 1, 2), I = 1, 14)
     &    /0.0834, 0.1252, 0.1558, 0.1927, 0.2131,   0.2237, 0.2290,
     &     0.2315, 0.2327, 0.2332, 0.2335, 2*0.2336, 0.2337
     &    /
        DATA (GMVDR (I, 2, 2), I = 1, 14)
     &    /0.0789, 0.1235, 0.1531, 0.1912, 0.2122, 0.2232,  0.2286,
     &     0.2312, 0.2324, 0.2330, 0.2333, 0.2334, 2*0.2335
     &    /
        DATA (GMVDR (I, 1, 3), I = 1, 14)
     &    /0.0647, 0.1342, 0.2215, 0.2968, 0.3432, 0.3696, 0.3838,
     &     0.3912, 0.3950, 0.3968, 0.3978, 0.3982, 0.3984, 0.3985
     &    /
        DATA (GMVDR (I, 2, 3), I = 1, 14)
     &    /0.0258, 0.1227, 0.1999, 0.2825, 0.3339, 0.3634, 0.3794,
     &     0.3877, 0.3919, 0.3940, 0.3950, 0.3956, 0.3958, 0.3959
     &    /
        DATA (GMVDR (I, 1, 4), I = 1, 14)
     &    /0.3371, 0.5762, 0.7159, 0.7927, 0.8324, 0.8526,  0.8624,
     &     0.8671, 0.8693, 0.8704, 0.8709, 0.8710, 2*0.8712
     &    /
        DATA (GMVDR (I, 2, 4), I = 1, 14)
     &    /0.2634, 0.4375, 0.5532, 0.6291, 0.6763, 0.7048, 0.7213,
     &     0.7310, 0.7363, 0.7395, 0.7411, 0.7420, 0.7426, 0.7428
     &    /
        DATA (GMVDR (I, 1, 5), I = 1, 14)
     &    /0.0971, 0.1544, 0.2511, 0.3157, 0.3548, 0.3768, 0.3886,
     &     0.3948, 0.3978, 0.3994, 0.4001, 0.4006, 0.4007, 0.4008
     &    /

        DATA (GMVDR (I, 2, 5), I = 1, 14)
     &    /0.0924, 0.1470, 0.2458, 0.3123, 0.3527, 0.3756, 0.3877,
     &     0.3942, 0.3974, 0.3990, 0.3998, 0.4002, 0.4004, 0.4005
     &    /

        DATA (GMVDR (I, 1, 6), I = 1, 14)
     &    /0.0970, 0.1355, 0.1841, 0.2230, 0.2447,  0.2561, 0.2617,
     &     0.2645, 0.2658, 0.2664, 0.2667, 3*0.2669
     &    /

        DATA (GMVDR (I, 2, 6), I = 1, 14)
     &    /0.0934, 0.1337, 0.1812, 0.2213, 0.2437, 0.2554, 0.2613,
     &     0.2642, 0.2656, 0.2662, 0.2665, 0.2667, 0.2667, 0.2668
     &    /

        DATA (GMVDR (I, 1, 7), I = 1, 14) /14*1./
        DATA (GMVDR (I, 2, 7), I = 1, 14) /14*1./

        DATA (GMVDR (I, 1, 8), I = 1, 14) /14*1./
        DATA (GMVDR (I, 2, 8), I = 1, 14) /14*1./

        DATA (GMVDR (I, 1, 9), I = 1, 14) /14*1./
        DATA (GMVDR (I, 2, 9), I = 1, 14) /14*1./

        DATA (GMVDR (I, 1, 10), I = 1, 14) /14*1./
        DATA (GMVDR (I, 2, 10), I = 1, 14) /14*1./

C****  -----------------------------------------------------------

        DATA (ALIDR (I, 1, 1), I = 1, 14)
     &    /0.2867,  0.2840, 0.2828, 0.2822, 0.2819, 0.2818, 2*0.2817,
     &     6*0.2816
     &    /
        DATA (ALIDR (I, 2, 1), I = 1, 14)
     &    /0.3564, 0.3573, 0.3577, 0.3580, 2*0.3581, 8*0.3582/
        DATA (ALIDR (I, 1, 2), I = 1, 14)
     &    /0.2848, 0.2819, 0.2804, 0.2798, 0.2795, 2*0.2793, 7*0.2792/
        DATA (ALIDR (I, 2, 2), I = 1, 14)
     &    /0.3544, 0.3550, 0.3553, 2*0.3555, 9*0.3556/
        DATA (ALIDR (I, 1, 3), I = 1, 14)
     &    /0.2350, 0.2311, 0.2293, 0.2285, 0.2281, 0.2280, 8*0.2279/
        DATA (ALIDR (I, 2, 3), I = 1, 14)
     &    /0.2474, 0.2436, 0.2418, 0.2410, 0.2406, 0.2405, 3*0.2404,
     &     5*0.2403
     &    /
        DATA (ALIDR (I, 1, 4), I = 1, 14)
     &    /0.5816, 0.6157, 0.6391, 0.6556, 0.6673, 0.6758, 0.6820,
     &     0.6866, 0.6899, 0.6924, 0.6943, 0.6956, 0.6966, 0.6974
     &    /
        DATA (ALIDR (I, 2, 4), I = 1, 14)
     &    /0.5489, 0.5770, 0.5955, 0.6079, 0.6163, 0.6221, 0.6261,
     &     0.6288, 0.6308, 0.6321, 0.6330, 0.6337, 0.6341, 0.6344
     &    /
        DATA (ALIDR (I, 1, 5), I = 1, 14)
     &    /0.2845, 0.2837, 0.2832, 0.2831, 0.2830, 9*0.2829/
        DATA (ALIDR (I, 2, 5), I = 1, 14)
     &    /0.3532, 0.3562, 0.3578,  0.3586, 0.3590, 0.3592, 0.3594,
     &     0.3594, 0.3594, 5*0.3595
     &    /
        DATA (ALIDR (I, 1, 6), I = 1, 14)
     &    /0.2825, 0.2812, 0.2806, 0.2803, 0.2802, 9*0.2801/
        DATA (ALIDR (I, 2, 6), I = 1, 14)
     &    /0.3512, 0.3538,  0.3552, 0.3559, 0.3562, 0.3564, 0.3565,
     &     0.3565, 6*0.3566
     &    /

        DATA (ALIDR (I, 1, 7), I = 1, 14) /14*ALIDRS/
        DATA (ALIDR (I, 2, 7), I = 1, 14) /14*ALIDRS/

        DATA (ALIDR (I, 1, 8), I = 1, 14) /14*ALIDRDL/
        DATA (ALIDR (I, 2, 8), I = 1, 14) /14*ALIDRDL/

        DATA (ALIDR (I, 1, 9), I = 1, 14) /14*ALIDRI/
        DATA (ALIDR (I, 2, 9), I = 1, 14) /14*ALIDRI/

        DATA (ALIDR (I, 1, 10), I = 1, 14) /14*ALIDRDD/
        DATA (ALIDR (I, 2, 10), I = 1, 14) /14*ALIDRDD/

C**** -----------------------------------------------------------
        DATA (BTIDR (I, 1, 1), I = 1, 14)
     &    /0.1291, 0.1707, 0.1969, 0.2125, 0.2216,   0.2267, 0.2295,
     &     0.2311, 0.2319, 0.2323, 0.2326, 2*0.2327, 0.2328
     &    /
        DATA (BTIDR (I, 2, 1), I = 1, 14)
     &    /0.1939, 0.2357, 0.2598, 0.2735, 0.2810,  0.2851, 0.2874,
     &     0.2885, 0.2892, 0.2895, 0.2897, 3*0.2898
     &    /
        DATA (BTIDR (I, 1, 2), I = 1, 14)
     &    /0.1217, 0.1522, 0.1713, 0.1820,   0.1879,  0.1910, 0.1926,
     &     0.1935, 0.1939, 0.1942, 2*0.1943, 2*0.1944
     &    /
        DATA (BTIDR (I, 2, 2), I = 1, 14)
     &    /0.1781, 0.2067, 0.2221, 0.2301,   0.2342,  0.2363, 0.2374,
     &     0.2379, 0.2382, 0.2383, 2*0.2384, 2*0.2385
     &    /
        DATA (BTIDR (I, 1, 3), I = 1, 14)
     &    /0.0846, 0.1299, 0.1614, 0.1814, 0.1935,   0.2004, 0.2043,
     &     0.2064, 0.2076, 0.2082, 0.2085, 2*0.2087, 0.2088
     &    /
        DATA (BTIDR (I, 2, 3), I = 1, 14)
     &    /0.0950, 0.1410, 0.1722, 0.1921, 0.2042, 0.2111,  0.2151,
     &     0.2172, 0.2184, 0.2191, 0.2194, 0.2196, 2*0.2197
     &    /
        DATA (BTIDR (I, 1, 4), I = 1, 14)
     &    /0.5256, 0.7444, 0.9908, 1.2700, 1.5680, 1.8505, 2.0767,
     &     2.2211, 2.2808, 2.2774, 2.2362, 2.1779, 2.1160, 2.0564
     &    /
        DATA (BTIDR (I, 2, 4), I = 1, 14)
     &    /0.4843, 0.6714, 0.8577, 1.0335, 1.1812, 1.2858, 1.3458,
     &     1.3688, 1.3685, 1.3546, 1.3360, 1.3168, 1.2989, 1.2838
     &    /
        DATA (BTIDR (I, 1, 5), I = 1, 14)
     &    /0.1498, 0.1930, 0.2201, 0.2364, 0.2460, 0.2514, 0.2544,
     &     0.2560, 0.2569, 0.2574, 0.2577, 0.2578, 0.2579, 0.2579
     &    /

        DATA (BTIDR (I, 2, 5), I = 1, 14)
     &    /0.2184, 0.2656, 0.2927, 0.3078, 0.3159,  0.3202, 0.3224,
     &     0.3235, 0.3241, 0.3244, 0.3245, 3*0.3246
     &    /

        DATA (BTIDR (I, 1, 6), I = 1, 14)
     &    /0.1369, 0.1681, 0.1860, 0.1958, 0.2010,  0.2038, 0.2053,
     &     0.2060, 0.2064, 0.2066, 0.2067, 3*0.2068
     &    /

        DATA (BTIDR (I, 2, 6), I = 1, 14)
     &    /0.1969, 0.2268, 0.2416,  0.2488, 0.2521, 0.2537, 0.2544,
     &     0.2547, 0.2548, 5*0.2549
     &    /


        DATA (BTIDR (I, 1, 7), I = 1, 14) /14*0./
        DATA (BTIDR (I, 2, 7), I = 1, 14) /14*0./

        DATA (BTIDR (I, 1, 8), I = 1, 14) /14*0./
        DATA (BTIDR (I, 2, 8), I = 1, 14) /14*0./

        DATA (BTIDR (I, 1, 9), I = 1, 14) /14*0./
        DATA (BTIDR (I, 2, 9), I = 1, 14) /14*0./

        DATA (BTIDR (I, 1, 10), I = 1, 14) /14*0./
        DATA (BTIDR (I, 2, 10), I = 1, 14) /14*0./

C**** --------------------------------------------------------------
        DATA (GMIDR (I, 1, 1), I = 1, 14)
     &    /0.1582, 0.2581, 0.3227, 0.3635, 0.3882, 0.4026, 0.4108,
     &     0.4154, 0.4179, 0.4193, 0.4200, 0.4204, 0.4206, 0.4207
     &    /
        DATA (GMIDR (I, 2, 1), I = 1, 14)
     &    /0.1934, 0.3141, 0.3818, 0.4200, 0.4415, 0.4533, 0.4598,
     &     0.4633, 0.4651, 0.4662, 0.4667, 0.4671, 2*0.4672
     &    /
        DATA (GMIDR (I, 1, 2), I = 1, 14)
     &    /0.1347, 0.1871, 0.2277, 0.2515, 0.2651, 0.2727, 0.2768,
     &     0.2790, 0.2801, 0.2808, 0.2811, 0.2812, 0.2813, 0.2814
     &    /
        DATA (GMIDR (I, 2, 2), I = 1, 14)
     &    /0.1440, 0.2217, 0.2629, 0.2839, 0.2947, 0.3003, 0.3031,
     &     0.3046, 0.3054, 0.3058, 0.3060, 2*0.3061, 0.3062
     &    /
        DATA (GMIDR (I, 1, 3), I = 1, 14)
     &    /0.1372, 0.2368, 0.3235, 0.3839, 0.4229, 0.4465, 0.4602,
     &     0.4679, 0.4722, 0.4745, 0.4758, 0.4764, 0.4768, 0.4770
     &    /
        DATA (GMIDR (I, 2, 3), I = 1, 14)
     &    /0.1435, 0.2524, 0.3370, 0.3955, 0.4332, 0.4563, 0.4697,
     &     0.4773, 0.4815, 0.4839, 0.4851, 0.4858, 0.4861, 0.4863
     &    /
        DATA (GMIDR (I, 1, 4), I = 1, 14)
     &    /0.4298, 0.9651, 1.6189, 2.4084, 3.2992, 4.1928, 4.9611,
     &     5.5095, 5.8085, 5.9069, 5.8726, 5.7674, 5.6346, 5.4944
     &    /
        DATA (GMIDR (I, 2, 4), I = 1, 14)
     &    /0.4167, 0.8974, 1.4160, 1.9414, 2.4147, 2.7803, 3.0202,
     &     3.1468, 3.1954, 3.1932, 3.1676, 3.1328, 3.0958, 3.0625
     &    /
        DATA (GMIDR (I, 1, 5), I = 1, 14)
     &    /0.1959, 0.3203, 0.3985, 0.4472, 0.4766, 0.4937, 0.5034,
     &     0.5088, 0.5117, 0.5134, 0.5143, 0.5147, 0.5150, 0.5152
     &    /

        DATA (GMIDR (I, 2, 5), I = 1, 14)
     &    /0.2328, 0.3859, 0.4734, 0.5227, 0.5498, 0.5644, 0.5720,
     &     0.5761, 0.5781, 0.5792, 0.5797, 0.5800, 0.5802, 0.5802
     &    /

        DATA (GMIDR (I, 1, 6), I = 1, 14)
     &    /0.1447, 0.2244, 0.2698, 0.2953, 0.3094, 0.3170, 0.3211,
     &     0.3233, 0.3244, 0.3250, 0.3253, 0.3255, 0.3256, 0.3256
     &    /

        DATA (GMIDR (I, 2, 6), I = 1, 14)
     &    /0.1643, 0.2624, 0.3110, 0.3347, 0.3461, 0.3517, 0.3543,
     &     0.3556, 0.3562, 0.3564, 0.3565, 0.3566, 0.3566, 0.3566
     &    /

        DATA (GMIDR (I, 1, 7), I = 1, 14) /14*1./
        DATA (GMIDR (I, 2, 7), I = 1, 14) /14*1./

        DATA (GMIDR (I, 1, 8), I = 1, 14) /14*1./
        DATA (GMIDR (I, 2, 8), I = 1, 14) /14*1./

        DATA (GMIDR (I, 1, 9), I = 1, 14) /14*1./
        DATA (GMIDR (I, 2, 9), I = 1, 14) /14*1./

        DATA (GMIDR (I, 1, 10), I = 1, 14) /14*1./
        DATA (GMIDR (I, 2, 10), I = 1, 14) /14*1./

C**** -----------------------------------------------------------

      DATA GRN /0.33, 0.67/

#include "snwmid.h"
      DATA SNWALB /.65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .65, .38, .65, .38,
     &             .80, .60, .80, .60,
     &             .65, .38, .65, .38
     &            /

#ifdef CRAY
#ifdef f77
cfpp$ expand (coeff)
#endif
#endif

      DO 100 I=1,IRUN
          ALA = MIN (MAX (ZERO, VLAI(I)), ALATRM)
          LAI = 1 + MAX(0, INT((ALA-BLAI)/DLAI) )
          DX = (ALA - (BLAI+(LAI-1)*DLAI)) * (ONE/DLAI)
          DY = (VGRN(I)- GRN(1)) * (ONE/(GRN(2) - GRN(1)))

          ALPHA = COEFF (ALVDR (1, 1, ITYP (I)), NLAI, LAI ,DX, DY)
          BETA  = COEFF (BTVDR (1, 1, ITYP (I)), NLAI, LAI ,DX, DY)
          GAMMA = COEFF (GMVDR (1, 1, ITYP (I)), NLAI, LAI ,DX, DY)

          AVISDR(I) = ALPHA - ZTH(I)*BETA / (GAMMA+ZTH(I))
          AVISDF(I) = ALPHA-BETA
     &          + 2.*BETA*GAMMA*(1.-GAMMA*LOG((1.+GAMMA)/GAMMA))

          ALPHA = COEFF (ALIDR (1, 1, ITYP (I)), NLAI, LAI ,DX, DY)
          BETA  = COEFF (BTIDR (1, 1, ITYP (I)), NLAI, LAI ,DX, DY)
          GAMMA = COEFF (GMIDR (1, 1, ITYP (I)), NLAI, LAI ,DX, DY)

          ANIRDR(I) = ALPHA - ZTH(I)*BETA / (GAMMA+ZTH(I))
          ANIRDF(I) = ALPHA-BETA
     &          + 2.*BETA*GAMMA*(1.-GAMMA*LOG((1.+GAMMA)/GAMMA))

          IF (SNW (I) .GT. ZERO) THEN
           FAC = SNW(I) / (SNW(I) + SNWMID(ITYP(I)))

           AVISDR(I) = AVISDR(I) + (SNWALB(1,ITYP(I)) - AVISDR(I)) * FAC
           ANIRDR(I) = ANIRDR(I) + (SNWALB(2,ITYP(I)) - ANIRDR(I)) * FAC
           AVISDF(I) = AVISDF(I) + (SNWALB(3,ITYP(I)) - AVISDF(I)) * FAC
           ANIRDF(I) = ANIRDF(I) + (SNWALB(4,ITYP(I)) - ANIRDF(I)) * FAC
          ENDIF

 100  CONTINUE

      RETURN
      END
      FUNCTION COEFF(TABLE, NTABL, LAI ,DX, DY)

      INTEGER NTABL, LAI
      _RL coeff
      _RL TABLE (NTABL, 2), DX, DY

      COEFF = (TABLE(LAI,  1)
     &      + (TABLE(LAI  ,2) - TABLE(LAI  ,1)) * DY ) * (1.0-DX)
     &      + (TABLE(LAI+1,1)
     &      + (TABLE(LAI+1,2) - TABLE(LAI+1,1)) * DY ) * DX

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GETLGR(sec,IMON,IDAY,ALAT,ITYP,NCHPS,nchpdim,
     &    nSx,nSy,bi,bj,ALAI,AGRN)
C*********************************************************************
      IMPLICIT NONE

      INTEGER ntyps
      _RL one,daylen
      PARAMETER (NTYPS=10)
      PARAMETER (one = 1.)
      PARAMETER (daylen = 86400.)

      INTEGER sec, imon, iday, nchps, nchpdim, nSx, nSy, bi, bj
      _RL ALAI(nchpdim,nSx,nSy), AGRN(nchpdim,nSx,nSy)
      _RL ALAT(nchpdim,nSx,nSy)
      INTEGER ITYP(nchpdim,nSx,nSy)

      INTEGER i,midmon,midm,midp,id,k1,k2,kk1,kk2
      _RL fac

      INTEGER     DAYS(12)
      DATA        DAYS/31,28,31,30,31,30,31,31,30,31,30,31/

      _RL VGLA(12,NTYPS), VGGR(12,NTYPS)

      DATA VGLA  /
     1    5.117, 5.117, 5.117, 5.117, 5.117, 5.117, 5.117, 5.117,
     1    5.117, 5.117, 5.117, 5.117,
     2    0.520, 0.520, 0.867, 2.107, 4.507, 6.773, 7.173, 6.507,
     2    5.040, 2.173, 0.867, 0.520,
     3    8.760, 9.160, 9.827,10.093,10.360,10.760,10.493,10.227,
     3   10.093, 9.827, 9.160, 8.760,
     4    0.782, 0.893, 1.004, 1.116, 1.782, 3.671, 4.782, 4.227,
     4    2.004, 1.227, 1.004, 0.893,
     5    3.760, 3.760, 2.760, 1.760, 1.760, 1.760, 1.760, 5.760,
     5   10.760, 7.760, 4.760, 3.760,
     6    0.739, 0.739, 0.739, 0.739, 0.739, 1.072, 5.072, 5.739,
     6    4.405, 0.739, 0.739, 0.739,
     7    0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     7    0.001, 0.001, 0.001, 0.001,
     8    0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     8    0.001, 0.001, 0.001, 0.001,
     9    0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     9    0.001, 0.001, 0.001, 0.001,
     1    0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     1    0.001, 0.001, 0.001, 0.001
     &  /

      DATA VGGR
     1  /0.905, 0.905, 0.905, 0.905, 0.905, 0.905, 0.905, 0.905,
     1   0.905, 0.905, 0.905, 0.905,
     2   0.026, 0.026, 0.415, 0.759, 0.888, 0.925, 0.836, 0.697,
     2   0.331, 0.166, 0.015, 0.026,
     3   0.913, 0.917, 0.923, 0.925, 0.927, 0.905, 0.902, 0.913,
     3   0.898, 0.855, 0.873, 0.913,
     4   0.568, 0.622, 0.664, 0.697, 0.810, 0.908, 0.813, 0.394,
     4   0.443, 0.543, 0.553, 0.498,
     5   0.798, 0.532, 0.362, 0.568, 0.568, 0.568, 0.568, 0.868,
     5   0.651, 0.515, 0.630, 0.798,
     6   0.451, 0.451, 0.451, 0.451, 0.451, 0.622, 0.920, 0.697,
     6   0.076, 0.451, 0.451, 0.451,
     7   0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     7   0.001, 0.001, 0.001, 0.001,
     8   0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     8   0.001, 0.001, 0.001, 0.001,
     9   0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     9   0.001, 0.001, 0.001, 0.001,
     1   0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001,
     1   0.001, 0.001, 0.001, 0.001
     &  /

      MIDMON = DAYS(IMON)/2 + 1

      IF (IDAY .LT. MIDMON) THEN
        K2 = IMON
        K1 = MOD(IMON+10,12) + 1
      ELSE
        K1 = IMON
        K2 = MOD(IMON,12) + 1
      ENDIF

      IF (IDAY .LT. MIDMON) THEN
        MIDM = DAYS(K1)/2 + 1
        MIDP = DAYS(K1) + MIDMON
        ID = IDAY + DAYS(K1)
      ELSE
        MIDM = MIDMON
        MIDP = DAYS(K2)/2 + 1 + DAYS(K1)
        ID = IDAY
      ENDIF

      FAC = (float(ID  -MIDM)*DAYLEN + SEC) /
     &      (float(MIDP-MIDM)*DAYLEN            )

      DO 220 I=1,NCHPS

      IF(ALAT(I,bi,bj).GT.0.) THEN
       KK1 = K1
       KK2 = K2
      ELSE
       KK1 = MOD(K1+5,12) + 1
       KK2 = MOD(K2+5,12) + 1
      ENDIF

      ALAI(I,bi,bj) = VGLA(KK2,ITYP(I,bi,bj))*FAC+
     &                                 VGLA(KK1,ITYP(I,bi,bj))*(ONE-FAC)
      AGRN(I,bi,bj) = VGGR(KK2,ITYP(I,bi,bj))*FAC+
     &                                 VGGR(KK1,ITYP(I,bi,bj))*(ONE-FAC)

  220 CONTINUE

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GETALB(sec,month,day,cosz,snodep,fraci,fracg,im,jm,
     &        nchp,nchptot,nchpland,nSx,nSy,bi,bj,igrd,ityp,chfr,chlt,
     &        alai,agrn,albvr,albvf,albnr,albnf)
C***********************************************************************
C  PURPOSE
C     To act as an interface to routine sibalb, which calculates
C     the four albedos for use by the shortwave radiation routine
C
C INPUT:
C sec      - number of seconds into the day of current time
C month    - month of the year of current time
C day      - day of the month of current time
C cosz     - local cosine of the zenith angle [im,jm]
C snodep   - snow cover in meters [nchp,nSx,nSy]
C fraci    - real array in grid space of total sea ice fraction [im,jm]
C fracg    - real array in grid space of total land fraction [im,jm]
C im       - model grid longitude dimension
C jm       - model grid latitude dimension (number of lat. points)
C nchp     - integer actual number of tiles in tile space
C nchpland - integer number of land tiles
C nSx      - number of processors in x-direction
C nSy      - number of processors in y-direction
C bi       - processors index in x-direction
C bj       - processors index in y-direction
C igrd     - integer array in tile space of grid point number for each
C            tile [nchp,nSx,nSy]
C ityp     - integer array in tile space of land surface type for each
C            tile [nchp,nSx,nSy]
C chfr     - real array in tile space of land surface type fraction for
C            each tile [nchp,nSx,nSy]
C chlt     - real array in tile space of latitude value for each tile
C            [nchp,nSx,nSy]
C
C OUTPUT:
C albvr    - real array [im,jm] of visible direct beam albedo
C albvf    - real array [im,jm] of visible diffuse beam albedo
C albnr    - real array [im,jm] of near-ir direct beam albedo
C albnf    - real array [im,jm] of near-ir diffuse beam albedo
C
C***********************************************************************
      IMPLICIT NONE

      INTEGER sec,month,day,im,jm,nchp,nchptot,nchpland,nSx,nSy,bi,bj
      _RL cosz(im,jm),fraci(im,jm),fracg(im,jm)
      _RL snodep(nchp,nSx,nSy),chfr(nchp,nSx,nSy),chlt(nchp,nSx,nSy)
      INTEGER igrd(nchp,nSx,nSy),ityp(nchp,nSx,nSy)
      _RL alai(nchp,nSx,nSy),agrn(nchp,nSx,nSy)
      _RL albvr(im,jm,nSx,nSy),albvf(im,jm,nSx,nSy)
      _RL albnr(im,jm,nSx,nSy),albnf(im,jm,nSx,nSy)

C-    local variables:
      _RL one,a0,a1,a2,a3,ocnalb,albsi
      PARAMETER (one = 1.)
      PARAMETER (A0= 0.40670980)
      PARAMETER (A1=-1.2523634 )
      PARAMETER (A2= 1.4224051 )
      PARAMETER (A3=-0.55573341)
      PARAMETER (OCNALB=0.08)
      PARAMETER (ALBSI=0.7)

      _RL alboc(im,jm)
      _RL avisdr(nchp),anirdr(nchp),avisdf(nchp)
      _RL anirdf(nchp)
      _RL zenith(nchp)
      _RL tmpij(im,jm)
      INTEGER i,j

      DO I=1,IM
      DO J=1,JM
       ALBOC(I,J) = A0 + (A1 + (A2 +  A3*cosz(I,J))*cosz(I,J))*cosz(I,J)
       ALBVR(I,J,bi,bj) = ALBSI*FRACI(I,J) + ALBOC(I,J)*(ONE-FRACI(I,J))
       ALBNR(I,J,bi,bj) = ALBVR(I,J,bi,bj)
       ALBVF(I,J,bi,bj) = ALBSI * FRACI(I,J) + OCNALB * (ONE-FRACI(I,J))
       ALBNF(I,J,bi,bj) = ALBVF(I,J,bi,bj)
      ENDDO
      ENDDO

C and now some conversions from grid space to tile space before sibalb

      call grd2msc(cosz,im,jm,igrd(1,bi,bj),zenith,nchp,nchpland)

C and now call sibalb

      call sibalb(avisdr,anirdr,avisdf,anirdf,alai(1,bi,bj),
     &  agrn(1,bi,bj),zenith,snodep(1,bi,bj),ityp(1,bi,bj),nchpland)

C finally some transformations back to grid space for albedos

      DO I=1,IM
      DO J=1,JM
       tmpij(i,j) = albvr(i,j,bi,bj)
      ENDDO
      ENDDO
      call msc2grd(igrd(1,bi,bj),chfr(1,bi,bj),avisdr,nchp,nchpland,
     &                                     fracg,tmpij,im,jm)

      DO I=1,IM
      DO J=1,JM
       albvr(i,j,bi,bj) = tmpij(i,j)
      ENDDO
      ENDDO
      DO I=1,IM
      DO J=1,JM
       tmpij(i,j) = albvf(i,j,bi,bj)
      ENDDO
      ENDDO
      call msc2grd(igrd(1,bi,bj),chfr(1,bi,bj),avisdf,nchp,nchpland,
     &                                     fracg,tmpij,im,jm)
      DO I=1,IM
      DO J=1,JM
       albvf(i,j,bi,bj) = tmpij(i,j)
      ENDDO
      ENDDO
      DO I=1,IM
      DO J=1,JM
       tmpij(i,j) = albnr(i,j,bi,bj)
      ENDDO
      ENDDO
      call msc2grd(igrd(1,bi,bj),chfr(1,bi,bj),anirdr,nchp,nchpland,
     &                                     fracg,tmpij,im,jm)
      DO I=1,IM
      DO J=1,JM
       albnr(i,j,bi,bj) = tmpij(i,j)
      ENDDO
      ENDDO
      DO I=1,IM
      DO J=1,JM
       tmpij(i,j) = albnf(i,j,bi,bj)
      ENDDO
      ENDDO
      call msc2grd(igrd(1,bi,bj),chfr(1,bi,bj),anirdf,nchp,nchpland,
     &                                     fracg,tmpij,im,jm)
      DO I=1,IM
      DO J=1,JM
       albnf(i,j,bi,bj) = tmpij(i,j)
      ENDDO
      ENDDO

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GETEMISS(fracg,im,jm,nchp,nchptot,nSx,nSy,bi,bj,
     &   igrd,ityp,chfr,snowdep,fraci,emiss)
C***********************************************************************
C  PURPOSE
C     To act as an interface to routine to emissivity, which calculates
C     ten bands of surface emissivities for use by the longwave radiation
C
C INPUT:
C fracg    - real array in grid space of total land fraction [im,jm]
C im       - model grid longitude dimension
C jm       - model grid latitude dimension (number of lat. points)
C nchp     - integer actual number of tiles in tile space
C nSx      - number of processors in x-direction
C nSy      - number of processors in y-direction
C bi       - processors index in x-direction
C bj       - processors index in y-direction
C igrd     - integer array in tile space of grid point number for each
C            tile [nchp]
C ityp     - integer array in tile space of land surface type for each
C            tile [nchp]
C chfr     - real array in tile space of land surface type fraction for
C            each tile [nchp]
C snowdep  - real array in tile space of snow depth (liquid water equiv)
C            in mm [nchp]
C fraci    - real array in tile space of sea ice fraction [nchp]
C
C OUTPUT:
C emiss    - real array [im,jm,10,nSx,nSy] - surface emissivity (frac)
C
C***********************************************************************
      IMPLICIT NONE
      INTEGER im,jm,nchp,nchptot,nSx,nSy,bi,bj
      _RL fracg(im,jm)
      _RL chfr(nchp,nSx,nSy)
      INTEGER igrd(nchp,nSx,nSy), ityp(nchp,nSx,nSy)
      _RL snowdep(nchp,nSx,nSy)
      _RL fraci(nchp)
      _RL emiss(im,jm,10,nSx,nSy)

      _RL emisstile(nchp,10)
      _RL tmpij(im,jm)
      INTEGER i,j,k,n

      do i = 1,10
      do n = 1,nchptot
         emisstile(n,i) = 1.
      enddo
      enddo

C call emissivity to get values in tile space
C -------------------------------------------
      call emissivity(snowdep(1,bi,bj),fraci,nchp,nchptot,ityp(1,bi,bj),
     &                                                    emisstile)

C transform back to grid space for emissivities
C ---------------------------------------------
      do k = 1,10
      do j = 1,jm
      do i = 1,im
       tmpij(i,j) = 0.0
      enddo
      enddo
      call msc2grd(igrd(1,bi,bj),chfr(1,bi,bj),emisstile(1,k),nchp,
     &  nchptot,fracg,tmpij,im,jm)
      do j = 1,jm
      do i = 1,im
       emiss(i,j,k,bi,bj) = tmpij(i,j)
      enddo
      enddo
      enddo

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE EMISSIVITY (snowdepth,fraci,nchp,numpts,ityp,newemis)
      IMPLICIT NONE
      INTEGER nchp,numpts
      INTEGER   ityp(nchp)
      _RL snowdepth(nchp)
      _RL fraci(nchp)
      _RL newemis(nchp,10)

      _RL emis(12,11)
      _RL fac
      INTEGER i,j

C-----------------------------------------------------------------------
C  NOTE: Emissivities were obtained for the following surface types:
C  ( 1) evergreen needleleaf = conifer
C  ( 2) evergreen broadleaf = conifer
C  ( 3) deciduous needleleaf = deciduous
C  ( 4) deciduous broadleaf = deciduous
C  ( 5) mixed forests = 1/2 conifer + 1/2 deciduous = tree
C  ( 6) closed shrublands = 3/4 tree + 1/4 quartz
C  ( 7) open shrubland = 1/4 tree + 3/4 quartz
C  ( 8) woody savannas = grass
C  ( 9) savannas = grass
C  (10) grasslands = grass
C  (11) permanent wetlands = 1/2 grass + 1/2 water
C  (12) croplands = grass
C  (13) urban = black body
C  (14) mosaic = 1/2 grass + 1/2 mixed forest
C  (15) snow/ice
C  (16) barren/sparsely vegetated = desert(quartz)
C  (17) water
C  (18) tundra = frost
C
C  NOTE: Translation to Koster-Suarez surface types was as follows:
C  (  1) broadleaf evergreen  FROM above type 1  (conifer)
C  (  2) broadleaf deciduous  FROM above type 2  (deciduous)
C  (  3) needleleaf evergreen FROM above type 1  (conifer)
C  (  4) groundcover          FROM above type 10 (grass)
C  (  5) broadleaf shrubs     FROM above type 6  (closed shrublands)
C  (  6) dwarf trees (tundra) FROM above type 18 (tundra)
C  (  7) bare soil            FROM above type 16 (desert)
C  (  8) light desert         FROM above type 16 (desert)
C  (  9) glacier              FROM above type 15 (snow/ice)
C  ( 10) dark desert          FROM above type 16 (desert)
C  (100) ocean                FROM above type 17 (water)
C
C  NOTE: snow-covered ground uses interpolated emissivities based on snow depth
C =============================================================================
C -----------------------------------------------------------------------------
C   Emmissivities for 12 bands in Fu/Liou
C      band 1:   4.5 -  5.3 um
C      band 2:   5.3 -  5.9 um
C      band 3:   5.9 -  7.1 um
C      band 4:   7.1 -  8.0 um
C      band 5:   8.0 -  9.1 um
C      band 6:   9.1 - 10.2 um
C      band 7:  10.2 - 12.5 um
C      band 8:  12.5 - 14.9 um
C      band 9:  14.9 - 18.5 um
C      band 10: 18.5 - 25.0 um
C      band 11: 25.0 - 35.7 um
C      band 12: 35.7 -  oo  um
C
C-------------------------------------------------------------------------
      DATA ((emis(i,j),i=1,12),j=1,11) /
C evergreen needleleaf
     &   0.9891, 0.9892, 0.9900, 0.9914, 0.9908, 0.9903,
     &   0.9898, 0.9948, 1.0000, 1.0000, 1.0000, 1.0000,
C deciduous needleleaf
     &   0.9849, 0.9856, 0.9841, 0.9831, 0.9789, 0.9805,
     &   0.9733, 0.9869, 1.0000, 1.0000, 1.0000, 1.0000,
C evergreen needleleaf
     &   0.9891, 0.9892, 0.9900, 0.9914, 0.9908, 0.9903,
     &   0.9898, 0.9948, 1.0000, 1.0000, 1.0000, 1.0000,
C grasslands
     &   0.9867, 0.9897, 0.9920, 0.9933, 0.9830, 0.9752,
     &   0.9853, 0.9928, 1.0000, 1.0000, 1.0000, 1.0000,
C closed shrublands
     &   0.9490, 0.9697, 0.9738, 0.9712, 0.9474, 0.9582,
     &   0.9663, 0.9747, 0.9836, 0.9836, 0.9836, 0.9836,
C tundra
     &   0.9469, 0.9670, 0.9883, 0.9795, 0.9751, 0.9767,
     &   0.9920, 0.9888, 0.9888, 0.9888, 0.9888, 0.9888,
C barren
     &   0.8353, 0.9163, 0.9342, 0.9229, 0.8354, 0.8766,
     &   0.9210, 0.9262, 0.9345, 0.9345, 0.9345, 0.9345,
C barren
     &   0.8353, 0.9163, 0.9342, 0.9229, 0.8354, 0.8766,
     &   0.9210, 0.9262, 0.9345, 0.9345, 0.9345, 0.9345,
C snow/ice
     &   0.9998, 0.9998, 0.9998, 0.9998, 0.9998, 0.9999,
     &   0.9997, 0.9994, 0.9995, 0.9995, 0.9995, 0.9995,
C barren
     &   0.8353, 0.9163, 0.9342, 0.9229, 0.8354, 0.8766,
     &   0.9210, 0.9262, 0.9345, 0.9345, 0.9345, 0.9345,
C water
     &   0.9788, 0.9833, 0.9819, 0.9820, 0.9835, 0.9865,
     &   0.9886, 0.9719, 0.9719, 0.9719, 0.9719, 0.9719/

#include "snwmid.h"

C Convert to the 10 bands needed by Chou Radiation
C ------------------------------------------------
      do i=1,numpts

C land points
C------------
       if(ityp(i).le.10)then
        newemis(i, 1) = (emis( 1,ityp(i))+emis(2,ityp(i)))/2.
        newemis(i, 2) = (emis( 2,ityp(i))+emis(3,ityp(i)))/2.
        newemis(i, 3) = (emis( 4,ityp(i))+emis(5,ityp(i)))/2.
        newemis(i, 4) =  emis( 6,ityp(i))
        newemis(i, 5) =  emis( 7,ityp(i))
        newemis(i, 6) =  emis( 8,ityp(i))
        newemis(i, 7) =  emis( 9,ityp(i))
        newemis(i, 8) = (emis(10,ityp(i))+emis(11,ityp(i)))/2.
        newemis(i, 9) =  emis(12,ityp(i))
        newemis(i,10) =  emis( 4,ityp(i))

C modify emissivity for snow based on snow depth (like albedo)
C-------------------------------------------------------------
        if(snowdepth (i).gt.0.) then
         fac = snowdepth(i) / (snowdepth(i) + snwmid(ityp(i)))
         newemis(i, 1) = newemis(i, 1) + (((emis( 1,9)+emis( 2,9))/2.)
     &                                           - newemis(i, 1)) * fac
         newemis(i, 2) = newemis(i, 2) + (((emis( 2,9)+emis( 3,9))/2.)
     &                                           - newemis(i, 2)) * fac
         newemis(i, 3) = newemis(i, 3) + (((emis( 4,9)+emis( 5,9))/2.)
     &                                           - newemis(i, 3)) * fac
         newemis(i, 4) = newemis(i, 4) +              (emis( 6,9)
     &                                           - newemis(i, 4)) * fac
         newemis(i, 5) = newemis(i, 5) +              (emis( 7,9)
     &                                           - newemis(i, 5)) * fac
         newemis(i, 6) = newemis(i, 6) +              (emis( 8,9)
     &                                           - newemis(i, 6)) * fac
         newemis(i, 7) = newemis(i, 7) +              (emis( 9,9)
     &                                           - newemis(i, 7)) * fac
         newemis(i, 8) = newemis(i, 8) + (((emis(10,9)+emis(11,9))/2.)
     &                                           - newemis(i, 8)) * fac
         newemis(i, 9) = newemis(i, 9) +              (emis(12,9)
     &                                           - newemis(i, 9)) * fac
         newemis(i,10) = newemis(i,10) +              (emis( 4,9)
     &                                           - newemis(i,10)) * fac
        endif

C open water
C-----------
       else
        if(fraci(i).eq.0.)then
         newemis(i, 1) = (emis( 1,11)+emis(2,11))/2.
         newemis(i, 2) = (emis( 2,11)+emis(3,11))/2.
         newemis(i, 3) = (emis( 4,11)+emis(5,11))/2.
         newemis(i, 4) =  emis( 6,11)
         newemis(i, 5) =  emis( 7,11)
         newemis(i, 6) =  emis( 8,11)
         newemis(i, 7) =  emis( 9,11)
         newemis(i, 8) = (emis(10,11)+emis(11,11))/2.
         newemis(i, 9) =  emis(12,11)
         newemis(i,10) =  emis( 4,11)

C sea ice (like glacier and snow)
C--------------------------------
        else
         newemis(i, 1) = (emis( 1,9)+emis(2,9))/2.
         newemis(i, 2) = (emis( 2,9)+emis(3,9))/2.
         newemis(i, 3) = (emis( 4,9)+emis(5,9))/2.
         newemis(i, 4) =  emis( 6,9)
         newemis(i, 5) =  emis( 7,9)
         newemis(i, 6) =  emis( 8,9)
         newemis(i, 7) =  emis( 9,9)
         newemis(i, 8) = (emis(10,9)+emis(11,9))/2.
         newemis(i, 9) =  emis(12,9)
         newemis(i,10) =  emis( 4,9)
        endif
       endif
      enddo

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE GET_LANDFRAC(im,jm,nSx,nSy,bi,bj,maxtyp,surftype,
     &                                                    tilefrac,frac)
C***********************************************************************
C  Purpose
C     To compute the total fraction of land within a model grid-box
C
C***********************************************************************
      IMPLICIT NONE

      INTEGER im,jm,nSx,nSy,bi,bj,maxtyp
      INTEGER surftype(im,jm,maxtyp,nSx,nSy)
      _RL tilefrac(im,jm,maxtyp,nSx,nSy)
      _RL frac(im,jm)

      INTEGER  i,j,k

      do j=1,jm
      do i=1,im
      frac(i,j) = 0.0
      enddo
      enddo

      do k=1,maxtyp
      do j=1,jm
      do i=1,im
      if( (surftype(i,j,k,bi,bj).lt.100.).and.
     &                               (tilefrac(i,j,k,bi,bj).gt.0.0))then
       frac(i,j) = frac(i,j) + tilefrac(i,j,k,bi,bj)
      endif
      enddo
      enddo
      enddo

      return
      end
