C $Header: /u/gcmpack/MITgcm/pkg/ecco/cost_averagesinit.F,v 1.6 2007/09/20 22:01:37 heimbach Exp $

#include "COST_CPPOPTIONS.h"

      subroutine cost_averagesinit( mythid )

c     ==================================================================
c     SUBROUTINE cost_averagesinit
c     ==================================================================
c
c     o Set average fields for temperature, salinity, surface pressure,
c       and averaging counters to zero. The average fields are declared
c       in the header file ecco_cost.h.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_averagesinit
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "ecco_cost.h"
#include "ctrl_dummy.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_COST.h"
#endif

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec

c     == end of interface ==

c--   Set the loop ranges.
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      do bj = jtlo,jthi
        do bi = itlo,ithi

#ifdef ALLOW_SSH_COST_CONTRIBUTION
c--       Initialise surface pressure average.
          do j = jmin,jmax
            do i = imin,imax
              psbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo

          xx_psbar_mean_dummy = 0. _d 0
#endif

#ifdef ALLOW_BP_COST_CONTRIBUTION
c--       Initialise surface pressure average.
          do j = jmin,jmax
            do i = imin,imax
              bpbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo

          xx_bpbar_mean_dummy = 0. _d 0
#endif

#if (defined (ALLOW_THETA_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDT_COST_CONTRIBUTION) || \
     defined (ALLOW_XBT_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--       Initialise temperature average (3d).
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                tbar(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
          xx_tbar_mean_dummy = 0. _d 0
          nnztbar = nr
#else
#ifdef ALLOW_SST_COST_CONTRIBUTION
c--       Initialise temperature average (2d).
          k = 1
          do j = jmin,jmax
            do i =  imin,imax
              tbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
          xx_tbar_mean_dummy = 0. _d 0
          nnztbar = 1
#endif
#endif

#if (defined (ALLOW_SALT_COST_CONTRIBUTION) || \
     defined (ALLOW_CTDS_COST_CONTRIBUTION) || \
     defined (ALLOW_DRIFT_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--       Initialise salt average.
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                sbar(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
          xx_sbar_mean_dummy = 0. _d 0
          nnzsbar = nr
#else
#ifdef ALLOW_SSS_COST_CONTRIBUTION
c--       Initialise salinity average (2d).
          k = 1
          do j = jmin,jmax
            do i =  imin,imax
              sbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
          xx_sbar_mean_dummy = 0. _d 0
          nnzsbar = 1
#endif          
#endif

#if (defined (ALLOW_DRIFTER_COST_CONTRIBUTION) || \
     defined (ALLOW_OBCS_COST_CONTRIBUTION))
c--       Initialise uvel, vvel average.
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                ubar(i,j,k,bi,bj) = 0. _d 0
                vbar(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
          xx_ubar_mean_dummy = 0. _d 0
          xx_vbar_mean_dummy = 0. _d 0
#endif

#ifdef ALLOW_DRIFTW_COST_CONTRIBUTION
c--       Initialise uvel, vvel average.
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                wbar(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
          xx_wbar_mean_dummy = 0. _d 0
#endif

#ifdef ALLOW_SCAT_COST_CONTRIBUTION
c--       Initialise salinity average (2d).
          do j = jmin,jmax
            do i =  imin,imax
              tauxbar(i,j,bi,bj) = 0. _d 0
              tauybar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
          xx_taux_mean_dummy = 0. _d 0
          xx_tauy_mean_dummy = 0. _d 0
#endif

#ifdef ALLOW_MEAN_HFLUX_COST_CONTRIBUTION
c--       Initialise salinity average (2d).
          do j = jmin,jmax
            do i =  imin,imax
              hfluxbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
          xx_hflux_mean_dummy = 0. _d 0
#endif

#ifdef ALLOW_MEAN_SFLUX_COST_CONTRIBUTION
c--       Initialise salinity average (2d).
          do j = jmin,jmax
            do i =  imin,imax
              sfluxbar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo  
          xx_sflux_mean_dummy = 0. _d 0
#endif    

#ifdef ALLOW_SEAICE

# ifdef ALLOW_SEAICE_COST_SMR_AREA
c--       Initialise salinity average (2d).
          do j = jmin,jmax
            do i =  imin,imax
              smrareabar(i,j,bi,bj) = 0. _d 0
            enddo
          enddo  
          xx_smrareabar_mean_dummy = 0. _d 0
# endif

#endif    

#ifdef ALLOW_TRANSPORT_COST_CONTRIBUTION
          do irec = 1, ndaysrec
             transpbar(irec,bi,bj) = 0. _d 0
          enddo
#endif

        enddo
      enddo

      return
      end

