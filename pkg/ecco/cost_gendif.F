C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_gendif.F,v 1.2 2014/10/06 14:09:03 gforget Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: cost_gendif
C     !INTERFACE:
      subroutine cost_gendif(
     I                   localbar, nnzbar, localobs, nnzobs, localmask,
     I                   spminloc, spmaxloc, spzeroloc,
     I                   posproc, posproc_c,
     O                   localdif, difmask,
     I                   myThid
     &                   )

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE cost_gendif
C     ==================================================================
C     computes model-observation masked difference
C     ==================================================================
C     SUBROUTINE cost_gendif
C     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_ECCO
# include "ecco.h"
#endif

c     == routine arguments ==

      INTEGER myThid
      INTEGER nnzobs, nnzbar

      _RL localbar   (1-olx:snx+olx,1-oly:sny+oly,nnzbar,nsx,nsy)
      _RL localobs   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL localmask  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL localdif   (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)
      _RL difmask    (1-olx:snx+olx,1-oly:sny+oly,nnzobs,nsx,nsy)

      _RL spminloc, spmaxloc, spzeroloc

      character*(MAX_LEN_FNAM) posproc(NGENPPROC)
      character*(MAX_LEN_FNAM) posproc_c(NGENPPROC)

#ifdef ALLOW_ECCO

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

CEOP


c--     Determine the model-data difference mask
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nnzobs
             do j = jmin,jmax
              do i = imin,imax
#ifdef ALLOW_OLD_ESTIM_CODES
               difmask(i,j,k,bi,bj) = cosphi(i,j,bi,bj)*localmask(i,j,k,bi,bj)
#else
               difmask(i,j,k,bi,bj) = localmask(i,j,k,bi,bj)
#endif
                if ( localobs(i,j,k,bi,bj) .lt. spminloc .or.
     &               localobs(i,j,k,bi,bj) .gt. spmaxloc .or.
     &               localobs(i,j,k,bi,bj) .eq. spzeroloc ) then
                   difmask(i,j,k,bi,bj) = 0. _d 0
                endif
               localdif(i,j,k,bi,bj) = difmask(i,j,k,bi,bj)*
     &             (localbar(i,j,k,bi,bj)-localobs(i,j,k,bi,bj))
              enddo
             enddo
            enddo
          enddo
        enddo

#endif /* ALLOW_ECCO */

      RETURN
      END

