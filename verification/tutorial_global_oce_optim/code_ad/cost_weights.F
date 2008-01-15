C $Header: /u/gcmpack/MITgcm/verification/tutorial_global_oce_optim/code_ad/cost_weights.F,v 1.1 2008/01/15 21:16:42 dfer Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      subroutine cost_weights( mythid )

c     ==================================================================
c     SUBROUTINE cost_weights
c     ==================================================================
c
c     o Set weights used in the cost function and in the
c       normalization of the sensitivities when ALLOW_NON_DIMENSIONAL

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "ctrl.h"
#include "ctrl_weights.h"
#include "cost.h"

c     == routine arguments ==

      integer  mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer gwunit

      _RL factor
      _RL dummy
      real*8 wti(nr)
      real*8 wtemp

c     == external ==

      integer  ifnblnk
      external ifnblnk
      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

c--   Initialize variance (weight) fields.
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = jmin,jmax
            DO i = imin,imax
              whfluxm(i,j,bi,bj)= 0. _d 0
            ENDDO
          ENDDO
          DO k = 1,nr
             wunit(k,bi,bj)  = 1. _d 0
             wtheta(k,bi,bj) = 0. _d 0
             wsalt(k,bi,bj)  = 0. _d 0
          ENDDO
        ENDDO
      ENDDO

c--   Build area weighting matrix used in the cost function
c--   contributions.

c--   Read error information and set up weight matrices.
c--   First the temperature weights :
c
#ifdef ALLOW_COST_TEMP
      _BEGIN_MASTER(myThid)
       call mdsfindunit( gwunit, mythid )
       open(gwunit, file='Err_levitus_15layer.bin',
     &               status='old', form='unformatted',
     &     access = 'sequential'   )
       read(gwunit) (wti(k), k=1,nr)
       close(gwunit)
      _END_MASTER(myThid)
      _BARRIER
      print*,'Weights for temperature: wti', (wti(k),k=1,nr)

      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO k = 1, nr
               wtheta(k,bi,bj) = 1. _d 0/wti(k)/wti(k)
          ENDDO
        ENDDO
      ENDDO
#endif
c
c--   Then the hflux weights :
c
#if (defined (ALLOW_COST_HFLUXM) || defined (ALLOW_HFLUXM_CONTROL))
      _BEGIN_MASTER(myThid)
      CALL MDSREADFIELD('Err_hflux.bin',precFloat64,
     &                  'RL',1,whfluxm,1,myThid)
      _END_MASTER(myThid)
      _EXCH_XY_RL(whfluxm   , myThid )
      DO bj = jtlo,jthi
        DO bi = itlo,ithi
          DO j = jmin,jmax
            DO i = imin,imax
             print*,'Uncertainties for Heat Flux',i,j,whfluxm(i,j,bi,bj)
             IF (whfluxm(i,j,bi,bj) .NE. 0. _d 0) THEN
                 whfluxm(i,j,bi,bj) = 1. _d 0 /whfluxm(i,j,bi,bj)
     &                                        /whfluxm(i,j,bi,bj)
             ELSE
                 whfluxm(i,j,bi,bj) = 1. _d 0
             ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#ifdef ALLOW_NONDIMENSIONAL_CONTROL_IO
      call active_write_xy('whfluxm',whfluxm,1,0,mythid,dummy)
#endif
#endif
c
      end
