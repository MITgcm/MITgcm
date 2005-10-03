C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_tendency_apply.F,v 1.10 2005/10/03 18:49:07 molod Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"
      subroutine fizhi_tendency_apply_u(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_u
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to U tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C        Assumes that U and V tendencies are already on C-Grid
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
      _RL rayleighdrag

      integer i, j

      if(klev.eq.Nr .or. rC(klev).lt.1000.)then
       rayleighdrag = 1./(31.*86400.*2.)
      else
       rayleighdrag = 0.
      endif

      do j=jMin,jMax
       do i=iMin,iMax
        gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj) + 
     .      maskW(i,j,kLev,bi,bj) * guphy(i,j,kLev,bi,bj)
     . - rayleighdrag * maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_v(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_v
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to V tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C        Assumes that U and V tendencies are already on C-Grid
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
      _RL rayleighdrag

      integer i, j

      if(klev.eq.Nr .or. rC(klev).lt.1000.)then
       rayleighdrag = 1./(31.*86400.*2.)
      else
       rayleighdrag = 0.
      endif

      do j=jMin,jMax
       do i=iMin,iMax
        gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj) + 
     .      maskS(i,j,kLev,bi,bj) * gvphy(i,j,kLev,bi,bj)
     . - rayleighdrag * maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_t(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_t
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to T (theta) tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime
      _RL rayleighdrag,getcon,cp,kappa,pNrkappa

      integer i, j

      if(klev.eq.Nr .or. rC(klev).lt.1000.)then
       cp = getcon('CP')
       kappa = getcon('KAPPA')
       pNrkappa = (rC(klev)/100000.)**kappa 
       rayleighdrag = 1./((31.*86400.*2.)*(pNrkappa*cp))
      else
       rayleighdrag = 0.
      endif

      do j=jMin,jMax
       do i=iMin,iMax
        gT(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     .       *( gT(i,j,kLev,bi,bj) + gthphy(i,j,kLev,bi,bj) )
     . + rayleighdrag * 0.5 *
     . (maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)+ 
     .  maskW(i+1,j,kLev,bi,bj)*
     .                uVel(i+1,j,kLev,bi,bj)*uVel(i+1,j,kLev,bi,bj)+ 
     .  maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)+
     .  maskS(i,j+1,kLev,bi,bj)*
     .                vVel(i,j+1,kLev,bi,bj)*vVel(i,j+1,kLev,bi,bj))
       enddo
      enddo

      return
      end
      subroutine fizhi_tendency_apply_s(iMin, iMax, jMin, jMax,
     .    bi,bj,kLev,myTime,myThid)
C=======================================================================
C Routine: fizhi_tendency_apply_s
C     Interpolate tendencies from physics grid to dynamics grid and
C     add fizhi tendency terms to S tendency.
C   
C INPUT: 
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
C
C Notes: Routine works for one level at a time
C=======================================================================
      implicit none

#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "fizhi_SIZE.h"
#include "fizhi_land_SIZE.h"
#include "fizhi_coms.h"

      integer iMin, iMax, jMin, jMax, kLev, bi, bj, myThid
      _RL myTime

      integer i, j

      do j=jMin,jMax
       do i=iMin,iMax
        gS(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     .       *( gS(i,j,kLev,bi,bj) + gsphy(i,j,kLev,bi,bj) )
       enddo
      enddo

      return
      end
