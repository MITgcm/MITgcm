C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_tendency_apply.F,v 1.12 2006/01/18 19:00:38 molod Exp $
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
      _RL tmpdiag(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      integer i, j
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
#endif

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

      if(diagnostics_is_on('DIABUDYN',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = ( maskW(i,j,kLev,bi,bj) * guphy(i,j,kLev,bi,bj) 
     .    - rayleighdrag * maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj) ) 
     .    * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABUDYN',kLev,1,2,bi,bj,myThid)
      endif

      if(diagnostics_is_on('RFU     ',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = -1. _d 0 * rayleighdrag * 
     .       maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj) * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RFU     ',kLev,1,2,bi,bj,myThid)
      endif

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
      _RL tmpdiag(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      integer i, j
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
#endif

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

      if(diagnostics_is_on('DIABVDYN',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = ( maskS(i,j,kLev,bi,bj) * gvphy(i,j,kLev,bi,bj) 
     .    - rayleighdrag * maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj) ) 
     .    * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABVDYN',kLev,1,2,bi,bj,myThid)
      endif

      if(diagnostics_is_on('RFV     ',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = -1. _d 0 * rayleighdrag * 
     .       maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj) * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RFV     ',kLev,1,2,bi,bj,myThid)
      endif

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
      _RL tmpdiag(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      integer i, j
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
#endif

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

      if(diagnostics_is_on('DIABTDYN',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = ( maskC(i,j,kLev,bi,bj) * gthphy(i,j,kLev,bi,bj) 
     . + rayleighdrag * 0.5 *
     . (maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)+ 
     .  maskW(i+1,j,kLev,bi,bj)*
     .                uVel(i+1,j,kLev,bi,bj)*uVel(i+1,j,kLev,bi,bj)+ 
     .  maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)+
     .  maskS(i,j+1,kLev,bi,bj)*
     .                vVel(i,j+1,kLev,bi,bj)*vVel(i,j+1,kLev,bi,bj)) )
     .    * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABTDYN',kLev,1,2,bi,bj,myThid)
      endif

      if(diagnostics_is_on('RFT     ',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = ( rayleighdrag * 0.5 *
     . (maskW(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)*uVel(i,j,kLev,bi,bj)+ 
     .  maskW(i+1,j,kLev,bi,bj)*
     .                uVel(i+1,j,kLev,bi,bj)*uVel(i+1,j,kLev,bi,bj)+ 
     .  maskS(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)*vVel(i,j,kLev,bi,bj)+
     .  maskS(i,j+1,kLev,bi,bj)*
     .                vVel(i,j+1,kLev,bi,bj)*vVel(i,j+1,kLev,bi,bj)) )
     .       * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'RFT     ',kLev,1,2,bi,bj,myThid)
      endif

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
      _RL tmpdiag(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

      integer i, j
#ifdef ALLOW_DIAGNOSTICS
      logical  diagnostics_is_on
      external diagnostics_is_on
#endif

      do j=jMin,jMax
       do i=iMin,iMax
        gS(i,j,kLev,bi,bj) = maskC(i,j,kLev,bi,bj)
     .       *( gS(i,j,kLev,bi,bj) + gsphy(i,j,kLev,bi,bj) )
       enddo
      enddo

      if(diagnostics_is_on('DIABQDYN',myThid) ) then
       do j=jMin,jMax
       do i=iMin,iMax
        tmpdiag(i,j) = ( maskC(i,j,kLev,bi,bj) * gsphy(i,j,kLev,bi,bj) )
     .    * 86400
       enddo
       enddo
       call diagnostics_fill(tmpdiag,'DIABQDYN',kLev,1,2,bi,bj,myThid)
      endif

      return
      end
