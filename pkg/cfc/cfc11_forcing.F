C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc11_forcing.F,v 1.12 2013/06/07 15:43:31 jmc Exp $
C $Name:  $

C modified for external_forcing_DIC.F  August 1999
C
C modified swd Oct 01 and Feb 02, for use as package for c40_patch1
C modified to use with c44 and ptracers: swd May 2002
C modified to have carbonate and biological influences: swd June 2002
C modified for cfc: swd Sep 2003

#include "GCHEM_OPTIONS.h"
#define OCMIP_GRAD
#undef STEPH_GRAD

CStartOfInterFace
      SUBROUTINE CFC11_FORCING( 
     I                          pTr_CFC11, 
     U                          gCFC11,
     I                          bi, bj, imin, imax, jmin, jmax,
     I                          myIter, myTime, myThid )

C     *==========================================================*
C     | SUBROUTINE CFC11_FORCING
C     | o Calculate the changes to CFC11 through air-sea  fluxes
C     *==========================================================*
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CFC.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  pTr_CFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  gCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj, imin, imax, jmin, jmax

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC
C     == Local variables ==
      _RL  gSurfCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER i,j
       _RL myYear
       INTEGER lastYear, thisYear
       _RL dtinc, aWght, bWght
       _RL ACFC11north, ACFC11south
       INTEGER maxYear
#ifdef STEPH_GRAD
       _RL a1, a2
#endif
       _RL yNorth, ySouth
       _RL recip_dLat, weight

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          gSurfCFC11(i,j) = 0. _d 0
        ENDDO
       ENDDO

C find atmospheric CFC
       myYear=float(myIter-PTRACERS_Iter0)*deltaTclock
     &                       /(360.d0*24.d0*3600.d0)
       lastYear=1+int(myYear+0.5)
       thisYear=lastYear+1
       maxYear=cfc_yearend-cfc_yearbeg
       if (thisYear.lt.maxYear) then
         dtinc=myYear-(lastYear-1.d0)
         aWght=0.5d0+dtinc
         bWght=1.d0-aWght
c        IF (bi*bj.eq.1)
c    &      write(0,*) 'myYear = ',myYear,lastYear,dtinc,aWght
         ACFC11north = ACFC11(lastYear,1)*bWght
     &               + ACFC11(thisYear,1)*aWght
         ACFC11south = ACFC11(lastYear,2)*bWght
     &               + ACFC11(thisYear,2)*aWght
       else
         ACFC11north = ACFC11(maxYear,1)
         ACFC11south = ACFC11(maxYear,2)
       endif
c      print*,'YEAR,ACFC11north,ACFC11south',  myYear,
c    &        ACFC11north,ACFC11south
C provide gradient between N and S values

C STEPH S INITIAL VERSION
#ifdef STEPH_GRAD
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          if ((j.gt.int(sNy/2)+3.and.j.le.sNy).or.j.lt.1) then
             ATMOSCFC11(i,j,bi,bj)=ACFC11north
          endif
          if (j.ge.int(sNy/2)-3.and.j.le.int(sNy/2)+3) then
             a1=(float(j-int(sNy/2)+3)+.5)/7
             a2=1.d0-a1
             ATMOSCFC11(i,j,bi,bj)=a1*ACFC11south +
     &                             a2*ACFC11north
          endif
          if ((j.lt.int(sNy/2)-3.and.j.gt.0).or.j.gt.sNy) then
             ATMOSCFC11(i,j,bi,bj)=ACFC11south
          endif
        ENDDO
       ENDDO
#endif
C OCMIP VERSION
#ifdef OCMIP_GRAD
       yNorth =  10.0
       ySouth = -10.0
       recip_dLat = 1. _d 0 / ( yNorth - ySouth )
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          weight = ( yC(i,j,bi,bj) - ySouth )*recip_dLat
          weight = MAX( zeroRL, MIN( oneRL, weight ) )
          ATMOSCFC11(i,j,bi,bj)= weight * ACFC11north
     &               + ( oneRL - weight )*ACFC11south

        ENDDO
c         print*,'QQ cfc11', j, ATMOSCFC11(1,j,bi,bj)
       ENDDO
#endif
C cfc11 air-sea interaction
       CALL CFC11_SURFFORCING( pTr_CFC11, gSurfCFC11,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          gCFC11(i,j,1) = gCFC11(i,j,1) + gSurfCFC11(i,j)
        ENDDO
       ENDDO

#endif /* ALLOW_CFC */
#endif /* ALLOW_PTRACERS */

       RETURN
       END
