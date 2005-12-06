C modified for external_forcing_DIC.F  August 1999
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c modified swd Oct 01 and Feb 02, for use as package for c40_patch1
c modified to use with c44 and ptracers: swd May 2002
c modified to have carbonate and biological influences: swd June 2002
c modified for cfc: swd Sep 2003
C

#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CFC11_FORCING( PTR_CFC11, GCFC11, 
     &                            bi,bj,imin,imax,jmin,jmax,
     &                             myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE CFC11_FORCING                                   |
C     | o Calculate the changes to CFC11 through air-sea  fluxes   |  
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CFC.h"
#include "GCHEM.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  PTR_CFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj, imin, imax, jmin, jmax

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC
C     == Local variables ==
      _RL  SURCFC11(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  xintp(1-OLy:sNy+OLy)
       INTEGER I,J
       INTEGER myYear, lastYear, thisYear
       _RL dtinc, aWght, bWght
       _RL ACFC11north, ACFC11south
       INTEGER maxYear
       _RL a1, a2
       _RL yNorth, ySouth
       

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           SURCFC11(i,j)=0.d0
          ENDDO
         ENDDO

c find atmospheric CFC
       myYear=float(myIter-PTRACERS_Iter0)*deltaTclock
     &                       /(360.d0*24.d0*3600.d0)
       lastYear=1+int(myYear+0.5)
       thisYear=lastYear+1
       maxYear=cfc_yearend-cfc_yearbeg
       if (thisYear.lt.maxYear) then
         dtinc=myYear-float(lastYear-1)
         aWght=0.5d0+dtinc
         bWght=1.d0-aWght
c        IF (bi*bj.eq.1)
c       &write(0,*) 'myYear = ',myYear,lastYear,dtinc,aWght
         ACFC11north = ACFC11(lastYear,1)*bWght
     &                + ACFC11(thisYear,1)*aWght
         ACFC11south = ACFC11(lastYear,2)*bWght
     &                + ACFC11(thisYear,2)*aWght
       else
         ACFC11north = ACFC11(maxYear,1)
         ACFC11south = ACFC11(maxYear,2)
       endif
c      print*,'ACFC11north,ACFC11south', ACFC11north,ACFC11south,
c    &                                   lastYear,thisYear
c provide gradient between N and S values
#define OCMIP_GRAD
#undef STEPH_GRAD
c STEPH'S INITIAL VERSION
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
c OCMIP VERSION
#ifdef OCMIP_GRAD
       yNorth =  10.0
       ySouth = -10.0
       DO j=1-OLy,sNy+OLy
          i=1
          IF(yC(i,j,bi,bj) .GE. yNorth) THEN
             xintp(j) = 1.0
          ELSE IF(yC(i,j,bi,bj) .LE. ySouth) THEN
             xintp(j) = 0.0
          ELSE
             xintp(j) = (yC(i,j,bi,bj) - ySouth)/
     &                           (yNorth - ySouth)
          ENDIF
          DO i=1-OLx,sNx+OLx
           ATMOSCFC11(i,j,bi,bj)= xintp(j) * ACFC11north
     &               + (1.0 - xintp(j))*ACFC11south

          ENDDO
c         print*,'QQ cfc11', j, ATMOSCFC11(1,j,bi,bj)
       ENDDO
#endif
c cfc11 air-sea interaction
       CALL CFC11_SURFFORCING( PTR_CFC11, SURCFC11,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GCFC11(i,j,1)=GCFC11(i,j,1)+SURCFC11(i,j)
          ENDDO
         ENDDO

#endif
#endif

c
       RETURN
       END
