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
      SUBROUTINE CFC12_FORCING( PTR_CFC12, GCFC12, 
     &                            bi,bj,imin,imax,jmin,jmax,
     &                             myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE CFC12_FORCING                                   |
C     | o Calculate the changes to CFC12 through air-sea  fluxes   |  
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
      _RL  PTR_CFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL  GCFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj, imin, imax, jmin, jmax

#ifdef ALLOW_PTRACERS
#ifdef ALLOW_CFC
C     == Local variables ==
      _RL  SURCFC12(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  xintp(1-OLy:sNy+OLy)
       INTEGER I,J
       _RL myYear
       INTEGER lastYear, thisYear
       _RL dtinc, aWght, bWght
       _RL ACFC12north, ACFC12south
       INTEGER maxYear
       _RL a1, a2
       _RL yNorth, ySouth

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           SURCFC12(i,j)=0.d0
          ENDDO
         ENDDO

c find atmospheric CFC
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
         ACFC12north = ACFC12(lastYear,1)*bWght
     &                + ACFC12(thisYear,1)*aWght
         ACFC12south = ACFC12(lastYear,2)*bWght
     &                + ACFC12(thisYear,2)*aWght
       else
         ACFC12north = ACFC12(maxYear,1)
         ACFC12south = ACFC12(maxYear,2)
       endif
       print*,'YEAR,ACFC12north,ACFC12south',  myYear,
     &        ACFC12north,ACFC12south
c provide gradient between N and S values
#define OCMIP_GRAD
#undef STEPH_GRAD
c STEPH'S INITIAL VERSION
#ifdef STEPH_GRAD
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          if ((j.gt.int(sNy/2)+3.and.j.le.sNy).or.j.lt.1) then
             ATMOSCFC12(i,j,bi,bj)=ACFC12north
          endif
          if (j.ge.int(sNy/2)-3.and.j.le.int(sNy/2)+3) then
             a1=(float(j-int(sNy/2)+3)+.5)/7
             a2=1.d0-a1
             ATMOSCFC12(i,j,bi,bj)=a1*ACFC12south +
     &                             a2*ACFC12north
          endif
          if ((j.lt.int(sNy/2)-3.and.j.gt.0).or.j.gt.sNy) then
             ATMOSCFC12(i,j,bi,bj)=ACFC12south
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
           ATMOSCFC12(i,j,bi,bj)= xintp(j) * ACFC12north
     &               + (1.0 - xintp(j))*ACFC12south

          ENDDO
c         print*,'QQ cfc12', j, ATMOSCFC12(1,j,bi,bj)
       ENDDO
#endif
c cfc12 air-sea interaction
       CALL CFC12_SURFFORCING( PTR_CFC12, SURCFC12,
     &                    bi,bj,imin,imax,jmin,jmax,
     &                    myIter,myTime,myThid)

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GCFC12(i,j,1)=GCFC12(i,j,1)+SURCFC12(i,j)
          ENDDO
         ENDDO

#endif
#endif

c
       RETURN
       END
