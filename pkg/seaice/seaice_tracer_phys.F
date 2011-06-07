C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_tracer_phys.F,v 1.1 2011/06/07 03:58:23 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_TRACER_PHYS( myTime, myIter, myThid )
C     /=======================================================\
C     | SUBROUTINE seaice_tracer_phys                         |
C     | o Time step SItr/SItrEFF as a result of               |
C     |   seaice thermodynamics and specific tracer physics   |
C     \=======================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_TRACER.h"

C     === Routine arguments ===
C     INPUT:
C     myTime  :: Simulation time
C     myIter  :: Simulation timestep number
C     myThid  :: Thread no. that called this routine.
C     OUTPUT:
      _RL myTime
      INTEGER myIter, myThid
CEndOfInterface

C     === Local variables ===
#ifdef ALLOW_SITRACER

      INTEGER iTr, jTh, I, J, bi, bj, ks
      _RL SItrFromOcean  (1:sNx,1:sNy)
      _RL SItrFromSnow   (1:sNx,1:sNy)
      _RL HEFFprev, HEFFpost, growFact, meltPart, tmpscal1

#ifdef ALLOW_SITRACER_DIAG
      _RL DIAGarray     (1:sNx,1:sNy,Nr)
#endif

cgf for now I do not fully account for ocean-ice fluxes of tracer
cgf -> I just prescribe it consistent with age tracer
cgf eventually I will need to handle them as function params

      ks=1

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)
      DO iTr=1,SItrMaxNum

c 0) set ice-ocean and ice-snow exchange values       
c =============================================
      DO J=1,sNy
       DO I=1,sNx
        SItrFromOcean(i,j)=0. _d 0
        SItrFromSnow(i,j)=0. _d 0
       ENDDO
      ENDDO
      if (SItrName(iTr).EQ.'age') then
c age tracer: not passed from ocean or snow
       DO J=1,sNy
        DO I=1,sNx
         SItrFromOcean(i,j)=0. _d 0
        ENDDO
       ENDDO
      elseif (SItrName(iTr).EQ.'salinity') then
c salinity tracer: 
       DO J=1,sNy
        DO I=1,sNx
         SItrFromOcean(i,j)=SIsal0
#ifdef SEAICE_VARIABLE_SALINITY
         if (SIsalFRAC.GT.0.) 
     &   SItrFromOcean(i,j)=SIsalFRAC*salt(I,j,ks,bi,bj)
#endif
        ENDDO
       ENDDO
      elseif (SItrName(iTr).EQ.'one') then
c "ice concentration" tracer that should remain .EQ.1.
       DO J=1,sNy
        DO I=1,sNx
         SItrFromOcean(i,j)=1. _d 0
         SItrFromSnow(i,j)=1. _d 0
        ENDDO
       ENDDO
      endif
c 1) seaice thermodynamics processes
c ==================================
      DO J=1,sNy
       DO I=1,sNx
        HEFFprev=SItrHEFF(i,j,bi,bj,1)
#ifdef ALLOW_SITRACER_DIAG
        diagArray(I,J,5+(iTr-1)*5) = 
     &    HEFFprev*SItracer(i,j,bi,bj,iTr) + SItrBucket(i,j,bi,bj,iTr)
#endif
c apply the sequence of thermodynamics increments to actual traceur
c (see seaice_growth.F)
c (jTh=1 tendency due to ice-ocean interaction)
c (jTh=2 tendency due to the atmosphere, over ice covered part)
c (jTh=3 tendency due to the atmosphere, over open water part)
c (jTh=4 tendency due to flooding)
        DO jTh=1,3
         HEFFprev=SItrHEFF(i,j,bi,bj,jTh)
         HEFFpost=SItrHEFF(i,j,bi,bj,jTh+1)
c compute ratio in [0. 1.] range for either growth or melt
         growFact=1. _d 0
         meltPart=0. _d 0
         if (HEFFpost.GT.HEFFprev) growFact=HEFFprev/HEFFpost
         if (HEFFpost.LT.HEFFprev) meltPart=HEFFprev-HEFFpost
c update SItr accordingly
         SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)*growFact
     &                      +SItrFromOcean(i,j)*(1. _d 0 - growFact)
         SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &             -HEFFpost*SItrFromOcean(i,j)*(1. _d 0 - growFact)
         SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &                            +meltPart*SItracer(i,j,bi,bj,iTr)
        ENDDO
c apply flooding term
        growFact=1. _d 0
        HEFFprev=SItrHEFF(i,j,bi,bj,4)
        HEFFpost=SItrHEFF(i,j,bi,bj,5)
        if (HEFFpost.GT.HEFFprev) growFact=HEFFprev/HEFFpost
        SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)*growFact
     &                     +SItrFromSnow(i,j) *(1. _d 0 - growFact)
        if ((SItrName(iTr).EQ.'age').OR.(SItrName(iTr).EQ.'one')) then
c Including this term in the bucket only makes sense for diagnostics purposes
c and should not be done for tracers that are exchanged with the ocean (we would
c need another bucket for ice-snow exchange, and snow tracers to start with).
          SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &             -HEFFpost*SItrFromSnow(i,j)*(1. _d 0 - growFact)
        endif
#ifdef ALLOW_SITRACER_DIAG
        diagArray(I,J,5+(iTr-1)*5) = HEFFpost*SItracer(i,j,bi,bj,iTr) 
     &  +SItrBucket(i,j,bi,bj,iTr)-diagArray(I,J,5+(iTr-1)*5)
#endif
       ENDDO
      ENDDO
c 2) very ice tracer processes
c ============================
      if (SItrName(iTr).EQ.'age') then
c age tracer: grow old as time passes by
       DO J=1,sNy
        DO I=1,sNx
          if (SItrHEFF(i,j,bi,bj,5).GT.0. _d 0) then
            SItracer(i,j,bi,bj,iTr)=
     &      SItracer(i,j,bi,bj,iTr)+SEAICE_deltaTtherm
          else
            SItracer(i,j,bi,bj,iTr)=0. _d 0
          endif
        ENDDO
       ENDDO
      elseif (SItrName(iTr).EQ.'salinity') then
c salinity tracer: no specific process
      elseif (SItrName(iTr).EQ.'one') then
c "ice concentration" tracer: no specific process
      endif
c 3) ice-ocean tracer exchange
c =============================
      if (SItrName(iTr).EQ.'age') then
c age tracer: not passed to ocean
      elseif (SItrName(iTr).EQ.'salinity') then
c salinity tracer: salt flux
c      DO J=1,sNy
c       DO I=1,sNx
c        saltFlux(I,J,bi,bj) = - SItrBucket(i,j,bi,bj,iTr)
c    &     *HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm*SEAICE_rhoIce
c note: at this point of the time step, that is the correct sign
c        saltPlumeFlux(I,J,bi,bj) = ...
c       ENDDO
c      ENDDO
      elseif (SItrName(iTr).EQ.'one') then
c "ice concentration" tracer: not passed to ocean
      endif
      DO J=1,sNy
       DO I=1,sNx
#ifdef ALLOW_SITRACER_DIAG
        diagArray(I,J,4+(iTr-1)*5) = - SItrBucket(i,j,bi,bj,iTr)
     &  *HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm*SEAICE_rhoIce
#endif
c empty bucket
        SItrBucket(i,j,bi,bj,iTr)=0. _d 0
       ENDDO
      ENDDO
c 4) diagnostics
c ==============
#ifdef ALLOW_SITRACER_DIAG
      DO J=1,sNy
       DO I=1,sNx
        HEFFpost=SItrHEFF(i,j,bi,bj,5)
        DIAGarray(I,J,1+(iTr-1)*5) = SItracer(i,j,bi,bj,iTr)
        DIAGarray(I,J,2+(iTr-1)*5) = SItracer(i,j,bi,bj,iTr)*HEFFpost
c diagArray(:,:,3) is the term of comparison for diagArray(:,:,2)
        if (SItrName(iTr).EQ.'age') then
          DIAGarray(I,J,3+(iTr-1)*5) = IceAgeTr(i,j,bi,bj,2)
        elseif (SItrName(iTr).EQ.'salinity') then
          DIAGarray(I,J,3+(iTr-1)*5) = HSALT(i,j,bi,bj)/SEAICE_rhoIce
        elseif (SItrName(iTr).EQ.'one') then
          DIAGarray(I,J,3+(iTr-1)*5) = HEFFpost
        endif
c diagArray(:,:,4) allows check of conservation : del(SItrBucket)+del(SItr*HEFF)=0. over do_phys
c diagArray(:,:,5) is the tracer flux from the ocean (<0 incr. ocean tracer)
       ENDDO
      ENDDO
#endif
      ENDDO
#ifdef ALLOW_SITRACER_DIAG
      CALL DIAGNOSTICS_FILL(DIAGarray,'UDIAG1  ',0,Nr,3,bi,bj,myThid)
#endif
      ENDDO
      ENDDO

#endif /* ALLOW_SITRACER */

      RETURN
      END
