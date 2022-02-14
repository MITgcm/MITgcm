#include "SEAICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE SEAICE_TRACER_PHYS( myTime, myIter, myThid )
C     *=======================================================*
C     | SUBROUTINE seaice_tracer_phys
C     | o Time step SItr/SItrEFF as a result of
C     |   seaice thermodynamics and specific tracer physics
C     *=======================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_TRACER.h"
#ifdef ALLOW_SALT_PLUME
# include "SALT_PLUME.h"
#endif

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
      _RL SItrFromFlood   (1:sNx,1:sNy)
      _RL HEFFprev, HEFFpost, growFact, meltPart, tmpscal1
      _RL SItrExpand  (1:sNx,1:sNy)
      _RL AREAprev, AREApost, expandFact
      CHARACTER*8   diagName

#ifdef ALLOW_SITRACER_DEBUG_DIAG
      _RL DIAGarray     (1:sNx,1:sNy,Nr)
#endif

cgf for now I do not fully account for ocean-ice fluxes of tracer
cgf -> I just prescribe it consistent with age tracer
cgf eventually I will need to handle them as function params

      ks=1

      DO bj=myByLo(myThid),myByHi(myThid)
      DO bi=myBxLo(myThid),myBxHi(myThid)
      DO iTr=1,SItrNumInUse

c 0) set ice-ocean and ice-snow exchange values
c =============================================
      DO J=1,sNy
       DO I=1,sNx
        SItrFromOcean(i,j)=SItrFromOcean0(iTr)
        SItrFromFlood(i,j)=SItrFromFlood0(iTr)
        SItrExpand(i,j)=SItrExpand0(iTr)
       ENDDO
      ENDDO
c salinity tracer:
      if ( (SItrName(iTr).EQ.'salinity').AND.
     &      (SItrFromOceanFrac(iTr).GT.ZERO) ) then
       DO J=1,sNy
        DO I=1,sNx
         SItrFromOcean(i,j)=SItrFromOceanFrac(iTr)*salt(I,j,ks,bi,bj)
         SItrFromFlood(i,j)=SItrFromFloodFrac(iTr)*salt(I,j,ks,bi,bj)
        ENDDO
       ENDDO
      endif
c 1) seaice thermodynamics processes
c ==================================
      if (SItrMate(iTr).EQ.'HEFF') then
      DO J=1,sNy
       DO I=1,sNx
        HEFFprev=SItrHEFF(i,j,bi,bj,1)
#ifdef ALLOW_SITRACER_DEBUG_DIAG
        DIAGarray(I,J,5+(iTr-1)*5) =
     &    HEFFprev*SItracer(i,j,bi,bj,iTr) + SItrBucket(i,j,bi,bj,iTr)
#endif
c apply the sequence of thermodynamics increments to actual tracer
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
     &                     +SItrFromFlood(i,j) *(1. _d 0 - growFact)
c rk: flooding can only imply an ocean-ice tracer exchange, as long
c as we dont have snow tracers, so it goes through SItrBucket.
          SItrBucket(i,j,bi,bj,iTr)=SItrBucket(i,j,bi,bj,iTr)
     &             -HEFFpost*SItrFromFlood(i,j)*(1. _d 0 - growFact)
#ifdef ALLOW_SITRACER_DEBUG_DIAG
        DIAGarray(I,J,5+(iTr-1)*5) = HEFFpost*SItracer(i,j,bi,bj,iTr)
     &  +SItrBucket(i,j,bi,bj,iTr)-DIAGarray(I,J,5+(iTr-1)*5)
#endif
       ENDDO
      ENDDO
c TAF?      if (SItrMate(iTr).EQ.'AREA') then
      else
c 1) or seaice cover expansion
c ============================
c this is much simpler than for ice volume/mass tracers, because
c properties of the ice surface are not be conserved across the
c ocean-ice system, the contraction/expansion terms are all
c simultaneous (which is sane), and the only generic effect
c is due to expansion (new cover).
      DO J=1,sNy
       DO I=1,sNx
c apply expansion
        AREAprev=SItrAREA(i,j,bi,bj,2)
        AREApost=SItrAREA(i,j,bi,bj,3)
c compute ratio in [0. 1.] range for expansion/contraction
        expandFact=1. _d 0
        if (AREApost.GT.AREAprev) expandFact=AREAprev/AREApost
c update SItr accordingly
         SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)*expandFact
     &                      +SItrExpand(i,j)*(1. _d 0 - expandFact)
       ENDDO
      ENDDO
      endif
c 2) very ice tracer processes
c ============================
      if (SItrName(iTr).EQ.'age') then
c age tracer: grow old as time passes by
       DO J=1,sNy
        DO I=1,sNx
          if (( (SItrHEFF(i,j,bi,bj,5).GT.0. _d 0).AND.(SItrMate(iTr)
     &     .EQ.'HEFF') ).OR.( (SItrAREA(i,j,bi,bj,3).GT.0. _d 0).AND.
     &     (SItrMate(iTr).EQ.'AREA') )) then
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
      elseif (SItrName(iTr).EQ.'ridge') then
c simple, made up, ice surface roughness index prototype
       DO J=1,sNy
        DO I=1,sNx
c ridging increases roughness
          SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)+
     &    MAX(0. _d 0, SItrAREA(i,j,bi,bj,1)-SItrAREA(i,j,bi,bj,2))
c ice melt reduces ridges/roughness
          HEFFprev=SItrHEFF(i,j,bi,bj,1)
          HEFFpost=SItrHEFF(i,j,bi,bj,4)
          tmpscal1=1. _d 0
          if (HEFFprev.GT.HEFFpost) tmpscal1=HEFFpost/HEFFprev
          SItracer(i,j,bi,bj,iTr)=SItracer(i,j,bi,bj,iTr)*tmpscal1
        ENDDO
       ENDDO
      endif
c 3) ice-ocean tracer exchange/mapping to external variables
c ==========================================================
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. SItrMate(iTr).EQ.'HEFF') THEN
        WRITE(diagName,'(A4,I2.2,A2)') 'SItr',iTr,'Fx'
        tmpscal1=-ONE/SEAICE_deltaTtherm*SEAICE_rhoIce
        CALL DIAGNOSTICS_SCALE_FILL(SItrBucket(1-OLx,1-OLy,bi,bj,iTr),
     &   tmpscal1, 1, diagName,0,1,2,bi,bj,myThid)
      ENDIF
#endif

      if ( (SItrName(iTr).EQ.'salinity').AND.
     &     (SEAICE_salinityTracer) ) then
c salinity tracer: salt flux
        DO J=1,sNy
         DO I=1,sNx
          saltFlux(I,J,bi,bj) = - SItrBucket(i,j,bi,bj,iTr)
     &      *HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm*SEAICE_rhoIce
c note: at this point of the time step, that is the correct sign
#ifdef ALLOW_SALT_PLUME
c should work for both constant and variable ice salinity -- to be tested
          saltPlumeFlux(I,J,bi,bj) = MAX(ZERO,saltFlux(I,J,bi,bj))
     &      *SPsalFRAC*(salt(I,j,ks,bi,bj)-SItrFromOcean(i,j))
#endif
         ENDDO
        ENDDO
      endif

      DO J=1,sNy
       DO I=1,sNx
#ifdef ALLOW_SITRACER_DEBUG_DIAG
        DIAGarray(I,J,4+(iTr-1)*5) = - SItrBucket(i,j,bi,bj,iTr)
     &  *HEFFM(I,J,bi,bj)/SEAICE_deltaTtherm*SEAICE_rhoIce
#endif
c empty bucket
c  but not for 'grease' (see seaice_growth.F)
        if (SItrName(iTr).NE.'grease')
     &      SItrBucket(i,j,bi,bj,iTr)=0. _d 0
       ENDDO
      ENDDO

c TAF? elseif (SItrMate(iTr).EQ.'AREA') then

c 4) diagnostics
c ==============
#ifdef ALLOW_SITRACER_DEBUG_DIAG
      if (SItrMate(iTr).EQ.'HEFF') then
      DO J=1,sNy
       DO I=1,sNx
        HEFFpost=SItrHEFF(i,j,bi,bj,5)
        DIAGarray(I,J,1+(iTr-1)*5) = SItracer(i,j,bi,bj,iTr)
        DIAGarray(I,J,2+(iTr-1)*5) = SItracer(i,j,bi,bj,iTr)*HEFFpost
c DIAGarray(:,:,3) is the term of comparison for DIAGarray(:,:,2)
        if (SItrName(iTr).EQ.'salinity') then
          DIAGarray(I,J,3+(iTr-1)*5) = HSALT(i,j,bi,bj)/SEAICE_rhoIce
        elseif (SItrName(iTr).EQ.'one') then
          DIAGarray(I,J,3+(iTr-1)*5) = HEFFpost
        endif
c DIAGarray(:,:,4) allows check of conservation : del(SItrBucket)+del(SItr*HEFF)=0. over do_phys
c DIAGarray(:,:,5) is the tracer flux from the ocean (<0 incr. ocean tracer)
       ENDDO
      ENDDO
      else
      DO J=1,sNy
       DO I=1,sNx
        AREApost=SItrAREA(i,j,bi,bj,3)
        DIAGarray(I,J,1+(iTr-1)*5) = SItracer(i,j,bi,bj,iTr)
        DIAGarray(I,J,2+(iTr-1)*5) = SItracer(i,j,bi,bj,iTr)*AREApost
       ENDDO
      ENDDO
      endif
#endif
      ENDDO
#ifdef ALLOW_SITRACER_DEBUG_DIAG
c     CALL DIAGNOSTICS_FILL(DIAGarray,'UDIAG1  ',0,Nr,3,bi,bj,myThid)
#endif
      ENDDO
      ENDDO

#endif /* ALLOW_SITRACER */

      RETURN
      END
