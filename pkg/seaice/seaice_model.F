C $Header:

#include "SEAICE_OPTIONS.h"
 
CStartOfInterface
      SUBROUTINE seaice_model( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_MODEL                                  |
C     | o Time stepping of sea ice model.                        |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE
 
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "SEAICE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE_EXTERNAL.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif ALLOW_AUTODIFF_TAMC

#ifdef SEAICE_EXTERNAL_FORCING
#include "SEAICE_FFIELDS.h"
#endif SEAICE_EXTERNAL_FORCING

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SEAICE

C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj

#ifdef SEAICE_EXTERNAL_FORCING
C--   Atmospheric state and runoff are from
C     pkg/exf, which does not update edges.
         _EXCH_XY_R8( uwind,  myThid )
         _EXCH_XY_R8( vwind,  myThid )
         _EXCH_XY_R8( atemp,  myThid )
         _EXCH_XY_R8( aqh,    myThid )
         _EXCH_XY_R8( lwflux, myThid )
         _EXCH_XY_R8( precip, myThid )
         _EXCH_XY_R8( evap,   myThid )
         _EXCH_XY_R8( runoff, myThid )
#ifndef ALLOW_KPP
         _EXCH_XY_R8( swflux, mythid )
#endif ALLOW_KPP
#else SEAICE_EXTERNAL_FORCING
C--   Load atmospheric state and runoff.
      CALL SEAICE_GET_FORCING ( myTime, myIter, myThid )
#endif SEAICE_EXTERNAL_FORCING

C--   Third level model velocity is used as proxy for geostrophic velocity
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy+1
         DO i=0,sNx+1
          GWATX(I,J,bi,bj)=HALF*(uVel(i+1,j,3,bi,bj)
     &                         +uVel(i+1,j+1,3,bi,bj))
          GWATY(I,J,bi,bj)=HALF*(vVel(i,j+1,3,bi,bj)
     &                         +vVel(i+1,j+1,3,bi,bj))
#ifdef SEAICE_DEBUG
c          write(*,'(2i4,2i2,f7.1,7f12.3)')
c     &     ,i,j,bi,bj,UVM(I,J,bi,bj)
c     &     ,GWATX(I,J,bi,bj),GWATY(I,J,bi,bj)
c     &     ,uVel(i+1,j,3,bi,bj),uVel(i+1,j+1,3,bi,bj)
c     &     ,vVel(i,j+1,3,bi,bj),vVel(i+1,j+1,3,bi,bj)
#endif SEAICE_DEBUG
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C solve ice momentum equations and calculate ocean surface stress
      CALL DYNSOLVER ( myTime, myIter, myThid )

C NOW DO ADVECTION
      CALL ADVECT( UICE, VICE, HEFF, HEFFM, myThid )
      CALL ADVECT( UICE, VICE, AREA, HEFFM, myThid )

C NOW DO GROWTH
C     MUST CALL GROWTH ONLY AFTER CALLING ADVECTION
      CALL GROWTH( myTime, myIter, myThid)

C--   Update overlap regions for a bunch of stuff
       _BARRIER
       CALL EXCH_RL( HEFF, OLx, OLx, OLy, OLy, 3, OLx, OLy,
     I      FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       CALL EXCH_RL( AREA, OLx, OLx, OLy, OLy, 3, OLx, OLy,
     I      FORWARD_SIMULATION, EXCH_UPDATE_CORNERS, myThid )
       _EXCH_XY_R4(fu   , myThid )
       _EXCH_XY_R4(fv   , myThid )
       _EXCH_XY_R4(EmPmR, myThid )
       _EXCH_XY_R4(Qnet , myThid )
       _EXCH_XY_R4(surfaceTendencyTice, myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw  , myThid )
#endif SHORTWAVE_HEATING
       _EXCH_XYZ_R8(theta , myThid )

C--   Sea ice diagnostics.
      CALL SEAICE_DO_DIAGS( myTime, myIter, myThid )

C--   Write sea ice restart files
      CALL SEAICE_WRITE_PICKUP ( .FALSE.,
     &     myTime+deltaTClock, myIter+1, myThid )

C---------------------------------------------------
C OOH NOOOO we need to move the whole stuff
C---------------------------------------------------
#ifdef ALLOW_AUTODIFF_TAMC
CRG CADJ store UICE,VICE,AREA,HEFF,fu,fv,EmPmR,Qnet,Qsw = comlev1_bibj
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Call sea-ice cost function routine
CRG      CALL SEAICE_COST( myTime, myIter, myThid )

#endif ALLOW_SEAICE

      RETURN
      END
