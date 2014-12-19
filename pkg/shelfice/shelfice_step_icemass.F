C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_step_icemass.F,v 1.1 2014/12/19 18:09:33 dgoldberg Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_U_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_STEP_ICEMASS(
     I                        myTime, myIter, myThid )

!        myTime, myIter, myThid
     
C !DESCRIPTION:
C Serves as a "stub" for ice dynamics
C will later be used to 

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
!#include "STREAMICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP
      
      

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C  i,j                  :: loop indices
      INTEGER bi,bj,k,i,j

      print *, "STEP MASS 1", shelficemass(25,25,1,1)

      IF (SHELFICEallowThinIceMass) then
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx

           shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj) +
     &      shelfIceFreshWaterFlux(I,J,bi,bj) * deltaT

           shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj) +
     &      shelficeMassDynamics(I,J,bi,bj) * 910. * deltaT
!          FOR NOW THE DENSITY OF ICE IS HARDCODED
!          SHOULD USE PARAM FOR ICE DENSITY FROM STREAMICE
!          -- but this is just a placeholder for streamice anyway          

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
     
      _EXCH_XY_RS(shelficeMass, myThid )      

      print *, "STEP MASS 2",shelficemass(25,25,1,1)

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL(shelficemass,'SHI_mass',0,1,0,1,1,
     &      myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
