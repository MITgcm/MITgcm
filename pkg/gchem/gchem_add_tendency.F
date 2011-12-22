C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_add_tendency.F,v 1.4 2011/12/22 19:05:27 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_ADD_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_ADD_TENDENCY(bi,bj,iMin,iMax,jMin,jMax,k,
     &                           iTracer, myTime,myIter, myThid )

C !DESCRIPTION:
C     In the case of GCHEM_SEPARATE_FORCING undefined, GCHEM_ADD_TENDENCY
C     will update passive tracer tendencies gPtr with gchemTendency,
C     the latter having been computed in GCHEM_CALC_TENDENDY, so that
C     they will be incorporated into regular timestepping in
C     PTRACERS_INTERGRATE.
C     IF GCHEM_SEPARATE_FORCING is defined, this routine is empty.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#include "GCHEM_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  bi,bj                :: tile indices
C  k                    :: vertical level
C  iTracer              :: ptracer number
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k
      INTEGER iTracer
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_GCHEM
# ifndef GCHEM_SEPARATE_FORCING
C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j

C     apply the tendencies computed in GCHEM_CALC_TENDENCY to the passive
C     tracers tendendy terms. The part of the code could also go into
C     ptracers_forcing and replace the call to gchem_add_tendency there,
C     but this way, no gchem-related header files and CPP-flags are required
C     within ptracers_forcing.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gPtr(i,j,k,bi,bj,iTracer) = gPtr(i,j,k,bi,bj,iTracer)
     &       + gchemTendency(i,j,k,bi,bj,iTracer)
       ENDDO
      ENDDO
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
