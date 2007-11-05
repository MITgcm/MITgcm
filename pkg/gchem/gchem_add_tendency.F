C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_add_tendency.F,v 1.3 2007/11/05 19:05:01 jmc Exp $
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
c#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "GCHEM_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  iTracer              :: ptracer number
C  bi,bj                :: tile indices
C  k                    :: vertical level
      INTEGER myThid, myIter
      _RL myTime
      INTEGER iTracer
      INTEGER bi,bj,imin,imax,jmin,jmax,k

C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
      INTEGER i,j
      INTEGER niter
CEOP

#ifdef ALLOW_GCHEM
# ifndef GCHEM_SEPARATE_FORCING
C     apply the tendencies computed in GCHEM_CALC_TENDENCY to the passive
C     tracers tendendy terms. The part of the code could also go into
C     ptracers_forcing and replace the call to gchem_add_tendency there,
C     but this way, no gchem-related header files and CPP-flags are required
C     within ptracers_forcing.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gPtr(I,J,K,bi,bj,iTracer) = gPtr(I,J,K,bi,bj,iTracer)
     &       + gchemTendency(I,J,K,bi,bj,iTracer)
       ENDDO
      ENDDO
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
