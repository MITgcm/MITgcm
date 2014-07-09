C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_add_tendency.F,v 1.5 2014/07/09 16:56:08 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_ADD_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_ADD_TENDENCY(
     U                     gPtracer,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     iTracer, myTime, myIter, myThid )

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
c#include "PTRACERS_FIELDS.h"
#include "GCHEM_FIELDS.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C  gPtracer             :: the tendency array
C  k                    :: vertical level
C  bi,bj                :: tile indices
C  iTracer              :: ptracer number
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL gPtracer   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax
      INTEGER k, bi, bj, iTracer
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
         gPtracer(i,j) = gPtracer(i,j)
     &                 + gchemTendency(i,j,k,bi,bj,iTracer)
       ENDDO
      ENDDO
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
