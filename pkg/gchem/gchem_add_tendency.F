C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_add_tendency.F,v 1.1 2004/11/28 23:48:31 mlosch Exp $
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
#include "PTRACERS.h"
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
CML We will keep the old way of calling CFC??_FORCING for now, but eventually
CML this can be deleted.
CML#  ifdef ALLOW_CFC
CMLccccccccccccccccccccccccc
CMLc chemical forcing      c
CMLccccccccccccccccccccccccc
CMLccccccccccccccccccccccccccc CFC cccccccccccccccccccccccccccccccc
CMLc
CML         IF (iTracer.EQ.1.AND.k.EQ.1) THEN
CML     &     call CFC11_Forcing( Ptracer(1-Olx,1-Oly,1,bi,bj,1),
CML     &                          gPtr(1-Olx,1-Oly,1,bi,bj,1),
CML     &                          bi,bj,imin,imax,jmin,jmax,
CML     &                          myIter,myTime,myThid)
CML        IF (iTracer.EQ.2.AND.k.EQ.1) 
CML     &    call CFC12_Forcing( Ptracer(1-Olx,1-Oly,1,bi,bj,2),
CML     &                          gPtr(1-Olx,1-Oly,1,bi,bj,2),
CML     &                          bi,bj,imin,imax,jmin,jmax,
CML     &                          myIter,myTime,myThid)
CMLcccccccccccccccccccccccccc END CFC cccccccccccccccccccccccccccccccccc
CMLc
CML#  endif /* ALLOW_CFC */
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
