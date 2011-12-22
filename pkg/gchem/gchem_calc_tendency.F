C $Header: /u/gcmpack/MITgcm/pkg/gchem/gchem_calc_tendency.F,v 1.4 2011/12/22 19:05:27 jmc Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_CALC_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_CALC_TENDENCY(
     I     myTime, myIter, myThid )

C !DESCRIPTION:
C In the case of GCHEM_SEPARATE_FORCING not defined,
C this subroutine computes the tracer tendencies due to a
C bio-geogchemistry or ecosystem model and stores them on an array
C gchemTendency, that will be incorporated into regular timestepping in
C in ptracers_intergrate.F
C The current example uses the CFC package, but his is meant to
C be replaced by anything that the user provides.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "GCHEM.h"
#include "GCHEM_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef ALLOW_GCHEM
# ifndef GCHEM_SEPARATE_FORCING
C !LOCAL VARIABLES: ====================================================
C  i,j                  :: loop indices
C  iTracer              :: ptracer number
C  bi,bj                :: tile indices
C  k                    :: vertical level
      INTEGER i,j
      INTEGER iTracer
      INTEGER bi,bj,iMin,iMax,jMin,jMax,k

C     gchemTendency is re-initialized here
      DO iTracer = 1, PTRACERS_numInUse
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k = 1, Nr
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
            gchemTendency(i,j,k,bi,bj,iTracer) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
C     Here is the place for code to compute bio-geochemical
C     tendency terms (sometimes referred to as source-minus-sink
C     terms). The tendencies are stored on gchemTendency, as show
C     in the CFC-example.
C
C     loop over tiles
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     define horizontal loop ranges
        iMin = 1
        iMax = sNx
        jMin = 1
        jMax = sNy
C        DO iTracer = 1, PTRACER_numInUse
C         DO k = 1, Nr
C          DO j = 1-OLy, sNy+OLy
C           DO i = 1-OLx, sNx+OLx
C            gchemTendency(i,j,k,bi,bj,iTracer) = your specific model
C           ENDDO
C          ENDDO
C         ENDDO
C        ENDDO

#ifdef ALLOW_CFC
ccccccccccccccccccccccccc
c chemical forcing      c
ccccccccccccccccccccccccc
ccccccccccccccccccccccccccc CFC cccccccccccccccccccccccccccccccc
c
        k = 1
        CALL CFC11_FORCING( pTracer     (1-OLx,1-OLy,1,bi,bj,1),
     &                      gchemTendency(1-OLx,1-OLy,1,bi,bj,1),
     &                      bi, bj, iMin, iMax, jMin, jMax,
     &                      myIter, myTime, myThid )
        CALL CFC12_FORCING( pTracer     (1-OLx,1-OLy,1,bi,bj,2),
     &                      gchemTendency(1-OLx,1-OLy,1,bi,bj,2),
     &                      bi, bj, iMin, iMax, jMin, jMax,
     &                      myIter, myTime, myThid )

cccccccccccccccccccccccccc END CFC cccccccccccccccccccccccccccccccccc
#endif /* ALLOW_CFC */
C     end of tile-loops
       ENDDO
      ENDDO
# endif /* GCHEM_SEPARATE_FORCING */
#endif /* ALLOW_GCHEM */

      RETURN
      END
