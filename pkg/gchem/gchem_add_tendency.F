#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: GCHEM_ADD_TENDENCY

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_ADD_TENDENCY(
     U                     gPtracer,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     iTr, myTime, myIter, myThid )

C !DESCRIPTION:
C     In the case of a tracer not using GCHEM_SEPARATE_FORCING,
C     GCHEM_ADD_TENDENCY will update passive tracer tendencies gPtr with
C     gchemTendency, the latter having been computed in GCHEM_CALC_TENDENDY,
C     so that they will be incorporated into regular timestepping in
C     PTRACERS_INTERGRATE.

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GCHEM.h"
#include "GCHEM_SIZE.h"
#include "GCHEM_FIELDS.h"

C !INPUT/OUTPUT PARAMETERS: ============================================
C  gPtracer     :: the tendency array
C  k            :: vertical level
C  bi,bj        :: tile indices
C  iTr          :: ptracer number
C  myTime       :: current time
C  myIter       :: current timestep
C  myThid       :: thread number
      _RL gPtracer   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin,iMax,jMin,jMax
      INTEGER k, bi, bj, iTr
      _RL myTime
      INTEGER myIter, myThid
CEOP

#ifdef GCHEM_ADD2TR_TENDENCY
C !LOCAL VARIABLES: ====================================================
C  jTr          :: number of corresponding tracer tendency (in gchemTendency)
C  i,j          :: loop indices
      INTEGER jTr
      INTEGER i,j

C     Apply the tendencies computed in GCHEM_CALC_TENDENCY to the passive
C     tracers tendendy terms. The part of the code could also go into
C     ptracers_forcing and replace the call to gchem_add_tendency there,
C     but this way, no gchem-related header files and CPP-flags are required
C     within ptracers_forcing.
      IF ( iTr.GT.gchem_sepFTr_num .AND.
     &     iTr.LE.gchem_Tracer_num ) THEN
       jTr = iTr - gchem_sepFTr_num
       DO j=jMin,jMax
        DO i=iMin,iMax
         gPtracer(i,j) = gPtracer(i,j)
     &                 + gchemTendency(i,j,k,bi,bj,jTr)
        ENDDO
       ENDDO
      ENDIF
#endif /* GCHEM_ADD2TR_TENDENCY */

      RETURN
      END
