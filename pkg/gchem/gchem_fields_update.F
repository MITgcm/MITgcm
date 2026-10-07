#include "GCHEM_OPTIONS.h"

C !INTERFACE: ==========================================================
      SUBROUTINE GCHEM_FIELDS_UPDATE(
     I                      bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C     Update fields needed for BGC models (e.g. for fluxterms or pH calculation)
C       with value imported from the main model or other pkgs/components
C       after the main model has been updated.
C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "GCHEM.h"
#include "GCHEM_SIZE.h"
#include "GCHEM_FIELDS.h"
#include "GCHEM_AUX_FIELDS.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     bi, bj       :: tile indices
C     myTime       :: Current time in simulation
C     myIter       :: Current timestep number
C     myThid       :: my Thread Id number
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

c !LOCAL VARIABLES: ===================================================
      INTEGER i, j, k
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
#ifdef GCHEM_ALLOW_FFIELDS
C Update forcing fields in pkg/gchem common blocks
        k = 1
C------------------------------------------------------------
C If there is no ice file given in data.gchem, try fields from
C pkg/seaice/thsIce
      IF ( gchem_IceFile .EQ. ' ' ) THEN
#ifdef ALLOW_SEAICE
        IF ( useSEAICE ) THEN
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gchemIce(i,j,bi,bj) = AREA(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_THSICE
        IF ( useThSIce ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            gchemIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#endif
C endif empty gchem_IceFile       
      ENDIF

C If there is no SST file given in data.gchem, get fields from
C  the main model so we can initialize pH/pCO2 correctly.
      IF ( gchem_SSTFile .EQ. ' ' ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gchemSST(i,j,bi,bj) = theta(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF

C If there is no SSS file given in data.gchem, get fields from
C  the main model so we can initialize pH/pCO2 correctly.
      IF ( gchem_SSSFile .EQ. ' ' ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          gchemSSS(i,j,bi,bj) = salt(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF        
#endif /* GCHEM_ALLOW_FFIELDS */
      RETURN
      END
