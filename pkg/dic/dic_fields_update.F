#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"
CBOP
C !ROUTINE: DIC_FIELDS_UPDATE

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_FIELDS_UPDATE(
     I                      bi, bj, myTime, myIter, myThid )

C !DESCRIPTION:
C     Update fields (needed for fluxterms or pH calculation)
C       with value imported from other pkgs or components

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif
#ifdef ALLOW_OCN_COMPON_INTERF
# include "CPL_PARAMS.h"
# include "OCNCPL.h"
#endif /* ALLOW_OCN_COMPON_INTERF */

C !INPUT PARAMETERS: ===================================================
C     bi, bj       :: tile indices
C     myTime       :: Current time in simulation
C     myIter       :: Current timestep number
C     myThid       :: my Thread Id number
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC

c !LOCAL VARIABLES: ===================================================
#if defined(ALLOW_THSICE) || defined(ALLOW_SEAICE) || defined(COMPONENT_MODULE)
      INTEGER i, j
#endif
c     CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
#ifndef GCHEM_ALLOW_FFIELDS
C     Update forcing fields in pkg/dic common blocks
        IF ( useThSIce ) THEN
#ifdef ALLOW_THSICE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
           ENDDO
          ENDDO
#endif /* ALLOW_THSICE */
        ELSEIF ( useSEAICE ) THEN
#ifdef ALLOW_SEAICE
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = AREA(i,j,bi,bj)
           ENDDO
          ENDDO
#endif /* ALLOW_SEAICE */
        ELSEIF ( useCoupler ) THEN
#ifdef ALLOW_OCN_COMPON_INTERF
         IF ( useImportFice ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             fIce(i,j,bi,bj) = sIceFrac_cpl(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif /* ALLOW_OCN_COMPON_INTERF */
        ENDIF

#ifdef ALLOW_OCN_COMPON_INTERF
        IF ( useCoupler ) THEN
         IF ( useImportCO2 ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             AtmospCO2(i,j,bi,bj) = airCO2(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportWSpd ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             wind(i,j,bi,bj) = surfWSpeed(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
        ENDIF
#endif /* ALLOW_OCN_COMPON_INTERF */
#endif /* GCHEM_ALLOW_FFIELDS */      
#endif /* ALLOW_DIC */
      RETURN
      END
