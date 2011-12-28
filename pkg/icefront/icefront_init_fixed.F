C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_init_fixed.F,v 1.5 2011/12/28 20:45:05 jmc Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"
#undef ALLOW_ICEFRONT_DEBUG

      SUBROUTINE ICEFRONT_INIT_FIXED( myThid )
C     *============================================================*
C     | SUBROUTINE ICEFRONT_INIT_FIXED
C     | o Routine to initialize ICEFRONT parameters and variables.
C     *============================================================*
C     | Initialize ICEFRONT parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEFRONT.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_FIXED
      INTEGER myThid

#ifdef ALLOW_ICEFRONT
C     === Local variables ===
C     I,J,K,bi,bj - Loop counters
      INTEGER I, J, K, bi, bj
#ifdef ALLOW_ICEFRONT_DEBUG
      INTEGER ISinterface
#endif

      IF ( ICEFRONTlengthFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( ICEFRONTlengthFile, ' ',
     &      icefrontlength, 0, myThid )
       _EXCH_XY_RS( icefrontlength, myThid )
      ENDIF

      IF ( ICEFRONTdepthFile .NE. ' ' ) THEN
       _BARRIER
       CALL READ_FLD_XY_RS( ICEFRONTdepthFile, ' ',
     &      R_icefront, 0, myThid )
       _EXCH_XY_RS( R_icefront, myThid )
      ENDIF

C     Make sure that R_icefront is positive
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          R_icefront(I,J,bi,bj) = ABS(R_icefront(I,J,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1-OLy, sNy+OLy
         DO I = 1-OLx, sNx+OLx
          K_icefront(i,j,bi,bj) = 0
          DO K = 1 , Nr
           IF ( R_icefront(I,J,bi,bj) .GT. ABS(rF(K)))
     &          K_icefront(I,J,bi,bj) = K
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_ICEFRONT_DEBUG
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1, sNy
          DO I = 1, sNx
C           IsInterface=0
           IF (ICEFRONTlength(I,J,bi,bj) .GT. 0. _d 0) THEN
C              print*, 'IsInterface=', '2' , ',xuyun'
              IsInterface=Isinterface + K_icefront(I,J,bi,bj) 
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       print*, 'Interface # =', IsInterface
#endif /* ALLOW_ICEFRONT_DEBUG */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL ICEFRONT_DIAGNOSTICS_INIT(myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ICEFRONT */

      RETURN
      END
