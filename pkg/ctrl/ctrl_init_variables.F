#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_INIT_VARIABLES
C     !INTERFACE:
      SUBROUTINE CTRL_INIT_VARIABLES( myThid )
C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE CTRL_INIT_VARIABLES
c     ==================================================================
c
c     o Set parts of the vector of control variables and initialize the
c       rest to zero.
c
c     started: heimbach@mit.edu 25-Mar-2002
c
c     ==================================================================
c     SUBROUTINE CTRL_INIT_VARIABLES
c     ==================================================================
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "CTRL_FIELDS.h"
# include "CTRL_GENARR.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef ALLOW_CTRL

C     !LOCAL VARIABLES:
C     == Local variables
      INTEGER bi,bj
      INTEGER k
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      INTEGER i,j
#endif
c     == end of interface ==
CEOP

#ifdef ALLOW_BOTTOMDRAG_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              bottomDragFld(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_OBCS_CONTROL
      IF (useOBCS) CALL CTRL_INIT_OBCS_VARIABLES( myThid )
#endif

C--   Map the control variables onto the model state.
C-    Needs to be called after few {PKG}_init_varia (e.g., PTRACERS_INIT_VARIA)
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL))
      CALL CTRL_MAP_INI_GENARR( myThid )
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      CALL CTRL_MAP_INI_GENTIM2D( myThid )
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k=1,maxCtrlTim2D
            objf_gentim2d(bi,bj,k)     = 0. _d 0
            num_gentim2d(bi,bj,k)      = 0. _d 0
          ENDDO
        ENDDO
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k=1,maxCtrlArr2D
            objf_genarr2d(bi,bj,k)    = 0. _d 0
            num_genarr2d(bi,bj,k)    = 0. _d 0
          ENDDO
        ENDDO
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO bj = myByLo(myThid),myByHi(myThid)
        DO bi = myBxLo(myThid),myBxHi(myThid)
          DO k=1,maxCtrlArr3D
            objf_genarr3d(bi,bj,k)    = 0. _d 0
            num_genarr3d(bi,bj,k)    = 0. _d 0
          ENDDO
        ENDDO
      ENDDO
#endif

#ifdef ALLOW_SEAICE
      CALL SEAICE_CTRL_MAP_INI( myThid )
#endif

#endif /* ALLOW_CTRL */

      RETURN
      END
