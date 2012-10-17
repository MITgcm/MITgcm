C $Header: /u/gcmpack/MITgcm/pkg/layers/layers_init_varia.F,v 1.4 2012/10/17 18:49:15 rpa Exp $
C $Name:  $

#include "LAYERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE LAYERS_INIT_VARIA( myThid )

C ===================================================================
C     Initialize LAYERS variables.
C ===================================================================

      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C  INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid

#ifdef ALLOW_LAYERS

C     === Local variables ===
      INTEGER i,j,kg,bi,bj,iLa

      DO iLa=1,layers_maxNum
      
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Loop through 3D variables:
        DO kg=1,Nlayers
         DO j=1-Oly,sNy+OLy
          DO i=1-Olx,sNx+Olx

#ifdef LAYERS_UFLUX
           layers_UH(i,j,kg,bi,bj,iLa) = 0. _d 0
#ifdef LAYERS_THICKNESS
           layers_Hw(i,j,kg,bi,bj,iLa) = 0. _d 0
           layers_PIw(i,j,kg,bi,bj,iLa) = 0. _d 0
           layers_U(i,j,kg,bi,bj,iLa) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
           layers_VH(i,j,kg,bi,bj,iLa) = 0. _d 0
#ifdef LAYERS_THICKNESS
           layers_Hs(i,j,kg,bi,bj,iLa) = 0. _d 0
           layers_PIs(i,j,kg,bi,bj,iLa) = 0. _d 0
           layers_V(i,j,kg,bi,bj,iLa) = 0. _d 0
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */

          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_LAYERS_OUTPUT
#ifdef ALLOW_TIMEAVE
cgf layers_maxNum loop and dimension would be needed for
cgf the following and tave output to work beyond iLa.EQ.1
      IF ( iLa.EQ.1 ) THEN
C     Initialize averages to zero
        layers_TimeAve(bi,bj) = 0. _d 0

#ifdef LAYERS_UFLUX
        CALL TIMEAVE_RESET(layers_UH_T,Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
        CALL TIMEAVE_RESET(layers_Hw_T,Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_UFLUX */

#ifdef LAYERS_VFLUX
        CALL TIMEAVE_RESET(layers_VH_T,Nlayers,bi,bj,myThid)
#ifdef LAYERS_THICKNESS
        CALL TIMEAVE_RESET(layers_Hs_T,Nlayers,bi,bj,myThid)
#endif /* LAYERS_THICKNESS */
#endif /* LAYERS_VFLUX */
      ENDIF !IF ( iLa.EQ.1 ) THEN
#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_LAYERS_OUTPUT */

C- end bi,bj loops
       ENDDO
      ENDDO
      ENDDO !DO iLa=1,layers_maxNum

#endif /* ALLOW_LAYERS */

      RETURN
      END
