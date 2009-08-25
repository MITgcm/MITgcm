C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_init_varia.F,v 1.3 2009/08/25 21:33:28 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C     !ROUTINE: GAD_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE GAD_INIT_VARIA( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GAD_INIT_VARIA
C     | o Routine to initialize GAD variables
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "GRID.h"
#include "GAD.h"
#include "GAD_SOM_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef GAD_ALLOW_TS_SOM_ADV
C     !LOCAL VARIABLES:
C     === Local variables ===
      INTEGER i,j,k,bi,bj
      INTEGER n

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--   Initialize 1rst Order (3:x,y,z) and 2nd Order (6:xx,yy,zz,xy,xz,yz) moments
        DO n=1,nSOM
         DO k=1,Nr
          DO j=1-Oly,sNy+OLy
           DO i=1-Olx,sNx+Olx
C       pot.temp field:
            som_T(i,j,k,bi,bj,n) = 0. _d 0
C       salinity field:
            som_S(i,j,k,bi,bj,n) = 0. _d 0
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C- end bi,bj loops
       ENDDO
      ENDDO

      IF ( tempSOM_Advection .OR. saltSOM_Advection ) THEN
       IF ( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &                      .AND. pickupSuff.EQ.' ' ) THEN
C-     Need to synchronize here before doing master-thread IO
c        _BARRIER
C-     Read-in initial conditions:
C        --- not yet coded ---
c        CALL GAD_SOM_EXCHANGES( myThid )
       ELSE
C-     Read 2nd-O moments state from pickup files
         CALL GAD_READ_PICKUP( nIter0, myThid )
         CALL GAD_SOM_EXCHANGES( myThid )
       ENDIF
      ENDIF

#endif /* GAD_ALLOW_TS_SOM_ADV */

      RETURN
      END
