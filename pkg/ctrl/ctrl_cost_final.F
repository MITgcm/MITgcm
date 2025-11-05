#include "CTRL_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: CTRL_COST_FINAL
C     !INTERFACE:
      SUBROUTINE CTRL_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE CTRL_COST_FINAL
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_COST
# include "cost.h"
#endif
#include "CTRL_SIZE.h"
#include "CTRL.h"
#include "CTRL_GENARR.h"
#include "OPTIMCYCLE.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#if defined ALLOW_COST && ( defined ALLOW_GENTIM2D_CONTROL || \
                            defined ALLOW_GENARR2D_CONTROL || \
                            defined ALLOW_GENARR3D_CONTROL )
C     ! FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER bi, bj, num_var
# ifdef ALLOW_GENTIM2D_CONTROL
      _RL f_gentim2d(maxCtrlTim2D)
      _RL no_gentim2d(maxCtrlTim2D)
# endif
# ifdef ALLOW_GENARR2D_CONTROL
      _RL f_genarr2d(maxCtrlArr2D)
      _RL no_genarr2d(maxCtrlArr2D)
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      _RL f_genarr3d(maxCtrlArr3D)
      _RL no_genarr3d(maxCtrlArr3D)
# endif

      INTEGER ifc
      CHARACTER*22 cfname
      INTEGER IL
      LOGICAL cfexist
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      ifc = 30

C     Initialise cost function variables zo zero
# ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       f_gentim2d(num_var)= 0. _d 0
       no_gentim2d(num_var)= 0. _d 0
      ENDDO
# endif
# ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       f_genarr2d(num_var)= 0. _d 0
       no_genarr2d(num_var)= 0. _d 0
      ENDDO
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       f_genarr3d(num_var)= 0. _d 0
       no_genarr3d(num_var)= 0. _d 0
      ENDDO
# endif

# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('CTRL_COST_DRIVER',myThid)
# endif
      CALL TIMER_START('CTRL_COST_DRIVER [COST_FINAL]', myThid)
      CALL CTRL_COST_DRIVER( myThid )
      CALL TIMER_STOP ('CTRL_COST_DRIVER [COST_FINAL]', myThid)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
# ifdef ALLOW_GENTIM2D_CONTROL
        DO num_var=1,maxCtrlTim2D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &        + mult_gentim2d(num_var)
     &        *objf_gentim2d(bi,bj,num_var)
         f_gentim2d(num_var) = f_gentim2d(num_var)
     &        +objf_gentim2d(bi,bj,num_var)
         no_gentim2d(num_var) = no_gentim2d(num_var)
     &        +num_gentim2d(bi,bj,num_var)
        ENDDO
# endif
# ifdef ALLOW_GENARR2D_CONTROL
        DO num_var=1,maxCtrlArr2D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &        + mult_genarr2d(num_var)
     &        *objf_genarr2d(bi,bj,num_var)
         f_genarr2d(num_var) = f_genarr2d(num_var)
     &        +objf_genarr2d(bi,bj,num_var)
         no_genarr2d(num_var) = no_genarr2d(num_var)
     &        +num_genarr2d(bi,bj,num_var)
        ENDDO
# endif
# ifdef ALLOW_GENARR3D_CONTROL
        DO num_var=1,maxCtrlArr3D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &        + mult_genarr3d(num_var)
     &        *objf_genarr3d(bi,bj,num_var)
         f_genarr3d(num_var) = f_genarr3d(num_var)
     &        +objf_genarr3d(bi,bj,num_var)
         no_genarr3d(num_var) = no_genarr3d(num_var)
     &        +num_genarr3d(bi,bj,num_var)
        ENDDO
# endif
       ENDDO
      ENDDO

# ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       _GLOBAL_SUM_RL(f_gentim2d(num_var), myThid )
       _GLOBAL_SUM_RL(no_gentim2d(num_var), myThid )
       IF (no_gentim2d(num_var).GT.0. _d 0) THEN
        IL  = ILNBLNK( xx_gentim2d_file(num_var) )
        WRITE(msgBuf,'(A,D22.15,I2.0,1X,3A)')
     &     ' --> f_gentim2d =',f_gentim2d(num_var),
     &      num_var,'(',xx_gentim2d_file(num_var)(1:IL),')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
# endif
# ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       _GLOBAL_SUM_RL(f_genarr2d(num_var), myThid )
       _GLOBAL_SUM_RL(no_genarr2d(num_var), myThid )
       IF (no_genarr2d(num_var).GT.0. _d 0) THEN
        IL  = ILNBLNK( xx_genarr2d_file(num_var) )
        WRITE(msgBuf,'(A,D22.15,I2.0,1X,3A)')
     &     ' --> f_genarr2d =',f_genarr2d(num_var),
     &      num_var,'(',xx_genarr2d_file(num_var)(1:IL),')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
# endif
# ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       _GLOBAL_SUM_RL(f_genarr3d(num_var), myThid )
       _GLOBAL_SUM_RL(no_genarr3d(num_var), myThid )
       IF (no_genarr3d(num_var).GT.0. _d 0) THEN
        IL  = ILNBLNK( xx_genarr3d_file(num_var) )
        WRITE(msgBuf,'(A,D22.15,I2.0,1X,3A)')
     &     ' --> f_genarr3d =',f_genarr3d(num_var),
     &      num_var,'(',xx_genarr3d_file(num_var)(1:IL),')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
# endif

C     write contributions to costfunction file

C--   Each process has calculated the global part for itself.
      IF ( MASTER_CPU_THREAD(myThid) .AND. ctrlWriteCostFunction ) THEN
       cfexist = .FALSE.
       WRITE(cfname,'(A,I4.4)') 'costfunction',optimcycle
       INQUIRE(FILE=cfname(1:16), EXIST=cfexist )
       IF ( cfexist ) THEN
        OPEN(unit=ifc,file=cfname(1:16),ACCESS='append',STATUS='old')
       ELSE
        WRITE(cfname,'(A,I4.4)') 'costfunction_ctrl',optimcycle
        OPEN(unit=ifc,file=cfname)
       ENDIF
       WRITE(msgBuf,'(A,A)') 'Writing cost function info to ', cfname
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

# ifdef ALLOW_GENTIM2D_CONTROL
       DO num_var=1,maxCtrlTim2D
        IF (no_gentim2d(num_var).GT.0. _d 0) THEN
         IL  = ILNBLNK( xx_gentim2d_file(num_var) )
         IL  = max (IL,15)
         WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &        xx_gentim2d_file(num_var)(1:IL),
     &        ' (gentim2d ', num_var, ') = ',
     &        f_gentim2d(num_var),
     &        no_gentim2d(num_var)
        ENDIF
       ENDDO
# endif

# ifdef ALLOW_GENARR2D_CONTROL
       DO num_var=1,maxCtrlArr2D
        IF (no_genarr2d(num_var).GT.0. _d 0) THEN
         IL  = ILNBLNK( xx_genarr2d_file(num_var) )
         IL  = max (IL,15)
         WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &        xx_genarr2d_file(num_var)(1:IL),
     &        ' (genarr2d ', num_var, ') = ',
     &        f_genarr2d(num_var),
     &        no_genarr2d(num_var)
        ENDIF
       ENDDO
# endif

# ifdef ALLOW_GENARR3D_CONTROL
       DO num_var=1,maxCtrlArr3D
        IF (no_genarr3d(num_var).GT.0. _d 0) THEN
         IL  = ILNBLNK( xx_genarr3d_file(num_var) )
         IL  = max (IL,15)
         WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &        xx_genarr3d_file(num_var)(1:IL),
     &        ' (genarr3d ', num_var, ') = ',
     &        f_genarr3d(num_var),
     &        no_genarr3d(num_var)
        ENDIF
       ENDDO
# endif
       CLOSE(ifc)
C     Do it only once per run, otherwise grdchk will overwrite or append
C     files in an uncontrolled way.
       ctrlWriteCostFunction = .FALSE.
      ENDIF

#endif /* ALLOW_COST */

      RETURN
      END
