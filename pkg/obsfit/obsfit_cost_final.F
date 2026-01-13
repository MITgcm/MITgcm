#include "OBSFIT_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OBSFIT_COST_FINAL
C     !INTERFACE:
      SUBROUTINE OBSFIT_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE OBSFIT_COST_FINAL
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
#ifdef ALLOW_CTRL
# include "OPTIMCYCLE.h"
#endif
#include "OBSFIT_SIZE.h"
#include "OBSFIT.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#ifdef ALLOW_COST
C     ! FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER num_file_obs
      _RL f_obsfit(NFILESMAX_OBS)
      _RL no_obsfit(NFILESMAX_OBS)

      INTEGER ifc
      CHARACTER*23 cfname
      INTEGER IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      ifc = 30

c--   Each process has calculated the global part for itself.
      DO num_file_obs=1,NFILESMAX_OBS
        glofc = glofc + mult_obsfit(num_file_obs)
     &                 *objf_obsfit(num_file_obs)
      ENDDO

C     This is just a copy since obsfit does not have the tile logic of
C     other part of the code
      DO num_file_obs=1,NFILESMAX_OBS
       f_obsfit(num_file_obs) = objf_obsfit(num_file_obs)
       no_obsfit(num_file_obs) = num_obsfit(num_file_obs)
      ENDDO

CML I do not understand, why we do not need this or something like this here
CMLc--   Do global summation for each part of the cost function
CML      DO num_var=1,NVARMAX
CML       DO num_file=1,NFILESPROFMAX
CML        _GLOBAL_SUM_RL(f_obsfit(num_file_obs), myThid )
CML        _GLOBAL_SUM_RL(no_obsfit(num_file_obs), myThid )
CML       ENDDO
CML      ENDDO

C     start printing to STDOUT
      DO num_file_obs=1,NFILESMAX_OBS
       IF ( no_obsfit(num_file_obs).GT.zeroRL ) THEN
        WRITE(msgBuf,'(A,D22.15,i2.0)')
     &       ' --> f_obsfit =', f_obsfit(num_file_obs), num_file_obs
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO

c--   Each process has calculated the global part for itself.

C     only master thread of master CPU open and write to file
      IF ( MASTER_CPU_THREAD(myThid)
     &     .AND. obsfitWriteCostFunction ) THEN
       WRITE(cfname,'(A,I4.4)') 'costfunction_obsfit',optimcycle
       OPEN(unit=ifc,file=cfname)
       WRITE(msgBuf,'(A,A)')
     &      'Writing obsfit cost function info to ', cfname
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

       DO num_file_obs=1,NFILESMAX_OBS
        IF ( no_obsfit(num_file_obs).GT.zeroRL ) THEN
         IL = ILNBLNK( obsfitfiles(num_file_obs) )
         IL = MAX(IL,30)
         WRITE(ifc,'(4A,2D22.15)')
     &        obsfitfiles(num_file_obs)(1:IL), ' ', obsfit_nameval,
     &        ' = ', f_obsfit(num_file_obs), no_obsfit(num_file_obs)
        ENDIF
       ENDDO
       CLOSE(ifc)
C     Do it only once per run, otherwise grdchk will overwrite or append
C     files in an uncontrolled way.
       obsfitWriteCostFunction = .FALSE.

C     MASTER_CPU_THREAD .AND. obsfitWriteCostFunction
      ENDIF

#endif /* ALLOW_COST */

      RETURN
      END
