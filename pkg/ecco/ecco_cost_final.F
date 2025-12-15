#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE ECCO_COST_FINAL( myThid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      IMPLICIT NONE

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#ifdef ALLOW_COST
# include "cost.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif

c     == routine arguments ==

      INTEGER myThid

C     === Functions ====
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == local variables ==

      INTEGER bi,bj
      INTEGER ifc
      INTEGER num_var

#ifndef ALLOW_OBSFIT
      INTEGER NFILESMAX_OBS
      PARAMETER ( NFILESMAX_OBS=1 )
#endif

      _RL locfc
      _RL f_gencost(NGENCOST)
      _RL no_gencost(NGENCOST)
#ifdef ALLOW_OBSFIT
      INTEGER num_file_obs
      _RL f_obsfit(NFILESMAX_OBS)
      _RL no_obsfit(NFILESMAX_OBS)
#endif
      INTEGER IL
      CHARACTER*22 cfname
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

      ifc = 30

      locfc = 0. _d 0

#ifdef ALLOW_OBSFIT
      DO num_file_obs=1,NFILESMAX_OBS
       f_obsfit(num_file_obs) = objf_obsfit(num_file_obs)
       no_obsfit(num_file_obs) = num_obsfit(num_file_obs)
      ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       f_gencost(num_var) = 0. _d 0
       no_gencost(num_var)= 0. _d 0
      ENDDO
#endif

c--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_GENCOST_CONTRIBUTION
        DO num_var=1,NGENCOST
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gencost(num_var)
     &            *objf_gencost(bi,bj,num_var)
         f_gencost(num_var)=f_gencost(num_var)
     &            +objf_gencost(bi,bj,num_var)
         no_gencost(num_var)=no_gencost(num_var)
     &            +num_gencost(bi,bj,num_var)
        ENDDO
#endif

C-   end bi,bj loops
       ENDDO
      ENDDO

c local copy used in print statements, for
c which we always want to do the global sum.
      CALL GLOBAL_SUM_TILE_RL( tile_fc, locfc, myThid )

c--   Do global summation for each part of the cost function
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       _GLOBAL_SUM_RL(f_gencost(num_var), myThid )
       _GLOBAL_SUM_RL(no_gencost(num_var), myThid )
      ENDDO
#endif

C     start printing to STDOUT
#ifdef ALLOW_OBSFIT
      IF ( useOBSFIT ) THEN
       DO num_file_obs=1,NFILESMAX_OBS
        IF ( no_obsfit(num_file_obs).GT.zeroRL ) THEN
         WRITE(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_obsfit =', f_obsfit(num_file_obs), num_file_obs
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDDO
      ENDIF
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       IF (no_gencost(num_var).GT.0) THEN
        IL  = ILNBLNK( gencost_name(num_var) )
        WRITE(msgBuf,'(A,D22.15,i2.0,3A)')
     &     ' --> f_gencost  =',f_gencost(num_var),
     &     num_var, ' (', gencost_name(num_var)(1:IL), ')'
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
#endif

c--   Each process has calculated the global part for itself.

#ifdef ALLOW_OBSFIT
      DO num_file_obs=1,NFILESMAX_OBS
        glofc = glofc
     &          +  mult_obsfit(num_file_obs)
     &            *objf_obsfit(num_file_obs)
      ENDDO
#endif

      locfc=locfc+glofc

C     only master thread of master CPU open and write to file
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

       WRITE(cfname,'(A,i4.4)') 'costfunction_ecco',eccoiter
       IF ( eccoWriteCostFunction ) THEN
        WRITE(msgBuf,'(A,A)')
     &           'Writing ecco cost function info to ', cfname
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        open(unit=ifc,file=cfname)

#ifdef ALLOW_OBSFIT
        IF ( useOBSFIT ) THEN
         DO num_file_obs=1,NFILESMAX_OBS
          IF ( no_obsfit(num_file_obs).GT.zeroRL ) THEN
           IL  = ILNBLNK( obsfitfiles(num_file_obs) )
           IL  = max (IL,30)
           WRITE(ifc,'(4A,2D22.15)')
     &      obsfitfiles(num_file_obs)(1:IL),' ',
     &      obsfit_nameval, ' = ',
     &      f_obsfit(num_file_obs),
     &      no_obsfit(num_file_obs)
          ENDIF
         ENDDO
        ENDIF
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
        DO num_var=1,NGENCOST
         IF (no_gencost(num_var).GT.0) THEN
          IL  = ILNBLNK( gencost_name(num_var) )
          IL  = max (IL,15)
          WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &    gencost_name(num_var)(1:IL),' (gencost ', num_var, ') = ',
     &    f_gencost(num_var),
     &    no_gencost(num_var)
         ENDIF
        ENDDO
#endif

        CLOSE(ifc)
C     Do not ever write this cost function again to cfname
        eccoWriteCostFunction = .FALSE.

       ELSE
        WRITE(msgBuf,'(A,A)')
     &           'Not writing cost function info to ', cfname
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C     eccoWriteCostFunction
       ENDIF

C     MASTER_CPU_THREAD
      ENDIF

#ifdef ECCO_VERBOSE
      WRITE(msgBuf,'(a,D22.15)')
     &  ' cost_Final: ecco cost function = ',locfc
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &  '             cost function evaluation finished.'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

      RETURN
      END
