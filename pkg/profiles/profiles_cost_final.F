#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_COST
#include "COST_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: PROFILES_COST_FINAL
C     !INTERFACE:
      SUBROUTINE PROFILES_COST_FINAL( myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE PROFILES_COST_FINAL
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
#include "PROFILES_SIZE.h"
#include "profiles.h"

C     !INPUT/OUTPUT PARAMETERS:
      INTEGER myThid

#if defined ALLOW_COST
C     ! FUNCTIONS:
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
      INTEGER bi, bj, num_var
#ifndef ALLOW_PROFILES
      INTEGER NFILESPROFMAX
      parameter (NFILESPROFMAX=1)
      INTEGER NVARMAX
      parameter (NVARMAX=1)
#endif
      INTEGER num_file
      _RL f_profiles(NFILESPROFMAX,NVARMAX)
      _RL f_profiles_mean(NVARMAX)
      _RL no_profiles(NFILESPROFMAX,NVARMAX)
      _RL no_profiles_mean(NVARMAX)

      INTEGER ifc
      CHARACTER*25 cfname
      INTEGER IL
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      ifc = 30

C     Initialise cost function variables zo zero
      DO num_var=1,NVARMAX
       DO num_file=1,NFILESPROFMAX
        f_profiles(num_file,num_var)= 0. _d 0
        no_profiles(num_file,num_var)= 0. _d 0
       ENDDO
       f_profiles_mean(num_var)= 0. _d 0
       no_profiles_mean(num_var)= 0. _d 0
      ENDDO

c--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO num_var=1,NVARMAX
         DO num_file=1,NFILESPROFMAX
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_profiles(num_file,num_var)
     &            *objf_profiles(num_file,num_var,bi,bj)
          f_profiles(num_file,num_var)=f_profiles(num_file,num_var)
     &            +objf_profiles(num_file,num_var,bi,bj)
          no_profiles(num_file,num_var)=no_profiles(num_file,num_var)
     &            +num_profiles(num_file,num_var,bi,bj)
         ENDDO
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &           + mult_profiles_mean(num_var)
     &           *objf_profiles_mean(num_var,bi,bj)
         f_profiles_mean(num_var)=f_profiles_mean(num_var)
     &           +objf_profiles_mean(num_var,bi,bj)
         no_profiles_mean(num_var)=no_profiles_mean(num_var)
     &           +num_profiles_mean(num_var,bi,bj)
        ENDDO

C-   end bi,bj loops
       ENDDO
      ENDDO

c--   Do global summation for each part of the cost function
      DO num_var=1,NVARMAX
       DO num_file=1,NFILESPROFMAX
        _GLOBAL_SUM_RL(f_profiles(num_file,num_var), myThid )
        _GLOBAL_SUM_RL(no_profiles(num_file,num_var), myThid )
       ENDDO
       _GLOBAL_SUM_RL(f_profiles_mean(num_var), myThid )
       _GLOBAL_SUM_RL(no_profiles_mean(num_var), myThid )
      ENDDO

C     start printing to STDOUT
      DO num_file=1,NFILESPROFMAX
       DO num_var=1,NVARMAX
        IF ( no_profiles(num_file,num_var).GT.zeroRL ) THEN
         WRITE(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles =',f_profiles(num_file,num_var),
     &      num_file, num_var
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
       ENDDO
      ENDDO
      DO num_var=1,NVARMAX
       IF ( no_profiles_mean(num_var).GT.zeroRL ) THEN
        WRITE(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles_mean =',f_profiles_mean(num_var),
     &      num_var
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO

c--   Each process has calculated the global part for itself.

C     only master thread of master CPU open and write to file
      IF ( MASTER_CPU_THREAD(myThid)
     &     .AND. profilesWriteCostFunction ) THEN
       WRITE(cfname,'(A,I4.4)') 'costfunction_profiles',optimcycle
       OPEN(unit=ifc,file=cfname)
       WRITE(msgBuf,'(A,A)')
     &      'Writing profiles cost function info to ', cfname
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

       DO num_file=1,NFILESPROFMAX
        DO num_var=1,NVARMAX
         IF ( no_profiles(num_file,num_var).GT.zeroRL ) THEN
          IL = ILNBLNK( profilesfiles(num_file) )
          IL = MAX(IL,30)
          WRITE(ifc,'(4A,2D22.15)')
     &         profilesfiles(num_file)(1:IL),' ',
     &         prof_names(num_file,num_var), ' = ',
     &         f_profiles(num_file,num_var),
     &         no_profiles(num_file,num_var)
         ENDIF
        ENDDO
       ENDDO
       DO num_var=1,NVARMAX
        IF ( no_profiles_mean(num_var).GT.zeroRL ) THEN
         WRITE(ifc,'(3A,2D22.15)')
     &        'profile_mean ', prof_names(1,num_var), ' = ',
     &        f_profiles_mean(num_var), no_profiles_mean(num_var)
        ENDIF
       ENDDO
       CLOSE(ifc)
C     Do it only once per run, otherwise grdchk will overwrite or append
C     files in an uncontrolled way.
       profilesWriteCostFunction = .FALSE.

C     MASTER_CPU_THREAD .AND. profilesoWriteCostFunction
      ENDIF

#endif /* ALLOW_COST */

      RETURN
      END
