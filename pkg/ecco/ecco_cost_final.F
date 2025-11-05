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
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
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

#ifndef ALLOW_PROFILES
      INTEGER NFILESPROFMAX
      parameter (NFILESPROFMAX=1)
      INTEGER NVARMAX
      parameter (NVARMAX=1)
#endif

#ifndef ALLOW_COST
c This quick fix allows to compile and run fwd but, as far as
c the adjoint, pkg/autodiff most likely require cost though.
      _RL fc, glofc
#endif
      _RL locfc

      _RL f_gencost(NGENCOST)
      _RL no_gencost(NGENCOST)
#ifdef ALLOW_PROFILES
      _RL f_profiles(NFILESPROFMAX,NVARMAX)
      _RL f_profiles_mean(NVARMAX)
      _RL no_profiles(NFILESPROFMAX,NVARMAX)
      _RL no_profiles_mean(NVARMAX)
#endif
#ifdef ALLOW_PROFILES
      INTEGER num_file
#endif

      CHARACTER*20 cfname
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER IL

c     == end of interface ==

      ifc = 30

      locfc = 0. _d 0

#ifdef ALLOW_PROFILES
      DO num_var=1,NVARMAX
       DO num_file=1,NFILESPROFMAX
        f_profiles(num_file,num_var)= 0. _d 0
        no_profiles(num_file,num_var)= 0. _d 0
       ENDDO
       f_profiles_mean(num_var)= 0. _d 0
       no_profiles_mean(num_var)= 0. _d 0
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

#ifdef ALLOW_PROFILES
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
#endif
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

#ifndef ALLOW_COST
cgf global sum is now done in cost_final if allow_cost
c--   Do global summation.
      _GLOBAL_SUM_RL( fc , myThid )
#endif

c--   Do global summation for each part of the cost function
#ifdef ALLOW_PROFILES
      DO num_var=1,NVARMAX
       DO num_file=1,NFILESPROFMAX
        _GLOBAL_SUM_RL(f_profiles(num_file,num_var), myThid )
        _GLOBAL_SUM_RL(no_profiles(num_file,num_var), myThid )
       ENDDO
       _GLOBAL_SUM_RL(f_profiles_mean(num_var), myThid )
       _GLOBAL_SUM_RL(no_profiles_mean(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       _GLOBAL_SUM_RL(f_gencost(num_var), myThid )
       _GLOBAL_SUM_RL(no_gencost(num_var), myThid )
      ENDDO
#endif

C     start printing to STDOUT
#ifdef ALLOW_PROFILES
      IF (usePROFILES) THEN
       DO num_file=1,NFILESPROFMAX
        DO num_var=1,NVARMAX
         IF ( no_profiles(num_file,num_var).GT.zeroRL ) THEN
          WRITE(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles =',f_profiles(num_file,num_var),
     &      num_file, num_var
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDDO
       ENDDO
       DO num_var=1,NVARMAX
        IF ( no_profiles_mean(num_var).GT.zeroRL ) THEN
         WRITE(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles_mean =',f_profiles_mean(num_var),
     &      num_var
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

#ifndef ALLOW_COST
cgf this sum is now done in cost_final if allow_cost
      fc = fc + glofc
#endif

      locfc=locfc+glofc

C     only master thread of master CPU open and write to file
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

       WRITE(msgBuf,'(A,D22.15)')
     &           ' --> fc         =', locfc
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )

       WRITE(cfname,'(A,i4.4)') 'costfunction',eccoiter
       IF ( eccoWriteCostFunction ) THEN
        WRITE(msgBuf,'(A,A)')
     &           'Writing cost function info to ', cfname(1:16)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        open(unit=ifc,file=cfname)

#ifdef ALLOW_ECCO_OLD_FC_PRINT
        WRITE(ifc,*)
#else
        WRITE(ifc,'(A,2D22.15)')
#endif
     &       'fc =', locfc, 0.
#ifdef ALLOW_PROFILES
        IF (usePROFILES) THEN
         DO num_file=1,NFILESPROFMAX
          DO num_var=1,NVARMAX
           IF ( no_profiles(num_file,num_var).GT.zeroRL ) THEN
            IL  = ILNBLNK( profilesfiles(num_file) )
            IL  = max (IL,30)
            WRITE(ifc,'(4A,2D22.15)')
     &    profilesfiles(num_file)(1:IL),' ',
     &    prof_names(num_file,num_var), ' = ',
     &    f_profiles(num_file,num_var),
     &    no_profiles(num_file,num_var)
           ENDIF
          ENDDO
         ENDDO
         DO num_var=1,NVARMAX
          IF ( no_profiles_mean(num_var).GT.zeroRL ) THEN
           WRITE(ifc,'(3A,2D22.15)')
     &    'profile_mean ',
     &    prof_names(1,num_var), ' = ',
     &    f_profiles_mean(num_var),
     &    no_profiles_mean(num_var)
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

        close(ifc)
C     Do not ever write this cost function again to cfname
        eccoWriteCostFunction = .FALSE.

       ELSE
        WRITE(msgBuf,'(A,A)')
     &           'Not writing cost function info to ', cfname(1:16)
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
C     eccoWriteCostFunction
       ENDIF
C     MASTER_CPU_THREAD
      ENDIF

#ifdef ECCO_VERBOSE
      WRITE(msgBuf,'(a,D22.15)')
     &  ' cost_Final: final cost function = ',locfc
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
