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
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_GENARR.h"
# ifdef ALLOW_OBCS
#  include "CTRL_OBCS.h"
# endif /* ALLOW_OBCS */
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
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _RL f_obcsn, f_obcss, f_obcsw, f_obcse, f_ageos
#endif
#ifdef ALLOW_PROFILES
      _RL f_profiles(NFILESPROFMAX,NVARMAX)
      _RL f_profiles_mean(NVARMAX)
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      _RL f_gentim2d(maxCtrlTim2D)
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      _RL f_genarr2d(maxCtrlArr2D)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL f_genarr3d(maxCtrlArr3D)
#endif
      _RL no_gencost(NGENCOST)
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _RL no_obcsn, no_obcss, no_obcsw, no_obcse, no_ageos
#endif
#ifdef ALLOW_PROFILES
      INTEGER num_file
      _RL no_profiles(NFILESPROFMAX,NVARMAX)
      _RL no_profiles_mean(NVARMAX)
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      _RL no_gentim2d(maxCtrlTim2D)
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      _RL no_genarr2d(maxCtrlArr2D)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL no_genarr3d(maxCtrlArr3D)
#endif

      CHARACTER*20 cfname
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      INTEGER IL

c     == end of interface ==

      ifc = 30

      locfc = 0. _d 0

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      f_obcsn   = 0. _d 0
      f_obcss   = 0. _d 0
      f_obcsw   = 0. _d 0
      f_obcse   = 0. _d 0
      f_ageos   = 0. _d 0
#endif
#ifdef ALLOW_PROFILES
      DO num_file=1,NFILESPROFMAX
       DO num_var=1,NVARMAX
        f_profiles(num_file,num_var)= 0. _d 0
       ENDDO
      ENDDO
      DO num_var=1,NVARMAX
       f_profiles_mean(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       f_gencost(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       f_gentim2d(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       f_genarr2d(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       f_genarr3d(num_var)= 0. _d 0
      ENDDO
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      no_obcsn   = 0. _d 0
      no_obcss   = 0. _d 0
      no_obcsw   = 0. _d 0
      no_obcse   = 0. _d 0
      no_ageos   = 0. _d 0
#endif
#ifdef ALLOW_PROFILES
      DO num_file=1,NFILESPROFMAX
       DO num_var=1,NVARMAX
        no_profiles(num_file,num_var)= 0. _d 0
       ENDDO
      ENDDO
      DO num_var=1,NVARMAX
       no_profiles_mean(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       no_gencost(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       no_gentim2d(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       no_genarr2d(num_var)= 0. _d 0
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       no_genarr3d(num_var)= 0. _d 0
      ENDDO
#endif

c--   Sum up all contributions.
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
        tile_fc(bi,bj) = tile_fc(bi,bj)
     &         + mult_obcsn   * objf_obcsn(bi,bj)
     &         + mult_obcss   * objf_obcss(bi,bj)
     &         + mult_obcsw   * objf_obcsw(bi,bj)
     &         + mult_obcse   * objf_obcse(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
     &         + mult_ageos   * objf_ageos(bi,bj)
# endif
#endif
#ifdef ALLOW_PROFILES
        DO num_file=1,NFILESPROFMAX
         DO num_var=1,NVARMAX
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_profiles(num_file,num_var)
     &            *objf_profiles(num_file,num_var,bi,bj)
         ENDDO
        ENDDO
        DO num_var=1,NVARMAX
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &           + mult_profiles_mean(num_var)
     &           *objf_profiles_mean(num_var,bi,bj)
        ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
        DO num_var=1,NGENCOST
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gencost(num_var)
     &            *objf_gencost(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
        DO num_var=1,maxCtrlTim2D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gentim2d(num_var)
     &            *objf_gentim2d(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
        DO num_var=1,maxCtrlArr2D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_genarr2d(num_var)
     &            *objf_genarr2d(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
        DO num_var=1,maxCtrlArr3D
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_genarr3d(num_var)
     &            *objf_genarr3d(bi,bj,num_var)
        ENDDO
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
        f_obcsn  = f_obcsn + objf_obcsn(bi,bj)
        f_obcss  = f_obcss + objf_obcss(bi,bj)
        f_obcsw  = f_obcsw + objf_obcsw(bi,bj)
        f_obcse  = f_obcse + objf_obcse(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        f_ageos  = f_ageos + objf_ageos(bi,bj)
# endif
#endif
#ifdef ALLOW_PROFILES
        DO num_file=1,NFILESPROFMAX
         DO num_var=1,NVARMAX
          f_profiles(num_file,num_var)=f_profiles(num_file,num_var)
     &            +objf_profiles(num_file,num_var,bi,bj)
         ENDDO
        ENDDO
        DO num_var=1,NVARMAX
         f_profiles_mean(num_var)=f_profiles_mean(num_var)
     &           +objf_profiles_mean(num_var,bi,bj)
        ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
        DO num_var=1,NGENCOST
         f_gencost(num_var)=f_gencost(num_var)
     &            +objf_gencost(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
        DO num_var=1,maxCtrlTim2D
         f_gentim2d(num_var) = f_gentim2d(num_var)
     &            +objf_gentim2d(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
        DO num_var=1,maxCtrlArr2D
         f_genarr2d(num_var) = f_genarr2d(num_var)
     &            +objf_genarr2d(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
        DO num_var=1,maxCtrlArr3D
         f_genarr3d(num_var) = f_genarr3d(num_var)
     &            +objf_genarr3d(bi,bj,num_var)
        ENDDO
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
        no_obcsn = no_obcsn + num_obcsn(bi,bj)
        no_obcss = no_obcss + num_obcss(bi,bj)
        no_obcse = no_obcse + num_obcse(bi,bj)
        no_obcsw = no_obcsw + num_obcsw(bi,bj)
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        no_ageos = no_ageos + num_ageos(bi,bj)
# endif
#endif
#ifdef ALLOW_PROFILES
        DO num_file=1,NFILESPROFMAX
         DO num_var=1,NVARMAX
          no_profiles(num_file,num_var)=no_profiles(num_file,num_var)
     &            +num_profiles(num_file,num_var,bi,bj)
         ENDDO
        ENDDO
        DO num_var=1,NVARMAX
         no_profiles_mean(num_var)=no_profiles_mean(num_var)
     &           +num_profiles_mean(num_var,bi,bj)
        ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
        DO num_var=1,NGENCOST
         no_gencost(num_var)=no_gencost(num_var)
     &            +num_gencost(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
        DO num_var=1,maxCtrlTim2D
         no_gentim2d(num_var) = no_gentim2d(num_var)
     &            +num_gentim2d(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
        DO num_var=1,maxCtrlArr2D
         no_genarr2d(num_var) = no_genarr2d(num_var)
     &            +num_genarr2d(bi,bj,num_var)
        ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
        DO num_var=1,maxCtrlArr3D
         no_genarr3d(num_var) = no_genarr3d(num_var)
     &            +num_genarr3d(bi,bj,num_var)
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
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _GLOBAL_SUM_RL( f_obcsn , myThid )
      _GLOBAL_SUM_RL( f_obcss , myThid )
      _GLOBAL_SUM_RL( f_obcsw , myThid )
      _GLOBAL_SUM_RL( f_obcse , myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      _GLOBAL_SUM_RL( f_ageos , myThid )
# endif
#endif
#ifdef ALLOW_PROFILES
      DO num_file=1,NFILESPROFMAX
       DO num_var=1,NVARMAX
        _GLOBAL_SUM_RL(f_profiles(num_file,num_var), myThid )
       ENDDO
      ENDDO
      DO num_var=1,NVARMAX
       _GLOBAL_SUM_RL(f_profiles_mean(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       _GLOBAL_SUM_RL(f_gencost(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       _GLOBAL_SUM_RL(f_gentim2d(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       _GLOBAL_SUM_RL(f_genarr2d(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       _GLOBAL_SUM_RL(f_genarr3d(num_var), myThid )
      ENDDO
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      _GLOBAL_SUM_RL( no_obcsn , myThid )
      _GLOBAL_SUM_RL( no_obcss , myThid )
      _GLOBAL_SUM_RL( no_obcsw , myThid )
      _GLOBAL_SUM_RL( no_obcse , myThid )
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      _GLOBAL_SUM_RL( no_ageos , myThid )
# endif
#endif
#ifdef ALLOW_PROFILES
      DO num_file=1,NFILESPROFMAX
       DO num_var=1,NVARMAX
        _GLOBAL_SUM_RL(no_profiles(num_file,num_var), myThid )
       ENDDO
      ENDDO
      DO num_var=1,NVARMAX
       _GLOBAL_SUM_RL(no_profiles_mean(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENCOST_CONTRIBUTION
      DO num_var=1,NGENCOST
       _GLOBAL_SUM_RL(no_gencost(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       _GLOBAL_SUM_RL(no_gentim2d(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       _GLOBAL_SUM_RL(no_genarr2d(num_var), myThid )
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       _GLOBAL_SUM_RL(no_genarr3d(num_var), myThid )
      ENDDO
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_obcsn   =',f_obcsn
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_obcss   =',f_obcss
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_obcsw   =',f_obcsw
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_obcse   =',f_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      WRITE(standardMessageUnit,'(A,D22.15)')
     &     ' --> f_ageos   =',f_ageos
# endif
#endif
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
        WRITE(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_gencost =',f_gencost(num_var),
     &      num_var
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      DO num_var=1,maxCtrlTim2D
       IF (no_gentim2d(num_var).GT.0. _d 0) THEN
        WRITE(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_gentim2d =',f_gentim2d(num_var),
     &      num_var
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      DO num_var=1,maxCtrlArr2D
       IF (no_genarr2d(num_var).GT.0. _d 0) THEN
        WRITE(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_genarr2d =',f_genarr2d(num_var),
     &      num_var
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      DO num_var=1,maxCtrlArr3D
       IF (no_genarr3d(num_var).GT.0. _d 0) THEN
        WRITE(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_genarr3d =',f_genarr3d(num_var),
     &      num_var
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
     &           ' --> fc               =', locfc
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
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
        WRITE(ifc,'(A,2D22.15)')
     &       'f_obcsn =', f_obcsn, no_obcsn
        WRITE(ifc,'(A,2D22.15)')
     &       'f_obcss =', f_obcss, no_obcss
        WRITE(ifc,'(A,2D22.15)')
     &       'f_obcsw =', f_obcsw, no_obcsw
        WRITE(ifc,'(A,2D22.15)')
     &       'f_obcse =', f_obcse, no_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        WRITE(ifc,'(A,2D22.15)')
     &       'f_ageos =', f_ageos, no_ageos
# endif
#endif
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

#ifdef ALLOW_GENTIM2D_CONTROL
        DO num_var=1,maxCtrlTim2D
         IF (no_gentim2d(num_var).GT.0. _d 0) THEN
          IL  = ILNBLNK( xx_gentim2d_file(num_var) )
          IL  = max (IL,15)
          WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_gentim2d_file(num_var)(1:IL),
     &    ' (gentim2d ', num_var, ') = ',
     &    f_gentim2d(num_var),
     &    no_gentim2d(num_var)
         ENDIF
        ENDDO
#endif

#ifdef ALLOW_GENARR2D_CONTROL
        DO num_var=1,maxCtrlArr2D
         IF (no_genarr2d(num_var).GT.0. _d 0) THEN
          IL  = ILNBLNK( xx_genarr2d_file(num_var) )
          IL  = max (IL,15)
          WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_genarr2d_file(num_var)(1:IL),
     &    ' (genarr2d ', num_var, ') = ',
     &    f_genarr2d(num_var),
     &    no_genarr2d(num_var)
         ENDIF
        ENDDO
#endif

#ifdef ALLOW_GENARR3D_CONTROL
        DO num_var=1,maxCtrlArr3D
         IF (no_genarr3d(num_var).GT.0. _d 0) THEN
          IL  = ILNBLNK( xx_genarr3d_file(num_var) )
          IL  = max (IL,15)
          WRITE(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_genarr3d_file(num_var)(1:IL),
     &    ' (genarr3d ', num_var, ') = ',
     &    f_genarr3d(num_var),
     &    no_genarr3d(num_var)
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
