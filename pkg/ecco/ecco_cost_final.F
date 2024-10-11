#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      subroutine ecco_cost_final( myThid )

c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================
c
c     o Sum of all cost function contributions.
c
c     ==================================================================
c     SUBROUTINE cost_final
c     ==================================================================

      implicit none

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
cav(
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif
cav)

c     == routine arguments ==

      integer myThid

C     === Functions ====
      LOGICAL  MASTER_CPU_THREAD
      EXTERNAL MASTER_CPU_THREAD
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == local variables ==

      integer bi,bj
      integer ifc
      integer num_var
cav(
c      integer num_var_obs
cav)

#ifndef ALLOW_PROFILES
      integer NFILESPROFMAX
      parameter (NFILESPROFMAX=1)
      integer NVARMAX
      parameter (NVARMAX=1)
#endif
cav(
#ifndef ALLOW_OBSFIT
      integer NFILESMAX_OBS
      parameter (NFILESMAX_OBS=1)
c      integer NVARMAX_OBS
c      parameter (NVARMAX_OBS=1)
#endif
cav)

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
cav(
#ifdef ALLOW_OBSFIT
      _RL f_obsfit(NFILESMAX_OBS)
#endif
cav)
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
      integer num_file
      _RL no_profiles(NFILESPROFMAX,NVARMAX)
      _RL no_profiles_mean(NVARMAX)
#endif
cav(
#ifdef ALLOW_OBSFIT
      integer num_file_obs
      _RL no_obsfit(NFILESMAX_OBS)
#endif
cav)
#ifdef ALLOW_GENTIM2D_CONTROL
      _RL no_gentim2d(maxCtrlTim2D)
#endif
#ifdef ALLOW_GENARR2D_CONTROL
      _RL no_genarr2d(maxCtrlArr2D)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RL no_genarr3d(maxCtrlArr3D)
#endif

      character*20 cfname
      character*(MAX_LEN_MBUF) msgBuf

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
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        f_profiles(num_file,num_var)= 0. _d 0
       enddo
      enddo
      do num_var=1,NVARMAX
       f_profiles_mean(num_var)= 0. _d 0
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
      do num_file_obs=1,NFILESMAX_OBS
c       do num_var_obs=1,NVARMAX_OBS
c        f_obsfit(num_file,num_var)= 0. _d 0
cav this is already global sum
        f_obsfit(num_file_obs)= 
     &          objf_obsfit(num_file_obs)
c       enddo
      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        f_gencost(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        f_gentim2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        f_genarr2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        f_genarr3d(num_var)= 0. _d 0
       enddo
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      no_obcsn   = 0. _d 0
      no_obcss   = 0. _d 0
      no_obcsw   = 0. _d 0
      no_obcse   = 0. _d 0
      no_ageos   = 0. _d 0
#endif
#ifdef ALLOW_PROFILES
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        no_profiles(num_file,num_var)= 0. _d 0
       enddo
      enddo
      do num_var=1,NVARMAX
       no_profiles_mean(num_var)= 0. _d 0
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
      do num_file_obs=1,NFILESMAX_OBS
c       do num_var_obs=1,NVARMAX_OBS
c        no_obsfit(num_file,num_var)= 0. _d 0
cav this is already global sum
        no_obsfit(num_file_obs)= 
     &           num_obsfit(num_file_obs)
c       enddo
      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        no_gencost(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        no_gentim2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        no_genarr2d(num_var)= 0. _d 0
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        no_genarr3d(num_var)= 0. _d 0
       enddo
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
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_profiles(num_file,num_var)
     &            *objf_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
      do num_var=1,NVARMAX
         tile_fc(bi,bj) = tile_fc(bi,bj)
     &           + mult_profiles_mean(num_var)
     &           *objf_profiles_mean(num_var,bi,bj)
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
cav don't add to all tiles
c      do num_file=1,NFILESMAX_OBS
c       do num_var=1,NVARMAX
c          tile_fc(bi,bj) = tile_fc(bi,bj)
c     &            + mult_obsfit(num_file,num_var)
c     &            *objf_obsfit(num_file,num_var)
c       enddo
c      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gencost(num_var)
     &            *objf_gencost(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_gentim2d(num_var)
     &            *objf_gentim2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_genarr2d(num_var)
     &            *objf_genarr2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
          tile_fc(bi,bj) = tile_fc(bi,bj)
     &            + mult_genarr3d(num_var)
     &            *objf_genarr3d(bi,bj,num_var)
       enddo
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
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
          f_profiles(num_file,num_var)=f_profiles(num_file,num_var)
     &            +objf_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
      do num_var=1,NVARMAX
         f_profiles_mean(num_var)=f_profiles_mean(num_var)
     &           +objf_profiles_mean(num_var,bi,bj)
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
cav do this outside of bi,bj loops
c      do num_file=1,NFILESMAX_OBS
c       do num_var=1,NVARMAX
c          f_obsfit(num_file,num_var)=f_obsfit(num_file,num_var)
c     &            +objf_obsfit(num_file,num_var)
c       enddo
c      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
          f_gencost(num_var)=f_gencost(num_var)
     &            +objf_gencost(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
          f_gentim2d(num_var) = f_gentim2d(num_var)
     &            +objf_gentim2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
          f_genarr2d(num_var) = f_genarr2d(num_var)
     &            +objf_genarr2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
          f_genarr3d(num_var) = f_genarr3d(num_var)
     &            +objf_genarr3d(bi,bj,num_var)
       enddo
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
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
          no_profiles(num_file,num_var)=no_profiles(num_file,num_var)
     &            +num_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
      do num_var=1,NVARMAX
         no_profiles_mean(num_var)=no_profiles_mean(num_var)
     &           +num_profiles_mean(num_var,bi,bj)
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
cav do this outside of bi,bj loops
c      do num_file=1,NFILESMAX_OBS
c       do num_var=1,NVARMAX
c          no_obsfit(num_file,num_var)=no_obsfit(num_file,num_var)
c     &            +num_obsfit(num_file,num_var)
c       enddo
c      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
          no_gencost(num_var)=no_gencost(num_var)
     &            +num_gencost(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
          no_gentim2d(num_var) = no_gentim2d(num_var)
     &            +num_gentim2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
          no_genarr2d(num_var) = no_genarr2d(num_var)
     &            +num_genarr2d(bi,bj,num_var)
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
          no_genarr3d(num_var) = no_genarr3d(num_var)
     &            +num_genarr3d(bi,bj,num_var)
       enddo
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
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        _GLOBAL_SUM_RL(f_profiles(num_file,num_var), myThid )
       enddo
      enddo
      do num_var=1,NVARMAX
       _GLOBAL_SUM_RL(f_profiles_mean(num_var), myThid )
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
c      do num_file_obs=1,NFILESMAX_OBS
c       do num_var_obs=1,NVARMAX_OBS
c        _GLOBAL_SUM_RL(f_obsfit(num_file_obs,num_var_obs), myThid )
c       enddo
c      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        _GLOBAL_SUM_RL(f_gencost(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        _GLOBAL_SUM_RL(f_gentim2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        _GLOBAL_SUM_RL(f_genarr2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        _GLOBAL_SUM_RL(f_genarr3d(num_var), myThid )
       enddo
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
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        _GLOBAL_SUM_RL(no_profiles(num_file,num_var), myThid )
       enddo
      enddo
      do num_var=1,NVARMAX
       _GLOBAL_SUM_RL(no_profiles_mean(num_var), myThid )
      enddo
#endif
cav(
#ifdef ALLOW_OBSFIT
c      do num_file=1,NFILESMAX_OBS
c       do num_var=1,NVARMAX
c        _GLOBAL_SUM_RL(f_obsfit(num_file,num_var), myThid )
c       enddo
c      enddo
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        _GLOBAL_SUM_RL(no_gencost(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        _GLOBAL_SUM_RL(no_gentim2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        _GLOBAL_SUM_RL(no_genarr2d(num_var), myThid )
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        _GLOBAL_SUM_RL(no_genarr3d(num_var), myThid )
       enddo
#endif

#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcsn   =',f_obcsn
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcss   =',f_obcss
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcsw   =',f_obcsw
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_obcse   =',f_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
      write(standardmessageunit,'(A,D22.15)')
     &     ' --> f_ageos   =',f_ageos
# endif
#endif
#ifdef ALLOW_PROFILES
      if (usePROFILES) then
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        if ( no_profiles(num_file,num_var).GT.zeroRL ) then
         write(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles =',f_profiles(num_file,num_var),
     &      num_file, num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
      enddo
      do num_var=1,NVARMAX
        if ( no_profiles_mean(num_var).GT.zeroRL ) then
         write(msgBuf,'(A,D22.15,i2.0,i2.0)')
     &     ' --> f_profiles_mean =',f_profiles_mean(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
      enddo
      endif
#endif
cav(
#ifdef ALLOW_OBSFIT
      if (useOBSFIT) then
      do num_file_obs=1,NFILESMAX_OBS
c       do num_var_obs=1,NVARMAX_OBS
        if ( no_obsfit(num_file_obs).GT.zeroRL ) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_obsfit =',f_obsfit(num_file_obs),
     &      num_file_obs
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT , mythid)
        endif
c       enddo
      enddo
      endif
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        if (no_gencost(num_var).GT.0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_gencost =',f_gencost(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        if (no_gentim2d(num_var).GT.0. _d 0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_gentim2d =',f_gentim2d(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif
#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        if (no_genarr2d(num_var).GT.0. _d 0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_genarr2d =',f_genarr2d(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif
#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        if (no_genarr3d(num_var).GT.0. _d 0) then
         write(msgBuf,'(A,D22.15,i2.0)')
     &     ' --> f_genarr3d =',f_genarr3d(num_var),
     &      num_var
         call print_message( msgBuf, standardmessageunit,
     &                       SQUEEZE_RIGHT, myThid )
        endif
       enddo
#endif

c--   Each process has calculated the global part for itself.

#ifndef ALLOW_COST
cgf this sum is now done in cost_final if allow_cost
        fc = fc + glofc
#endif

cav(
cost.h:c     glofc      - contributions from global mean constraints
#ifdef ALLOW_OBSFIT
      do num_file_obs=1,NFILESMAX_OBS
c       do num_var_obs=1,NVARMAX_OBS
        glofc = glofc
     &          +  mult_obsfit(num_file_obs)
     &            *objf_obsfit(num_file_obs)
c       enddo
      enddo
#endif
cav)

      locfc=locfc+glofc

C     only master thread of master CPU open and write to file
      IF ( MASTER_CPU_THREAD(myThid) ) THEN

        write(msgBuf,'(A,D22.15)')
     &           ' --> fc               =', locfc
        call print_message( msgBuf, standardmessageunit,
     &                      SQUEEZE_RIGHT, myThid )

        write(cfname,'(A,i4.4)') 'costfunction',eccoiter
        open(unit=ifc,file=cfname)

#ifdef ALLOW_ECCO_OLD_FC_PRINT
        write(ifc,*)
#else
        write(ifc,'(A,2D22.15)')
#endif
     &       'fc =', locfc, 0.
#if (defined ALLOW_CTRL && defined ALLOW_OBCS)
        write(ifc,'(A,2D22.15)')
     &       'f_obcsn =', f_obcsn, no_obcsn
        write(ifc,'(A,2D22.15)')
     &       'f_obcss =', f_obcss, no_obcss
        write(ifc,'(A,2D22.15)')
     &       'f_obcsw =', f_obcsw, no_obcsw
        write(ifc,'(A,2D22.15)')
     &       'f_obcse =', f_obcse, no_obcse
# ifdef OBCS_AGEOS_COST_CONTRIBUTION
        write(ifc,'(A,2D22.15)')
     &       'f_ageos =', f_ageos, no_ageos
# endif
#endif
#ifdef ALLOW_PROFILES
      if (usePROFILES) then
      do num_file=1,NFILESPROFMAX
       do num_var=1,NVARMAX
        if ( no_profiles(num_file,num_var).GT.zeroRL ) then
         IL  = ILNBLNK( profilesfiles(num_file) )
         IL  = max (IL,30)
         write(ifc,'(4A,2D22.15)')
     &    profilesfiles(num_file)(1:IL),' ',
     &    prof_names(num_file,num_var), ' = ',
     &    f_profiles(num_file,num_var),
     &    no_profiles(num_file,num_var)
        endif
       enddo
      enddo
      do num_var=1,NVARMAX
        if ( no_profiles_mean(num_var).GT.zeroRL ) then
         write(ifc,'(3A,2D22.15)')
     &    'profile_mean ',
     &    prof_names(1,num_var), ' = ',
     &    f_profiles_mean(num_var),
     &    no_profiles_mean(num_var)
        endif
      enddo
      endif
#endif
cav(
#ifdef ALLOW_OBSFIT
      if (useOBSFIT) then
      do num_file_obs=1,NFILESMAX_OBS
c       do num_var_obs=1,NVARMAX_OBS
        if ( no_obsfit(num_file_obs).GT.zeroRL ) then
         IL  = ILNBLNK( obsfitfiles(num_file_obs) )
         IL  = max (IL,30)
         write(ifc,'(4A,2D22.15)')
     &    obsfitfiles(num_file_obs)(1:IL),' ',
     &    obsfit_nameval, ' = ',
     &    f_obsfit(num_file_obs),
     &    no_obsfit(num_file_obs)
        endif
c       enddo
      enddo
      endif
#endif
cav)
#ifdef ALLOW_GENCOST_CONTRIBUTION
       do num_var=1,NGENCOST
        if (no_gencost(num_var).GT.0) then
         IL  = ILNBLNK( gencost_name(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    gencost_name(num_var)(1:IL),' (gencost ', num_var, ') = ',
     &    f_gencost(num_var),
     &    no_gencost(num_var)
         endif
       enddo
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
       do num_var=1,maxCtrlTim2D
        if (no_gentim2d(num_var).GT.0. _d 0) then
         IL  = ILNBLNK( xx_gentim2d_file(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_gentim2d_file(num_var)(1:IL),
     &    ' (gentim2d ', num_var, ') = ',
     &    f_gentim2d(num_var),
     &    no_gentim2d(num_var)
         endif
       enddo
#endif

#ifdef ALLOW_GENARR2D_CONTROL
       do num_var=1,maxCtrlArr2D
        if (no_genarr2d(num_var).GT.0. _d 0) then
         IL  = ILNBLNK( xx_genarr2d_file(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_genarr2d_file(num_var)(1:IL),
     &    ' (genarr2d ', num_var, ') = ',
     &    f_genarr2d(num_var),
     &    no_genarr2d(num_var)
         endif
       enddo
#endif

#ifdef ALLOW_GENARR3D_CONTROL
       do num_var=1,maxCtrlArr3D
        if (no_genarr3d(num_var).GT.0. _d 0) then
         IL  = ILNBLNK( xx_genarr3d_file(num_var) )
         IL  = max (IL,15)
         write(ifc,'(2A,i2.0,A,2D22.15)')
     &    xx_genarr3d_file(num_var)(1:IL),
     &    ' (genarr3d ', num_var, ') = ',
     &    f_genarr3d(num_var),
     &    no_genarr3d(num_var)
         endif
       enddo
#endif

        close(ifc)

      ENDIF

#ifdef ECCO_VERBOSE
      write(msgBuf,'(a,D22.15)')
     &  ' cost_Final: final cost function = ',locfc
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgBuf,'(a)') ' '
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgBuf,'(a)')
     &  '             cost function evaluation finished.'
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
      write(msgBuf,'(a)') ' '
      call print_message( msgBuf, standardmessageunit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

      return
      end
