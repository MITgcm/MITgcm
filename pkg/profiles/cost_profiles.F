C $Header: /u/gcmpack/MITgcm/pkg/profiles/cost_profiles.F,v 1.27 2015/08/16 14:48:34 gforget Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C     o==========================================================o
C     | subroutine cost_profiles                                 |
C     | o computes the cost for netcdf profiles data             |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE cost_profiles( myiter, mytime, myThid )

      IMPLICIT NONE

C     ======== Global data ============================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif
#ifdef ALLOW_CTRL
# include "optim.h"
#endif

c     == routine arguments ==
      integer myiter
      _RL     mytime
      integer mythid

#ifdef ALLOW_PROFILES

C     ========= Local variables =======================
      integer K,num_file,num_var,prof_num
      integer bi,bj,iG,jG,fid
      _RL tmp_lon
      _RL prof_traj1D(NLEVELMAX), prof_traj1D_mean(NLEVELMAX)
      _RL prof_data1D(NLEVELMAX), prof_weights1D(NLEVELMAX)
#ifndef ALLOW_CTRL
      integer optimcycle
#endif
      character*(max_len_mbuf) msgbuf
      character*(80) profilesfile, fnameequinc
      integer IL, JL, err

      _RL  objf_prof_tile (nSx,nSy)
      _RL  objf_prof_glo
      _RL  num_prof_tile (nSx,nSy)
      _RL  num_prof_glo

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == end of interface ==

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf,
     &  standardmessageunit,SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') '== cost_profiles: begin =='
      call print_message( msgbuf,
     &  standardmessageunit,SQUEEZE_RIGHT , mythid)

        _BEGIN_MASTER( mythid )

      DO bj=1,nSy
       DO bi=1,nSx

        do num_file=1,NFILESPROFMAX
         fid=fiddata(num_file,bi,bj)

         if ( (ProfNo(num_file,bi,bj).GT.0).AND.
     &        (profilesDoNcOutput) ) then
c need to close the file so that the data is not lost when run finishes
           err = NF_CLOSE(fidforward(num_file,bi,bj))
c then re-open it to compute cost function
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
           IL  = ILNBLNK( profilesfiles(num_file) )
           write(profilesfile(1:80),'(1a)')
     &     profilesfiles(num_file)(1:IL)
           IL  = ILNBLNK( profilesfile )
           JL  = ILNBLNK( profilesDir )
           write(fnameequinc(1:80),'(3a,i3.3,a,i3.3,a)')
     &     profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
c
           err = NF_OPEN(fnameequinc,NF_NOWRITE,
     &     fidforward(num_file,bi,bj))
         endif

         do prof_num=1,NOBSGLOB
          if (prof_num.LE.ProfNo(num_file,bi,bj)) then

c would be needed to call profiles_interp to e.g. get time averages
c          do k=1,NUM_INTERP_POINTS
c           prof_i1D(k)= prof_interp_i(num_file,prof_num,k,bi,bj)
c           prof_j1D(k)= prof_interp_j(num_file,prof_num,k,bi,bj)
c           prof_w1D(k)= prof_interp_weights(num_file,prof_num,k,bi,bj)
c          enddo

           do num_var=1,NVARMAX

            do K=1,NLEVELMAX
             prof_traj1D(k)=0.
             prof_traj1D_mean(k)=0.
             prof_mask1D_cur(k,bi,bj)=0.
             prof_data1D(k)=0.
             prof_weights1D(k)=0.
            enddo

            if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then

             call active_read_profile(num_file,
     &           ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &           prof_num,.false.,optimcycle,bi,bj,mythid,
     &           profiles_dummy(num_file,num_var,bi,bj))

             call profiles_readvector(num_file,num_var,
     &           prof_ind_glob(num_file,prof_num,bi,bj),
     &           ProfDepthNo(num_file,bi,bj),prof_data1D,bi,bj,myThid)

             call profiles_readvector(num_file,-num_var,
     &           prof_ind_glob(num_file,prof_num,bi,bj),
     &           ProfDepthNo(num_file,bi,bj),
     &           prof_weights1D,bi,bj,myThid)

             do K=1,ProfDepthNo(num_file,bi,bj)
               if (prof_weights1D(K).GT.0.) then
                 objf_profiles(num_file,num_var,bi,bj)=
     &             objf_profiles(num_file,num_var,bi,bj)
     &             +prof_weights1D(K)*prof_mask1D_cur(K,bi,bj)
     &             *(prof_traj1D(K)-prof_data1D(K)-prof_traj1D_mean(K))
     &             *(prof_traj1D(K)-prof_data1D(K)-prof_traj1D_mean(K))
                 num_profiles(num_file,num_var,bi,bj)=
     &               num_profiles(num_file,num_var,bi,bj)
     &               +prof_mask1D_cur(K,bi,bj)
               endif
             enddo
            endif

           enddo !do num_var...
          endif !if (prof_num.LE.ProfNo(num_file,bi,bj)) then
         enddo !do prof_num=..

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
         if (ProfNo(num_file,bi,bj).GT.0) then
          do num_var=1,NVARMAX
           write(msgbuf,'(a,4I9)') 'bi,bj,prof_num,num_var ',bi,bj,
     &      ProfNo(num_file,bi,bj),num_var
           call print_message(
     &      msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
           write(msgbuf,'(a,D22.15,D22.15)') 
     &      prof_names(num_file,num_var),
     &      objf_profiles(num_file,num_var,bi,bj),
     &      num_profiles(num_file,num_var,bi,bj)
          enddo !do num_var...
         endif
      ENDIF
#endif /* ALLOW_DEBUG */
        enddo !do num_file=1,NFILESPROFMAX

       ENDDO
      ENDDO

      _END_MASTER( mythid )

c print cost function values
      do num_file=1,NFILESPROFMAX
      do num_var=1,NVARMAX
c
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          objf_prof_tile(bi,bj) =
     &             objf_profiles(num_file,num_var,bi,bj)
          num_prof_tile(bi,bj) =
     &             num_profiles(num_file,num_var,bi,bj)
       enddo
      enddo
c
      CALL GLOBAL_SUM_TILE_RL( objf_prof_tile, objf_prof_glo, myThid )
      CALL GLOBAL_SUM_TILE_RL( num_prof_tile, num_prof_glo, myThid )
c
      write(msgbuf,'(a,I2,a,I2,a,2D12.5)')
     &  ' cost_profiles(',num_file,',',num_var,')= ',
     &  objf_prof_glo,num_prof_glo

      IF ( num_prof_glo .GT. 0. ) call print_message( msgbuf,
     &  standardmessageunit,SQUEEZE_RIGHT , mythid)
c
      enddo
      enddo

      write(msgbuf,'(a)') '== cost_profiles: end   =='
      call print_message( msgbuf,
     &  standardmessageunit,SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf,
     &  standardmessageunit,SQUEEZE_RIGHT , mythid)


c     call profiles_make_ncfile(mythid)

C===========================================================

#endif

      RETURN
      END
