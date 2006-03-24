
#include "CPP_OPTIONS.h"

C     o==========================================================o
C     | subroutine active_file_control_profile                   |
C     | o handles the i/o of active variables for the adjoint    |
C     |   calculations, related to netcdf profiles data files    | 
C     | o active_read_profile_rl, active_write_profile_rl	 |
C     | started: Gael Forget 15-March-2006			 |
C     o==========================================================o

      subroutine active_read_profile_rl(
     I                                fid,
     I                           active_num_file,
     I                                nactive_var,
     O                                active_var,
     I                                active_varnum,
     I                                lAdInit,
     I                                irec,
     I                                irecglob,
     I                                theSimulationMode,
     I                                myOptimIter,
     I                                mythid
     &                              )

c     ==================================================================
c
c     o Read an active 1D record.
c
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_PROFILES
#include "netcdf.inc"
#include "profiles.h"
#endif

c     == routine arguments ==

      integer err, fid, varid1 , vec_start(2), vec_count(2)

      logical  lAdInit
      integer  irec, irecglob,active_varnum
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      integer  nactive_var,active_num_file
      _RL   active_var(nactive_var)
      _RL   active_data_t(nactive_var)
      integer i,ivar
      real*8 vec_tmp(nactive_var+1)

#ifdef ALLOW_PROFILES

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

      if (profilesfile_equi_type.EQ.1) then
      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid,prof_names(active_varnum), varid1 )
      err = NF_GET_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_var)
      
      err = NF_INQ_VARID(fid,prof_namesmask(active_varnum), varid1 )
      err = NF_GET_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & prof_mask1D_cur)

      else

      read(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif    
      do ivar=1,nactive_var
      active_var(ivar)=vec_tmp(ivar)
      enddo
      read(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+2 )
     & vec_tmp
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      do ivar=1,nactive_var
      prof_mask1D_cur(ivar)=vec_tmp(ivar)
      enddo

      endif 

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

      if (profilesfile_equi_type.EQ.1) then

      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid,prof_names(active_varnum), varid1 )
      err = NF_GET_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_data_t)

c       Add active_var from appropriate location to data.
        do i = 1,nactive_var
           active_data_t(i) = active_data_t(i) + active_var(i)
        enddo
c       Store the result on disk.
      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid,prof_names(active_varnum), varid1 )
      err = NF_PUT_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_data_t)

c       Set active_var to zero. 
        do i = 1,nactive_var
           active_var(i) = 0. _d 0
        enddo

      else


      read(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      do ivar=1,nactive_var
      active_data_t(ivar)=vec_tmp(ivar)
      enddo

c       Add active_var from appropriate location to data.
        do i = 1,nactive_var
           active_data_t(i) = active_data_t(i) + active_var(i)
        enddo

c       Store the result on disk.
      do ivar=1,nactive_var
      vec_tmp(ivar)=active_data_t(ivar)
      enddo
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      write(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp

c       Set active_var to zero.
        do i = 1,nactive_var
           active_var(i) = 0. _d 0
        enddo

      endif

        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. TANGENT_SIMULATION) then

        _BEGIN_MASTER( mythid )

      if (profilesfile_equi_type.EQ.1) then

      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid,prof_names(active_varnum), varid1 )
      err = NF_GET_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_var)

      else

      read(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      do ivar=1,nactive_var
      active_var(ivar)=vec_tmp(ivar)
      enddo

      endif

        _END_MASTER( mythid )

      endif

      _BARRIER

#endif /* ALLOW_PROFILES */

      return
      end

c     ==================================================================


      subroutine active_write_profile_rl(
     I                                fid,
     I                           active_num_file,
     I                                 nactive_var,
     I                                 active_var,
     I                                 active_varnum,
     I                                 irec,
     I                                 irecglob,
     I                                 theSimulationMode,
     I                                 myOptimIter,
     I                                 mythid
     &                               )

c     ==================================================================
c
c     o Write an active 1D record to a file.
c
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_PROFILES
#include "netcdf.inc"
#include "profiles.h"
#endif

c     == routine arguments ==

      integer err, fid, varid1 , vec_start(2), vec_count(2)
      integer  ivar, irec, irecglob,active_varnum
      integer  theSimulationMode
      integer  myOptimIter
      integer  mythid
      integer  nactive_var,active_num_file
      _RL   active_var(nactive_var)
      real*8 vec_tmp(nactive_var+1)

#ifdef ALLOW_PROFILES
c     == local variables ==

      integer  i
      _RL   active_data_t(nactive_var)

c     == end of interface ==

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<


      if (theSimulationMode .eq. FORWARD_SIMULATION) then

        _BEGIN_MASTER( mythid )

      if (profilesfile_equi_type.EQ.1) then

      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid,prof_names(active_varnum), varid1 )
      err = NF_PUT_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_var)

      err = NF_INQ_VARID(fid,prof_namesmask(active_varnum), varid1 )
      err = NF_PUT_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & prof_mask1D_cur)

      err = NF_INQ_VARID(fid,'prof_ind_glob', varid1 )
      err = NF_PUT_VAR1_INT(fid, varid1 , vec_start(2),
     & irecglob)

      else


      do ivar=1,nactive_var
      vec_tmp(ivar)=active_var(ivar)
      enddo
      vec_tmp(nactive_var+1)=irecglob
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      write(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp
      do ivar=1,nactive_var
      vec_tmp(ivar)=prof_mask1D_cur(ivar)
      enddo
      vec_tmp(nactive_var+1)=irecglob
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      write(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+2 )
     & vec_tmp

      endif


        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. REVERSE_SIMULATION) then

        _BEGIN_MASTER( mythid )

      if (profilesfile_equi_type.EQ.1) then

      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1
      err = NF_INQ_VARID(fid,prof_names(active_varnum) , varid1 )
      err = NF_GET_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_data_t)

c       Add active_var to data.
        do i = 1,nactive_var
           active_var(i) = active_var(i) + active_data_t(i)
           active_data_t(i) = 0. _d 0
        enddo

      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid,prof_names(active_varnum), varid1 )
      err = NF_PUT_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_data_t)

      else


      read(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      do ivar=1,nactive_var
      active_data_t(ivar)=vec_tmp(ivar)
      enddo

c       Add active_var from appropriate location to data.
        do i = 1,nactive_var
           active_var(i) = active_var(i) + active_data_t(i)
           active_data_t(i) = 0. _d 0
        enddo

c       Store the result on disk.
      do ivar=1,nactive_var
      vec_tmp(ivar)=active_data_t(ivar)
      enddo
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      write(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp

      endif


        _END_MASTER( mythid )

      endif

c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
c     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      if (theSimulationMode .eq. TANGENT_SIMULATION) then

        _BEGIN_MASTER( mythid )

      if (profilesfile_equi_type.EQ.1) then

      vec_start(1)=1
      vec_start(2)=irec
      vec_count(1)=nactive_var
      vec_count(2)=1

      err = NF_INQ_VARID(fid, prof_names(active_varnum), varid1 )
      err = NF_PUT_VARA_DOUBLE(fid, varid1 , vec_start, vec_count,
     & active_var)

      else


      do ivar=1,nactive_var
      vec_tmp(ivar)=active_var(ivar)
      enddo
      vec_tmp(nactive_var+1)=irecglob
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
      write(fid,rec=( (irec-1)*prof_num_var_tot(active_num_file)
     & +prof_num_var_cur(active_num_file,active_varnum)-1)*2+1 )
     & vec_tmp

      endif


        _END_MASTER( mythid )

      endif

      _BARRIER

#endif /* ALLOW_PROFILES */

      return
      end

