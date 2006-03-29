#include "COST_CPPOPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_init_fixed 				 |
C     | o initialization for netcdf profiles data		 |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_init_fixed( myThid )

      implicit none

C ==================== Global Variables ===========================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "cal.h"
#include "ecco_cost.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PROFILES_CONTRIBUTION
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================

      integer k,l,m,bi,bj,iG,jG, myThid,num_file,length_for_tile
      integer fid, dimid, varid1, varid1a, varid1b
      integer varid2,varid3
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      integer tmpdate(4),tmpdiff(4)
      _RL  tmp_lon, tmp_lon2(1000), tmp_lat2(1000)
      integer vec_start(2), vec_count(2), profno_div1000, kk
      character*(80) profilesfile, fnamedatanc
      character*(80) fnameequinc, adfnameequinc
      integer IL, err
      logical  exst

#ifdef ALLOW_PROFILES_CONTRIBUTION

c     == external functions ==
      integer ILNBLNK

c--   == end of interface ==

      prof_names(1)='prof_T'
      prof_names(2)='prof_S'
      prof_names(3)='prof_U'
      prof_names(4)='prof_V'
      prof_namesmask(1)='prof_Tmask'
      prof_namesmask(2)='prof_Smask'
      prof_namesmask(3)='prof_Umask'
      prof_namesmask(4)='prof_Vmask'
      prof_namesweight(1)='prof_Tweight'
      prof_namesweight(2)='prof_Sweight'
      prof_namesweight(3)='prof_Uweight'
      prof_namesweight(4)='prof_Vweight'
      profiles_curfile_buff=0
      profilesfile_equi_type=2

      do m=1,NLEVELMAX
      do l=1,1000
      do k=1,4
      profiles_data_buff(m,l,k)=0
      profiles_weight_buff(m,l,k)=0
      enddo
      enddo
      enddo
 
c remplacer par une boucle ensuite :
      do num_file=1,NFILESPROFMAX

      IL  = ILNBLNK( profilesfiles(num_file) )
      if (IL.NE.0) then
      write(profilesfile(1:80),'(1a)') profilesfiles(num_file)(1:IL)
      else
      write(profilesfile(1:80),'(1a)') ' '
      endif

      IL  = ILNBLNK( profilesfile )
      if (IL.NE.0) then

C===========================================================
c open data files and read the position vectors
C===========================================================

      write(fnamedatanc(1:80),'(2a)') profilesfile(1:IL),'.nc'
      err = NF_OPEN(fnamedatanc, 0, fiddata(num_file))

c1)  read the number of profiles :
cgf      err = NF_OPEN(filename, 0, fid)
      fid=fiddata(num_file)
      err = NF_INQ_DIMID(fid,'iPROF', dimid )
      err = NF_INQ_DIMLEN(fid, dimid, ProfNo(num_file) )
      err = NF_INQ_DIMID(fid,'iDEPTH', dimid )
      if (err.NE.NF_NOERR) then
      err = NF_INQ_DIMID(fid,'Z', dimid )
      endif
      err = NF_INQ_DIMLEN(fid, dimid, ProfDepthNo(num_file) )
      print*,"fid num_file ProfNo(num_file) ProfDepthNo(num_file)",
     &fid,num_file,ProfNo(num_file),ProfDepthNo(num_file) 

c2) read the dates and positions :
      err = NF_INQ_VARID(fid,'depth', varid1a )
      do k=1,ProfDepthNo(num_file)
      err = NF_GET_VAR1_DOUBLE(fid,varid1a,k,
     & prof_depth(num_file,k))
      enddo

      err = NF_INQ_VARID(fid,'prof_YYYYMMDD', varid1a )
      err = NF_INQ_VARID(fid,'prof_HHMMSS', varid1b )
      err = NF_INQ_VARID(fid,'prof_lon', varid2 )
      err = NF_INQ_VARID(fid,'prof_lat', varid3 )

      DO bi = myBxLo(myThid), myBxHi(myThid)
      DO bj = myByLo(myThid), myByHi(myThid)

      do k=1,NOBSGLOB
      prof_time(num_file,k)=-999
      prof_lon(num_file,k)=-999
      prof_lat(num_file,k)=-999
      prof_ind_glob(num_file,k)=-999
      enddo


      length_for_tile=0
      profno_div1000=max(0,int(profno(num_file)/1000))

      do kk=1,profno_div1000+1

      if (min(ProfNo(num_file), 1000*kk).GE.
     &  1+1000*(kk-1)) then

      vec_start(1)=1
      vec_start(2)=1+1000*(kk-1)
      vec_count(1)=1
      vec_count(2)=min(1000,ProfNo(num_file)-1000*(kk-1))

      if ( (vec_count(2).LE.0).OR.(vec_count(2).GT.1000).OR.
     & (vec_start(2).LE.0).OR.
     & (vec_count(2)+vec_start(2)-1.GT.ProfNo(num_file)) ) 
     & then
      print*,"stop 1",vec_start, vec_count
      stop
      endif

      err = NF_GET_VARA_DOUBLE(fid,varid1a,vec_start(2),
     & vec_count(2), tmpyymmdd)
      err = NF_GET_VARA_DOUBLE(fid,varid1b,vec_start(2),
     & vec_count(2), tmphhmmss)
      err = NF_GET_VARA_DOUBLE(fid,varid2,vec_start(2),
     & vec_count(2), tmp_lon2)
      err = NF_GET_VARA_DOUBLE(fid,varid3,vec_start(2),
     & vec_count(2), tmp_lat2)

      if (err.NE.NF_NOERR) then
      print*,"stop 2",vec_start(2),vec_count(2),
     & kk,min(1000,ProfNo(num_file)-1000*(kk-1))
      stop
      endif 

      do k=1,min(1000,ProfNo(num_file)-1000*(kk-1))

      call cal_FullDate( int(tmpyymmdd(k)),int(tmphhmmss(k)),
     & tmpdate,mythid )
      call cal_TimePassed( modelstartdate,tmpdate,tmpdiff,mythid )
      call cal_ToSeconds (tmpdiff,diffsecs,mythid)
      diffsecs=diffsecs+nIter0*deltaTclock

       if (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) then
        tmp_lon=xC(sNx+1,1,bi,bj)+360
       else
        tmp_lon=xC(sNx+1,1,bi,bj)
       endif
       if ((xC(1,1,bi,bj).LE.tmp_lon2(k)).AND.
     & (tmp_lon.GT.tmp_lon2(k)).AND.
     & (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     & (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k))
     & ) then
        length_for_tile=length_for_tile+1
        prof_time(num_file,length_for_tile)=diffsecs
        prof_lon(num_file,length_for_tile)=tmp_lon2(k)
        prof_lat(num_file,length_for_tile)=tmp_lat2(k)
        prof_ind_glob(num_file,length_for_tile)=k+1000*(kk-1)
        if (length_for_tile.GT.NOBSGLOB) then
      print*,"too much profiles: need to increase NOBSGLOB,"
      print*,"   or split the data file (less memory cost)"
      stop
        endif
       elseif (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) then
        if ((xC(1,1,bi,bj).LE.tmp_lon2(k)+360).AND.
     &  (tmp_lon.GT.tmp_lon2(k)+360).AND.
     &  (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     &  (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k))
     &  ) then
         length_for_tile=length_for_tile+1
         prof_time(num_file,length_for_tile)=diffsecs
         prof_lon(num_file,length_for_tile)=tmp_lon2(k)+360
         prof_lat(num_file,length_for_tile)=tmp_lat2(k)
         prof_ind_glob(num_file,length_for_tile)=k+1000*(kk-1)
         if (length_for_tile.GT.NOBSGLOB) then
      print*,"too much profiles: need to increase NOBSGLOB,"
      print*,"   or split the data file (less memory cost)"
      stop
         endif
        endif
       endif
      enddo
      endif
      enddo

      ProfNo(num_file)=length_for_tile
      print*,"fid dimid ProfNo(num_file)",fid, dimid, 
     & num_file, ProfNo(num_file)

      do k=1,4
      prof_num_var_cur(num_file,k)=0
      enddo
      prof_num_var_tot(num_file)=0

c3) detect available data types
      err = NF_INQ_VARID(fid,'prof_T', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,1)=.TRUE.
      prof_num_var_tot(num_file)=prof_num_var_tot(num_file)+1
      prof_num_var_cur(num_file,1)=prof_num_var_tot(num_file)
      else
      vec_quantities(num_file,1)=.FALSE.
      endif
      err = NF_INQ_VARID(fid,'prof_S', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,2)=.TRUE.
      prof_num_var_tot(num_file)=prof_num_var_tot(num_file)+1
      prof_num_var_cur(num_file,2)=prof_num_var_tot(num_file)
      else
      vec_quantities(num_file,2)=.FALSE.
      endif
      err = NF_INQ_VARID(fid,'prof_U', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,3)=.TRUE.
      prof_num_var_tot(num_file)=prof_num_var_tot(num_file)+1
      prof_num_var_cur(num_file,3)=prof_num_var_tot(num_file)
      else
      vec_quantities(num_file,3)=.FALSE.
      endif
      err = NF_INQ_VARID(fid,'prof_V', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,4)=.TRUE.
      prof_num_var_tot(num_file)=prof_num_var_tot(num_file)+1
      prof_num_var_cur(num_file,4)=prof_num_var_tot(num_file)
      else
      vec_quantities(num_file,4)=.FALSE.
      endif


C===========================================================
c create files for model counterparts to observations
C===========================================================

           if (profno(num_file).GT.0) then   
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles

      if (profilesfile_equi_type.EQ.1) then
 
      write(fnameequinc(1:80),'(2a,i3.3,a,i3.3,a)') 
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
      write(adfnameequinc(1:80),'(3a,i3.3,a,i3.3,a)') 'ad',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'

      inquire( file=fnameequinc, exist=exst )
      if (.NOT.exst) then
      call profiles_init_ncfile(num_file,fiddata(num_file),fnameequinc,
     & fidforward(num_file),profno(num_file),profdepthno(num_file),
     & myThid)
      call profiles_init_ncfile(num_file,fiddata(num_file),
     & adfnameequinc, fidadjoint(num_file),profno(num_file),
     & profdepthno(num_file), myThid)
      else
      err = NF_OPEN(fnameequinc , NF_WRITE , fidforward(num_file) )
      err = NF_OPEN(adfnameequinc , NF_WRITE , fidadjoint(num_file) )
      endif

      else

      write(fnameequinc(1:80),'(2a,i3.3,a,i3.3,a)')
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.bin'
      write(adfnameequinc(1:80),'(3a,i3.3,a,i3.3,a)') 'ad',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.bin'

      inquire( file=fnameequinc, exist=exst )
      if (.NOT.exst) then
      call profiles_init_ncfile(num_file,fiddata(num_file),fnameequinc,
     & fidforward(num_file),profno(num_file),profdepthno(num_file),
     & myThid)
      call profiles_init_ncfile(num_file,fiddata(num_file),
     & adfnameequinc, fidadjoint(num_file),profno(num_file),
     & profdepthno(num_file), myThid)
      else
       call MDSFINDUNIT( fidforward(num_file) , mythid )
       open( fidforward(num_file),file=fnameequinc, 
     & form ='unformatted',status='unknown', access='direct',
     & recl=  (profdepthno(num_file)+1)*WORDLENGTH*2 )
       call MDSFINDUNIT( fidadjoint(num_file) , mythid )
       open( fidadjoint(num_file),file=adfnameequinc,
     & form ='unformatted',status='unknown', access='direct',
     & recl=  (profdepthno(num_file)+1)*WORDLENGTH*2 )
      endif

      endif

           endif

      ENDDO
      ENDDO


C===========================================================
      else
      ProfNo(num_file)=0
      do k=1,4
      prof_num_var_cur(num_file,k)=0
      vec_quantities(num_file,k)=.FALSE.
      enddo
      prof_num_var_tot(num_file)=0
      do k=1,NOBSGLOB
      prof_time(num_file,k)=-999
      prof_lon(num_file,k)=-999
      prof_lat(num_file,k)=-999
      prof_ind_glob(num_file,k)=-999
      enddo

      endif !if (IL.NE.0) then
      enddo !      do num_file=1,NFILESPROFMAX
C===========================================================

#endif

      END

