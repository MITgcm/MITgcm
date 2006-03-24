#include "COST_CPPOPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_init_ncfile                          |
C     | o initialization of model counterparts files 		 |
C     |   for netcdf profiles data 		                 |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      subroutine profiles_init_ncfile(num_file,fid1,file2,fid2,length,
     & Zlength,myThid)

      implicit none

C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES_CONTRIBUTION
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================
      integer fid1,fid2,dimid1,dimid2,varid(10),varid0
      integer myThid,err,vecid(2), length, Zlength
      character*(80) file1, file2
      integer irec, num_var,num_file
      real*8 tmp_vec(Zlength+1)
c     == end of interface ==

#ifdef ALLOW_PROFILES_CONTRIBUTION

      if (profilesfile_equi_type.EQ.1) then
c1) creation :
      err = NF_CREATE(file2 , NF_CLOBBER, fid2)
      err = NF_DEF_DIM(fid2,'iDEPTH', Zlength ,dimid1)
      err = NF_DEF_DIM(fid2,'iPROF',length,dimid2)
      vecid(1)=dimid1
      vecid(2)=dimid2

      err = NF_DEF_VAR (fid2,'prof_ind_glob', NF_INT, 1,vecid(2),
     & varid(1))
      err = NF_PUT_ATT_INT(fid2,varid(1),'_FillValue',NF_INT,1,0)

      do num_var=1,4
      err = NF_INQ_VARID(fid1,prof_names(num_var), varid0 )
      if (err.EQ.NF_NOERR) then

      err = NF_DEF_VAR (fid2,prof_names(num_var), NF_DOUBLE, 2,vecid,
     & varid(2+(num_var-1)*2)) 
      err = NF_PUT_ATT_DOUBLE(fid2, varid(2+(num_var-1)*2),'_FillValue',
     & NF_DOUBLE,1, 0. _d 0 ) 
      err = NF_DEF_VAR (fid2,prof_namesmask(num_var), 
     & NF_DOUBLE, 2,vecid, varid(3+(num_var-1)*2))
      err = NF_PUT_ATT_DOUBLE(fid2,varid(3+(num_var-1)*2),'_FillValue',
     & NF_DOUBLE,1, 0. _d 0)

      endif
      enddo 

      err=NF_ENDDEF(fid2)
      err=NF_CLOSE(fid2)

      err = NF_OPEN(file2, NF_WRITE , fid2)

      else

       call MDSFINDUNIT( fid2 , mythid )
       open( fid2, file=file2, form ='unformatted', status='unknown',
     &      access='direct', recl=  (Zlength + 1)*WORDLENGTH*2 )

       do irec=1,Zlength+1
       tmp_vec(irec)=0
       enddo
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(Zlength+1,tmp_vec)
#endif    
       do irec=length,1,-1
       do num_var=prof_num_var_tot(num_file),1,-1
       write(fid2,rec=((irec-1)*prof_num_var_tot(num_file)
     & +num_var-1)*2 +1) tmp_vec
       write(fid2,rec=((irec-1)*prof_num_var_tot(num_file)
     & +num_var-1)*2 +2) tmp_vec
       enddo
       enddo

       endif

#endif

      END
