#include "PROFILES_OPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_init_ncfile                          |
C     | o initialization of model counterparts files             |
C     |   for netcdf profiles data                               |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      subroutine profiles_init_ncfile(num_file,fid1,file2,fid2,length,
     & Zlength,bi,bj,myThid)

      implicit none

C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================
      integer fid1,fid2,dimid1,dimid2,varid(2*NVARMAX),varid0
      integer myThid,bi,bj,err,vecid(2), length, Zlength
      character*(MAX_LEN_FNAM) file2
      integer irec, num_var,num_file
      real*8 tmp_vec(Zlength+1)
c     == end of interface ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf

#ifdef ALLOW_PROFILES

      if (profilesDoNcOutput) then
c1) creation :
      err = NF_CREATE(file2 , NF_CLOBBER, fid2)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 1:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_DEF_DIM(fid2,'iDEPTH', Zlength ,dimid1)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 2:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_DEF_DIM(fid2,'iPROF',length,dimid2)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 3:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      vecid(1)=dimid1
      vecid(2)=dimid2

      err = NF_DEF_VAR (fid2,'prof_ind_glob', NF_INT, 1,vecid(2),
     & varid(1))
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 4:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_PUT_ATT_INT(fid2,varid(1),'_FillValue',NF_INT,1,0)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 5:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif

      do num_var=1,NVARMAX
      err = NF_INQ_VARID(fid1,prof_names(num_file,num_var), varid0 )
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 6:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      if (err.EQ.NF_NOERR) then

      err = NF_DEF_VAR (fid2,prof_names(num_file,num_var), NF_DOUBLE,
     & 2,vecid,varid(2+(num_var-1)*2))
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 7:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_PUT_ATT_DOUBLE(fid2, varid(2+(num_var-1)*2),'_FillValue',
     & NF_DOUBLE,1, 0. _d 0 )
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 8:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_DEF_VAR (fid2,prof_namesmask(num_file,num_var),
     & NF_DOUBLE, 2,vecid, varid(3+(num_var-1)*2))
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile 9:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_PUT_ATT_DOUBLE(fid2,varid(3+(num_var-1)*2),'_FillValue',
     & NF_DOUBLE,1, 0. _d 0)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile10:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif

      endif
      enddo

      err=NF_ENDDEF(fid2)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile11:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err=NF_CLOSE(fid2)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile12:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif

      err = NF_OPEN(file2, NF_WRITE , fid2)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_init_ncfile13:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif

      else

c      call MDSFINDUNIT( fid2 , mythid )
       open( fid2, file=file2, form ='unformatted', status='unknown',
     &      access='direct', recl=  (Zlength + 1)*WORDLENGTH*2 )

       do irec=1,Zlength+1
       tmp_vec(irec)=0
       enddo
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8(Zlength+1,tmp_vec)
#endif
       do irec=length,1,-1
       do num_var=prof_num_var_tot(num_file,bi,bj),1,-1
       write(fid2,rec=((irec-1)*prof_num_var_tot(num_file,bi,bj)
     & +num_var-1)*2 +1) tmp_vec
       write(fid2,rec=((irec-1)*prof_num_var_tot(num_file,bi,bj)
     & +num_var-1)*2 +2) tmp_vec
       enddo
       enddo

       endif

#endif

      END
