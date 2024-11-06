#include "PROFILES_OPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_readvector                           |
C     | o reads a profile from a netcdf profiles data file       |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_readvector(fNb, vNb, irec,
     & vec_loc_length, vec_loc , bi,bj, myThid)

      implicit none

C ==================== Global Variables ===========================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PROFILES
# include "netcdf.inc"
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif
C ==================== Routine Variables ==========================
      integer vec_loc_length, vNb, k, kk, kkk,bi,bj
      integer irec, fNb, myThid,err,varid1,tmpprofno
      _RL vec_loc(vec_loc_length)

#ifdef ALLOW_PROFILES

      integer vec_start(2),vec_count(2)
      _RL vec_tmp1(1000*NLEVELMAX),vec_tmp2(1000*NLEVELMAX)
#ifdef ALLOW_PROFILES_CLIMMASK
      _RL vec_tmp3(1000*NLEVELMAX)
#endif
      character*(max_len_mbuf) msgbuf

c--   == end of interface ==


      if ( (irec.LT.profiles_minind_buff(bi,bj)).OR.
     &  (irec.GT.profiles_maxind_buff(bi,bj)).OR.
     & (profiles_curfile_buff(bi,bj).NE.fNb) ) then
      err = NF_INQ_DIMID(fiddata(fNb,bi,bj),'iPROF', varid1)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_readvector 1:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_INQ_DIMLEN(fiddata(fNb,bi,bj), varid1, tmpprofno)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_readvector 2:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif

      if (profiles_curfile_buff(bi,bj).NE.fNb) then
c no asumption on whether a forward or a backward loop is calling
      profiles_minind_buff(bi,bj)=max(1,irec-500+1)
      profiles_maxind_buff(bi,bj)=min(tmpprofno,irec+500)
      elseif (irec.LT.profiles_minind_buff(bi,bj)) then
c implies that a backward loop is calling
      profiles_minind_buff(bi,bj)=max(1,irec-999)
      profiles_maxind_buff(bi,bj)=irec
      else
c implies that a forward loop is calling
      profiles_minind_buff(bi,bj)=irec
      profiles_maxind_buff(bi,bj)=min(tmpprofno,irec+999)
      endif

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
      write(msgbuf,'(a,5I9)')
     &   'buffer readvector ',
     &   profiles_minind_buff(bi,bj), profiles_maxind_buff(bi,bj),
     &   irec, profNo(fNb,bi,bj), tmpprofno
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      ENDIF
#endif

      vec_start(1)=1
      vec_start(2)=profiles_minind_buff(bi,bj)
      vec_count(1)=vec_loc_length
      vec_count(2)=
     & profiles_maxind_buff(bi,bj)-profiles_minind_buff(bi,bj)+1

      do kkk=1,NVARMAX
      if (vec_quantities(fNb,kkk,bi,bj).EQV..TRUE.) then
      err = NF_INQ_VARID(fiddata(fNb,bi,bj),prof_names(fNb,kkk),
     & varid1 )
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_readvector 3:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_GET_VARA_DOUBLE(fiddata(fNb,bi,bj), varid1 , vec_start,
     & vec_count, vec_tmp1)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_readvector 4:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_INQ_VARID(fiddata(fNb,bi,bj),prof_namesweight(fNb,kkk)
     & , varid1 )
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_readvector 5:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
      err = NF_GET_VARA_DOUBLE(fiddata(fNb,bi,bj), varid1 , vec_start,
     & vec_count, vec_tmp2)
        if (err /= NF_NOERR) then
            WRITE(msgBuf,'(a,i,a)') 'ESCO: p_readvector 6:',err,
     &      nf_strerror(err)
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT,myThid )
        endif
#ifdef ALLOW_PROFILES_CLIMMASK
      err = NF_INQ_VARID(fiddata(fNb,bi,bj),prof_namesclim(fNb,kkk)
     & , varid1 )
      err = NF_GET_VARA_DOUBLE(fiddata(fNb,bi,bj), varid1 , vec_start,
     & vec_count, vec_tmp3)
#endif

      if (err.NE.NF_NOERR) then
            WRITE(errorMessageUnit,'(A)')
     & 'WARNING in profiles_readvector: record not found!!'
      endif

      do k=1,vec_count(1)
      do kk=1,vec_count(2)
      profiles_data_buff(k,kk,kkk,bi,bj)=vec_tmp1((kk-1)*vec_count(1)+k)
      profiles_weight_buff(k,kk,kkk,bi,bj)=vec_tmp2((kk-1)*vec_count(1)
     & +k)
#ifdef ALLOW_PROFILES_CLIMMASK
      if(vec_tmp3((kk-1)*vec_count(1)+k).LE.-990. _d 0)
     &    profiles_weight_buff(k,kk,kkk,bi,bj) = 0. _d 0
#endif
      enddo
      enddo
      endif
      enddo

      profiles_curfile_buff(bi,bj)=fNb
      endif

cgf ...now, get vec_loc from the buffer
      if (vNb.LT.0) then
           do k=1,vec_loc_length
           vec_loc(k)= profiles_weight_buff
     & (k,irec-profiles_minind_buff(bi,bj)+1,-vNb,bi,bj)
           enddo

       else
           do k=1,vec_loc_length
           vec_loc(k)=profiles_data_buff
     & (k,irec-profiles_minind_buff(bi,bj)+1,vNb,bi,bj)
           enddo
       endif

#endif

      END
