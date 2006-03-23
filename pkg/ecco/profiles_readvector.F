#include "COST_CPPOPTIONS.h"

C     o==========================================================o
C     | subroutine profiles_readvector                           |
C     | o reads a profile from a netcdf profiles data file	 |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE profiles_readvector(fNb, vNb, irec, 
     & vec_loc_length, vec_loc , myThid)

      implicit none

C ==================== Global Variables ===========================
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "netcdf.inc"
#include "profiles.h"
C ==================== Routine Variables ==========================
      integer vec_loc_length, vNb, k, kk, kkk
      integer irec, fNb, myThid,err,varid1,tmpprofno
      _RL vec_loc(vec_loc_length)
      integer vec_start(2),vec_count(2)
      _RL vec_tmp1(1000*NLEVELMAX),vec_tmp2(1000*NLEVELMAX)

c--   == end of interface ==


      if ( (irec.LT.profiles_minind_buff).OR.
     &  (irec.GT.profiles_maxind_buff).OR.
     & (profiles_curfile_buff.NE.fNb) ) then
      err = NF_INQ_DIMID(fiddata(fNb),'iPROF', varid1)
      err = NF_INQ_DIMLEN(fiddata(fNb), varid1, tmpprofno)

      if (profiles_curfile_buff.NE.fNb) then
c no asumption on whether a forward or a backward loop is calling
      profiles_minind_buff=max(1,irec-500+1)
      profiles_maxind_buff=min(tmpprofno,irec+500)
      elseif (irec.LT.profiles_minind_buff) then
c implies that a backward loop is calling      
      profiles_minind_buff=max(1,irec-999)
      profiles_maxind_buff=irec
      else
c implies that a forward loop is calling
      profiles_minind_buff=irec
      profiles_maxind_buff=min(tmpprofno,irec+999)
      endif

      print*,"buffer readvector",profiles_minind_buff,
     & profiles_maxind_buff,irec,profno(fNb),tmpprofno

      vec_start(1)=1
      vec_start(2)=profiles_minind_buff
      vec_count(1)=vec_loc_length
      vec_count(2)=profiles_maxind_buff-profiles_minind_buff+1

      do kkk=1,4
      if (vec_quantities(fNb,kkk).EQV..TRUE.) then
      err = NF_INQ_VARID(fiddata(fNb),prof_names(kkk), varid1 )
      err = NF_GET_VARA_DOUBLE(fiddata(fNb), varid1 , vec_start, vec_count,
     & vec_tmp1)
      err = NF_INQ_VARID(fiddata(fNb),prof_namesweight(kkk), varid1 )
      err = NF_GET_VARA_DOUBLE(fiddata(fNb), varid1 , vec_start, vec_count,
     & vec_tmp2)

      if (err.NE.NF_NOERR) then
      print*,"stop readvector",vec_start(2),vec_count(2)
      stop
      endif

      do k=1,vec_count(1) 
      do kk=1,vec_count(2) 
      profiles_data_buff(k,kk,kkk)=vec_tmp1((kk-1)*vec_count(1)+k)
      profiles_weight_buff(k,kk,kkk)=vec_tmp2((kk-1)*vec_count(1)+k)
      enddo
      enddo 
      endif
      enddo

      profiles_curfile_buff=fNb
      endif

cgf ...now, get vec_loc from the buffer
      if (vNb.LT.0) then
           do k=1,vec_loc_length
           vec_loc(k)=
     & profiles_weight_buff(k,irec-profiles_minind_buff+1,-vNb)
           enddo

       else
           do k=1,vec_loc_length
           vec_loc(k)=
     & profiles_data_buff(k,irec-profiles_minind_buff+1,vNb)
           enddo
       endif 


      END
