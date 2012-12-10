C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_init_fixed.F,v 1.25 2012/12/10 00:01:30 gforget Exp $
C $Name:  $

#include "PROFILES_OPTIONS.h"
#include "AD_CONFIG.h"

C     *==========================================================*
C     | subroutine profiles_init_fixed
C     | o initialization for netcdf profiles data
C     | started: Gael Forget 15-March-2006
C     | extended: Gael Forget 14-June-2007
C     *==========================================================*

      SUBROUTINE profiles_init_fixed( myThid )

      implicit none

C ==================== Global Variables ===========================
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_CAL
#include "cal.h"
#endif
#ifdef ALLOW_PROFILES
# include "profiles.h"
# include "netcdf.inc"
#endif
C ==================== Routine Variables ==========================

      integer k,l,m,bi,bj,iG,jG, myThid,num_file,length_for_tile
      _RL stopProfiles
      integer fid, dimid, varid1, varid1a, varid1b
      integer varid2,varid3
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax

      integer tmpdate(4),tmpdiff(4),profIsInRunTime
      _RL  tmp_lon, tmp_lon2(1000), tmp_lat2(1000)
      integer vec_start(2), vec_count(2), profno_div1000, kk
      character*(80) profilesfile, fnamedatanc
      character*(80) fnameequinc, adfnameequinc
      integer IL, JL, err
      logical  exst

#ifdef ALLOW_PROFILES

#ifdef ALLOW_PROFILES_GENERICGRID
      integer varid_intp1, varid_intp2, varid_intp11 , varid_intp22
      integer varid_intp3, varid_intp4, varid_intp5, q
      _RL tmp_i(1000,NUM_INTERP_POINTS)
      _RL tmp_j(1000,NUM_INTERP_POINTS)
      _RL tmp_weights(1000,NUM_INTERP_POINTS),tmp_sum_weights
      _RL tmp_xC11(1000),tmp_yC11(1000)
      _RL tmp_xCNINJ(1000),tmp_yCNINJ(1000)
      _RL stopGenericGrid
      Real*8 xy_buffer_r8(0:sNx+1,0:sNy+1)
      integer vec_start2(2), vec_count2(2)
#endif

c     == external functions ==
      integer ILNBLNK
      EXTERNAL ILNBLNK
      integer MDS_RECLEN
      EXTERNAL MDS_RECLEN
      character*(max_len_mbuf) msgbuf

c--   == end of interface ==


      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// insitu profiles model sampling >>> START <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)


      stopProfiles=0. _d 0
#ifdef ALLOW_PROFILES_GENERICGRID
      stopGenericGrid=0. _d 0
#endif

#ifndef ALLOW_PROFILES_GENERICGRID
      IF ( profilesDoGenGrid ) THEN
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &  'profilesDoGenGrid=.true. requires'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &  'that ALLOW_PROFILES_GENERICGRID is defined'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF
#endif

      IF ( (.NOT.profilesDoGenGrid).AND.
     &     (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &  'profilesDoGenGrid=.true. is required'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &  'unless usingSphericalGrid=.TRUE. and rotateGrid=.FALSE.'
        CALL PRINT_ERROR( msgBuf , myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF


      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') 'general packages parameters :'
      JL  = ILNBLNK( profilesDir )
      if (JL.NE.0) then
        write(msgbuf,'(a,a)') '  profilesDir ',profilesDir(1:JL)
      else
        write(msgbuf,'(a,a)') '  profilesDir ','./'
      endif
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,a)') '  ALLOW_PROFILES_GENERICGRID ',
#ifdef ALLOW_PROFILES_GENERICGRID
     &   'was compiled'
#else
     &   'was NOT compiled'
#endif
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,l5)') '  profilesDoGenGrid  ',profilesDoGenGrid
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,l5)') '  profilesDoNcOutput ',profilesDoNcOutput
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)


      _BEGIN_MASTER( mythid )
      DO bj=1,nSy
      DO bi=1,nSx

        profiles_curfile_buff(bi,bj)=0
        yymmddMin=modelstartdate(1)
        yymmddMax=modelenddate(1)
        hhmmssMin=modelstartdate(2)
        hhmmssMax=modelenddate(2)

        do m=1,NLEVELMAX
         do l=1,1000
          do k=1,NVARMAX
           profiles_data_buff(m,l,k,bi,bj)=0
           profiles_weight_buff(m,l,k,bi,bj)=0
          enddo
         enddo
        enddo

        do num_file=1,NFILESPROFMAX

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)

      IL  = ILNBLNK( profilesfiles(num_file) )
      if (IL.NE.0) then
        write(profilesfile(1:80),'(1a)')
     &     profilesfiles(num_file)(1:IL)
        write(msgbuf,'(a,i3,a,a)')
     &     'profiles file ',num_file,' is ', profilesfile(1:80)
        call print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      else
        write(profilesfile(1:80),'(1a)') ' '
        write(msgbuf,'(a,i3,a,a)')
     &     'profiles file ',num_file,' is ',' (empty) '
        call print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      endif

      IL  = ILNBLNK( profilesfile )
      if (IL.NE.0) then

C===========================================================
c open data files and read information
C===========================================================

      write(fnamedatanc(1:80),'(2a)') profilesfile(1:IL),'.nc'
      write(msgbuf,'(a,a)') '  opening ', fnamedatanc(1:80)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      err = NF_OPEN(fnamedatanc, 0, fiddata(num_file,bi,bj))

c1)  read the number of profiles :
      fid=fiddata(num_file,bi,bj)
      err = NF_INQ_DIMID(fid,'iPROF', dimid )
      err = NF_INQ_DIMLEN(fid, dimid, ProfNo(num_file,bi,bj) )
      err = NF_INQ_DIMID(fid,'iDEPTH', dimid )
      if (err.NE.NF_NOERR) then
        err = NF_INQ_DIMID(fid,'Z', dimid )
      endif
      err = NF_INQ_DIMLEN(fid, dimid, ProfDepthNo(num_file,bi,bj) )

      write(msgbuf,'(a,i9)')
     &   '  file ID is        ', fid
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i5)')
     &   '  no. of depth levels   ',ProfDepthNo(num_file,bi,bj)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9)')
     &   '  no. of profiles   ', ProfNo(num_file,bi,bj)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)

c2) read the dates and positions :
      err = NF_INQ_VARID(fid,'prof_depth', varid1a )
      if (err.NE.NF_NOERR) then
c       if no prof_depth is found, then try old variable name:
        err = NF_INQ_VARID(fid,'depth', varid1a )
      endif
      if (err.NE.NF_NOERR) then
c       if neither is found, then stop
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc is not in the pkg/profiles format (no prof_depth etc.)'
        CALL PRINT_ERROR( msgBuf, myThid)
        stopProfiles=1. _d 0
      endif

      do k=1,ProfDepthNo(num_file,bi,bj)
      err = NF_GET_VAR1_DOUBLE(fid,varid1a,k,
     & prof_depth(num_file,k,bi,bj))
      enddo

      err = NF_INQ_VARID(fid,'prof_YYYYMMDD', varid1a )
      err = NF_INQ_VARID(fid,'prof_HHMMSS', varid1b )
      err = NF_INQ_VARID(fid,'prof_lon', varid2 )
      err = NF_INQ_VARID(fid,'prof_lat', varid3 )

      if (err.NE.NF_NOERR) then
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc is not in the pkg/profiles format (no prof_YYYYMMDD etc.)'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopProfiles=1. _d 0
      endif

#ifdef ALLOW_PROFILES_GENERICGRID
      if (profilesDoGenGrid) then
c3) read interpolattion information (grid points, coeffs, etc.)
           err = NF_INQ_VARID(fid,'prof_interp_XC11',varid_intp1)
           err = NF_INQ_VARID(fid,'prof_interp_YC11',varid_intp2)
           err = NF_INQ_VARID(fid,'prof_interp_XCNINJ',varid_intp11)
           err = NF_INQ_VARID(fid,'prof_interp_YCNINJ',varid_intp22)
           err = NF_INQ_VARID(fid,'prof_interp_weights',varid_intp3)
           err = NF_INQ_VARID(fid,'prof_interp_i',varid_intp4)
           err = NF_INQ_VARID(fid,'prof_interp_j',varid_intp5)
      if (err.NE.NF_NOERR) then
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc is missing interpolation information (profilesDoGenGrid)'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopGenericGrid=2. _d 0
      endif
      endif
#endif


c4) default values
      do k=1,NOBSGLOB
      prof_time(num_file,k,bi,bj)=-999
      prof_lon(num_file,k,bi,bj)=-999
      prof_lat(num_file,k,bi,bj)=-999
      prof_ind_glob(num_file,k,bi,bj)=-999
#ifdef ALLOW_PROFILES_GENERICGRID
      do q = 1,NUM_INTERP_POINTS
         prof_interp_i(num_file,k,q,bi,bj) = -999
         prof_interp_j(num_file,k,q,bi,bj) = -999
         prof_interp_weights(num_file,k,q,bi,bj) = -999
      enddo
      prof_interp_xC11(num_file,k,bi,bj)=-999
      prof_interp_yC11(num_file,k,bi,bj)=-999
      prof_interp_xCNINJ(num_file,k,bi,bj)=-999
      prof_interp_yCNINJ(num_file,k,bi,bj)=-999
#endif
      enddo


c5) main loop: look for profiles in this tile
      length_for_tile=0
      profno_div1000=max(0,int(ProfNo(num_file,bi,bj)/1000))

      do kk=1,profno_div1000+1

      if (min(ProfNo(num_file,bi,bj), 1000*kk).GE.
     &  1+1000*(kk-1)) then

c5.1) read a chunk
      vec_start(1)=1
      vec_start(2)=1+1000*(kk-1)
      vec_count(1)=1
      vec_count(2)=min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))

      if ( (vec_count(2).LE.0).OR.(vec_count(2).GT.1000).OR.
     & (vec_start(2).LE.0).OR.
     & (vec_count(2)+vec_start(2)-1.GT.ProfNo(num_file,bi,bj)) )
     & then
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc was not read properly (case 1).'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopProfiles=1. _d 0
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
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc was not read properly (case 2).'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopProfiles=1. _d 0
      endif

#ifdef ALLOW_PROFILES_GENERICGRID
      if (profilesDoGenGrid) then
      err = NF_GET_VARA_DOUBLE(fid,varid_intp1,vec_start(2),
     & vec_count(2), tmp_xC11)
      err = NF_GET_VARA_DOUBLE(fid,varid_intp2,vec_start(2),
     & vec_count(2), tmp_yC11)
      err = NF_GET_VARA_DOUBLE(fid,varid_intp11,vec_start(2),
     & vec_count(2), tmp_xCNINJ)
      err = NF_GET_VARA_DOUBLE(fid,varid_intp22,vec_start(2),
     & vec_count(2), tmp_yCNINJ)
      do q=1,NUM_INTERP_POINTS
        vec_start2(1)=q
        vec_start2(2)=1+1000*(kk-1)
        vec_count2(1)=1
        vec_count2(2)=min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))
        err = NF_GET_VARA_DOUBLE(fid,varid_intp3,vec_start2,
     &  vec_count2, tmp_weights(1,q))
        err = NF_GET_VARA_DOUBLE(fid,varid_intp4,vec_start2,
     &  vec_count2, tmp_i(1,q))
        err = NF_GET_VARA_DOUBLE(fid,varid_intp5,vec_start2,
     &  vec_count2, tmp_j(1,q))
      enddo
      endif
#endif

c5.2) loop through this chunk
      do k=1,min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))

      if ( stopProfiles .EQ. 0.) then

      profIsInRunTime=1

      if (( ( tmpyymmdd(k).GT.yymmddMin ).OR.(( tmpyymmdd(k).EQ.
     &        yymmddMin ).AND.( tmphhmmss(k).GT.hhmmssMin ))).AND.
     &    ( ( tmpyymmdd(k).LT.yymmddMax ).OR.(( tmpyymmdd(k).EQ.
     &        yymmddMax ).AND.( tmphhmmss(k).LT.hhmmssMax ))) ) then
        profIsInRunTime=1
        call cal_FullDate( int(tmpyymmdd(k)),int(tmphhmmss(k)),
     &   tmpdate,mythid )
        call cal_TimePassed( modelstartdate,tmpdate,tmpdiff,mythid )
        call cal_ToSeconds (tmpdiff,diffsecs,mythid)
        diffsecs=diffsecs+nIter0*deltaTclock
      else
        profIsInRunTime=0
        diffsecs=-deltaTclock
      endif

#ifdef ALLOW_PROFILES_GENERICGRID
       if (.NOT.profilesDoGenGrid) then
#endif
       if (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) then
        tmp_lon=xC(sNx+1,1,bi,bj)+360
       else
        tmp_lon=xC(sNx+1,1,bi,bj)
       endif
       if ((xC(1,1,bi,bj).LE.tmp_lon2(k)).AND.
     & (tmp_lon.GT.tmp_lon2(k)).AND.
     & (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     & (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k))
     & .AND.(profIsInRunTime.EQ.1)) then
        length_for_tile=length_for_tile+1
        prof_time(num_file,length_for_tile,bi,bj)=diffsecs
        prof_lon(num_file,length_for_tile,bi,bj)=tmp_lon2(k)
        prof_lat(num_file,length_for_tile,bi,bj)=tmp_lat2(k)
        prof_ind_glob(num_file,length_for_tile,bi,bj)=k+1000*(kk-1)
        if (length_for_tile.EQ.NOBSGLOB) then
          WRITE(msgBuf,'(3A)')
     &    'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &    '.nc was not read properly (increase NOBSGLOB).'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopProfiles=1. _d 0
        endif
       elseif (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) then
        if ((xC(1,1,bi,bj).LE.tmp_lon2(k)+360).AND.
     &  (tmp_lon.GT.tmp_lon2(k)+360).AND.
     &  (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     &  (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k))
     &  ) then
         length_for_tile=length_for_tile+1
         prof_time(num_file,length_for_tile,bi,bj)=diffsecs
         prof_lon(num_file,length_for_tile,bi,bj)=tmp_lon2(k)+360
         prof_lat(num_file,length_for_tile,bi,bj)=tmp_lat2(k)
         prof_ind_glob(num_file,length_for_tile,bi,bj)=k+1000*(kk-1)
         if (length_for_tile.EQ.NOBSGLOB) then
           WRITE(msgBuf,'(3A)')
     &      'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &      '.nc was not read properly (increase NOBSGLOB).'
           CALL PRINT_ERROR( msgBuf, myThid)
           stopProfiles=1. _d 0
         endif
        endif
       endif
#ifdef ALLOW_PROFILES_GENERICGRID
       else
       if (stopGenericGrid.EQ.0.) then

       if ( ( abs( tmp_xC11(k) - xC(1,1,bi,bj) ).LT.0.0001 ) .AND.
     & ( abs( tmp_yC11(k) - yC(1,1,bi,bj) ).LT.0.0001 ) .AND.
     & ( abs( tmp_xCNINJ(k) - xC(sNx,sNy,bi,bj) ).LT.0.0001 ) .AND.
     & ( abs( tmp_yCNINJ(k) - yC(sNx,sNy,bi,bj) ).LT.0.0001 )
     & .AND.(profIsInRunTime.EQ.1)) then

         length_for_tile=length_for_tile+1
         prof_time(num_file,length_for_tile,bi,bj)=diffsecs
         prof_interp_xC11(num_file,length_for_tile,bi,bj)=tmp_xC11(k)
         prof_interp_yC11(num_file,length_for_tile,bi,bj)=tmp_yC11(k)
       prof_interp_xCNINJ(num_file,length_for_tile,bi,bj)=tmp_xCNINJ(k)
       prof_interp_yCNINJ(num_file,length_for_tile,bi,bj)=tmp_yCNINJ(k)
       tmp_sum_weights=0. _d 0
        do q = 1,NUM_INTERP_POINTS
             prof_interp_weights(num_file,length_for_tile,q,bi,bj)
     &       =tmp_weights(k,q)
             prof_interp_i(num_file,length_for_tile,q,bi,bj)
     &       =tmp_i(k,q)
             prof_interp_j(num_file,length_for_tile,q,bi,bj)
     &       =tmp_j(k,q)
             tmp_sum_weights=tmp_sum_weights+tmp_weights(k,q)
c more test of the inputs: is the offline-computed
c interpolation information consistent (self and with grid)
       if ( (tmp_i(k,q).LT.0).OR.(tmp_j(k,q).LT.0)
     & .OR.(tmp_i(k,q).GT.sNx+1).OR.(tmp_j(k,q).GT.sNy+1) ) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'points (profilesDoGenGrid; out of tile)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1. _d 0
       endif
       if ( tmp_weights(k,q) .NE. 0. ) then
       if ( ((tmp_i(k,q).EQ.0).AND.(tmp_j(k,q).EQ.0))
     & .OR.((tmp_i(k,q).EQ.sNx+1).AND.(tmp_j(k,q).EQ.sNy+1))
     & .OR.((tmp_i(k,q).EQ.0).AND.(tmp_j(k,q).EQ.sNy+1))
     & .OR.((tmp_i(k,q).EQ.sNx+1).AND.(tmp_j(k,q).EQ.0)) ) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'points (profilesDoGenGrid; using overlap corners)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1. _d 0
       endif
       endif
       if ( (tmp_weights(k,q).LT.0).OR.(tmp_weights(k,q).GT.1) ) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'weights (profilesDoGenGrid; sum oustide 0-1)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1. _d 0
       endif

       enddo

       if ( abs(tmp_sum_weights -1. ) .GT. 0.0001 ) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'weights (profilesDoGenGrid; dont add up to 1)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1. _d 0
       endif

         prof_ind_glob(num_file,length_for_tile,bi,bj)=k+1000*(kk-1)
         if (length_for_tile.EQ.NOBSGLOB) then
          WRITE(msgBuf,'(3A)')
     &    'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &    '.nc was not read properly (increase NOBSGLOB).'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopProfiles=1. _d 0
         endif

       endif
       endif
       endif   !if (.NOT.profilesDoGenGrid) then
#endif /* ALLOW_PROFILES_GENERICGRID */
      endif    !if ( stopProfiles .EQ. 0.) then
      enddo    !do k=1,min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))
      endif    !if (min(ProfNo(num_file,bi,bj), 1000...
      enddo    !do kk=1,profno_div1000+1

      ProfNo(num_file,bi,bj)=length_for_tile

      write(msgbuf,'(a,i9)')
     &   '  within tile & run ', ProfNo(num_file,bi,bj)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)

c6) available variablesin the data set

      do k=1,NVARMAX
      prof_num_var_cur(num_file,k,bi,bj)=0
      enddo
      prof_num_var_tot(num_file,bi,bj)=0

      err = NF_INQ_VARID(fid,'prof_T', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,1,bi,bj)=.TRUE.
      prof_num_var_tot(num_file,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)+1
      prof_num_var_cur(num_file,1,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)
      else
      vec_quantities(num_file,1,bi,bj)=.FALSE.
      endif
      err = NF_INQ_VARID(fid,'prof_S', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,2,bi,bj)=.TRUE.
      prof_num_var_tot(num_file,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)+1
      prof_num_var_cur(num_file,2,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)
      else
      vec_quantities(num_file,2,bi,bj)=.FALSE.
      endif
#ifndef ALLOW_PROFILES_GENERICGRID
cgf This bloc wont work when model u/v are not
cgf zonal/meridional components, while prof_U/V are.
      err = NF_INQ_VARID(fid,'prof_U', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,3,bi,bj)=.TRUE.
      prof_num_var_tot(num_file,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)+1
      prof_num_var_cur(num_file,3,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)
      else
      vec_quantities(num_file,3,bi,bj)=.FALSE.
      endif
      err = NF_INQ_VARID(fid,'prof_V', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,4,bi,bj)=.TRUE.
      prof_num_var_tot(num_file,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)+1
      prof_num_var_cur(num_file,4,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)
      else
      vec_quantities(num_file,4,bi,bj)=.FALSE.
      endif
#endif
      err = NF_INQ_VARID(fid,'prof_ptr', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,5,bi,bj)=.TRUE.
      prof_num_var_tot(num_file,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)+1
      prof_num_var_cur(num_file,5,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)
      else
      vec_quantities(num_file,5,bi,bj)=.FALSE.
      endif
      err = NF_INQ_VARID(fid,'prof_ssh', varid1 )
      if (err.EQ.NF_NOERR) then
      vec_quantities(num_file,6,bi,bj)=.TRUE.
      prof_num_var_tot(num_file,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)+1
      prof_num_var_cur(num_file,6,bi,bj)=
     & prof_num_var_tot(num_file,bi,bj)
      else
      vec_quantities(num_file,6,bi,bj)=.FALSE.
      endif

      write(msgbuf,'(a,6L5)') '  incl. variables (T/F flags)'
     &,vec_quantities(num_file,1,bi,bj),vec_quantities(num_file,2,bi,bj)
     &,vec_quantities(num_file,3,bi,bj),vec_quantities(num_file,4,bi,bj)
     &,vec_quantities(num_file,5,bi,bj),vec_quantities(num_file,6,bi,bj)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)

C===========================================================
c create files for model counterparts to observations
C===========================================================

           if (ProfNo(num_file,bi,bj).GT.0) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles

      JL  = ILNBLNK( profilesDir )

      if (profilesDoNcOutput) then

      write(fnameequinc(1:80),'(3a,i3.3,a,i3.3,a)')
     & profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
      write(adfnameequinc(1:80),'(4a,i3.3,a,i3.3,a)')
     & profilesDir(1:JL),'ad',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'

      inquire( file=fnameequinc, exist=exst )
      if (.NOT.exst) then
        call profiles_init_ncfile(num_file,
     &   fiddata(num_file,bi,bj),fnameequinc,
     &   fidforward(num_file,bi,bj),ProfNo(num_file,bi,bj),
     &   ProfDepthNo(num_file,bi,bj),
     &   bi,bj,myThid)
      else
        err = NF_OPEN(fnameequinc,NF_WRITE,fidforward(num_file,bi,bj))
      endif
#ifdef ALLOW_ADJOINT_RUN
      inquire( file=adfnameequinc, exist=exst )
      if (.NOT.exst) then
        call profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   adfnameequinc, fidadjoint(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     & bi,bj, myThid)
      else
        err = NF_OPEN(adfnameequinc,NF_WRITE,fidadjoint(num_file,bi,bj))
      endif
#endif
      else

      write(fnameequinc(1:80),'(3a,i3.3,a,i3.3,a)')
     & profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.data'
      write(adfnameequinc(1:80),'(4a,i3.3,a,i3.3,a)')
     & profilesDir(1:JL),'ad',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.data'

      inquire( file=fnameequinc, exist=exst )
         fidforward(num_file,bi,bj)=
     &     2000+(num_file-1)*nSx*nSx+(bi-1)*nSy+bj
      if (.NOT.exst) then
        call profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   fnameequinc,fidforward(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj,myThid)
      else
c        call MDSFINDUNIT( fidforward(num_file,bi,bj) , mythid )
         open( fidforward(num_file,bi,bj),file=fnameequinc,
     &   form ='unformatted',status='unknown', access='direct',
     &   recl=  (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
      endif
#ifdef ALLOW_ADJOINT_RUN
      inquire( file=adfnameequinc, exist=exst )
         fidadjoint(num_file,bi,bj)=
     &     4000+(num_file-1)*nSx*nSx+(bi-1)*nSy+bj
      if (.NOT.exst) then
        call profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   adfnameequinc, fidadjoint(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj, myThid)
      else
c        call MDSFINDUNIT( fidadjoint(num_file,bi,bj) , mythid )
         open( fidadjoint(num_file,bi,bj),file=adfnameequinc,
     &   form ='unformatted',status='unknown', access='direct',
     &   recl=  (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
      endif
#endif

      endif

           endif


C===========================================================
      else
      ProfNo(num_file,bi,bj)=0
      do k=1,NVARMAX
      prof_num_var_cur(num_file,k,bi,bj)=0
      vec_quantities(num_file,k,bi,bj)=.FALSE.
      enddo
      prof_num_var_tot(num_file,bi,bj)=0
      do k=1,NOBSGLOB
      prof_time(num_file,k,bi,bj)=-999
      prof_lon(num_file,k,bi,bj)=-999
      prof_lat(num_file,k,bi,bj)=-999
      prof_ind_glob(num_file,k,bi,bj)=-999
#ifdef ALLOW_PROFILES_GENERICGRID
      do q = 1,NUM_INTERP_POINTS
         prof_interp_i(num_file,k,q,bi,bj) = -999
         prof_interp_j(num_file,k,q,bi,bj) = -999
         prof_interp_weights(num_file,k,q,bi,bj) = -999
      enddo
      prof_interp_xC11(num_file,k,bi,bj)=-999
      prof_interp_yC11(num_file,k,bi,bj)=-999
      prof_interp_xCNINJ(num_file,k,bi,bj)=-999
      prof_interp_yCNINJ(num_file,k,bi,bj)=-999
#endif
      enddo

      endif !if (IL.NE.0) then
      enddo !      do num_file=1,NFILESPROFMAX

C===========================================================
C error cases:
C===========================================================

#ifdef ALLOW_PROFILES_GENERICGRID

c1) you want to provide interpolation information

       if ( stopGenericGrid.EQ.2.) then
         iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
         jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
cgf XC grid
       call MDSFINDUNIT( fid , mythid )
       write(fnameequinc(1:80),'(a,i3.3,a,i3.3,a,i4.4,a,i4.4,a)')
     & 'profilesXCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
         k=MDS_RECLEN(64,(sNx+2)*(sNy+2),mythid)
            WRITE(standardMessageUnit,'(A,/,2A)')
     & 'PROFILES_INIT_FIXED: creating grid from profiles; file:',
     & fnameequinc
       open( fid, file= fnameequinc, form ='unformatted',
     &      status='unknown',access='direct', recl= k)
        DO m=0,sNy+1
         DO l=0,sNx+1
        xy_buffer_r8(l,m)=xC(l,m,bi,bj)
         ENDDO
        ENDDO
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8((sNx+2)*(sNy+2),xy_buffer_r8)
#endif
       write(fid,rec=1) xy_buffer_r8
       close(fid)
cgf YC grid
       call MDSFINDUNIT( fid , mythid )
       write(fnameequinc(1:80),'(a,i3.3,a,i3.3,a,i4.4,a,i4.4,a)')
     & 'profilesYCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
         k=MDS_RECLEN(64,(sNx+2)*(sNy+2),mythid)
            WRITE(standardMessageUnit,'(A,/,A)')
     & 'PROFILES_INIT_FIXED: creating grid from profiles; file:',
     & fnameequinc
       open( fid, file= fnameequinc, form ='unformatted',
     & status='unknown', access='direct', recl= k)
        DO m=0,sNy+1
         DO l=0,sNx+1
                xy_buffer_r8(l,m)=yC(l,m,bi,bj)
         ENDDO
        ENDDO
#ifdef _BYTESWAPIO
            call MDS_BYTESWAPR8((sNx+2)*(sNy+2),xy_buffer_r8)
#endif
       write(fid,rec=1) xy_buffer_r8
       close(fid)

       WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED : ',
     & 'when using ALLOW_PROFILES_GENERICGRID ',
     & 'you have to provide interpolation coeffs etc. '
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(2A)')
     & 'and some of your nc files dont have them. ',
     & 'You could use profiles_prep_mygrid.m and/or'
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(A)')
     & 'use the grid info in profiles*incl1PointOverlap*data'
       CALL PRINT_ERROR( msgBuf, myThid)
       stopProfiles=1. _d 0

      endif

#endif

      ENDDO
      ENDDO

      _END_MASTER( mythid )
      _BARRIER

c2) stop after other kind of errors
      _GLOBAL_SUM_RL( stopProfiles , myThid )
      if ( stopProfiles.GE.1.) then
             STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      endif
#ifdef ALLOW_PROFILES_GENERICGRID
      _GLOBAL_SUM_RL( stopGenericGrid , myThid )
      if ( stopGenericGrid.GE.1.) then
             STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      endif
#endif


      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// insitu profiles model sampling >>> END <<<'
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)')
     &'// ======================================================='
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)


#endif

      RETURN
      END
