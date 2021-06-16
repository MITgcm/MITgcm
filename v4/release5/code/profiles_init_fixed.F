C $Header: /u/gcmpack/MITgcm/pkg/profiles/profiles_init_fixed.F,v 1.39 2017/04/03 23:16:38 ou.wang Exp $
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
# include "PROFILES_SIZE.h"
# include "profiles.h"
# include "netcdf.inc"
#endif

C ==================== Routine Arguments ==========================

      integer myThid

C ==================== Routine Variables ==========================

#ifdef ALLOW_PROFILES

      integer i,j,k,l,m,bi,bj,iG,jG,num_file,ProfNo_tile
      integer stopProfiles
      integer fid, dimid, varid1, varid1a, varid1b
      integer varid2,varid3
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax

      integer tmpdate(4),tmpdiff(4),profIsInRunTime
      _RL  tmp_lon, tmp_lon2(1000), tmp_lat2(1000), lon_cur, lat_cur
      _RL lon_1, lon_2, lat_1, lat_2
      _RL lon_tmp1, lon_tmp2
      _RL lat_fac, lon_fac
      integer prof_i, prof_j
      integer vec_start(2), vec_count(2), profno_div1000, kk
      character*(80) profilesfile, fnamedatanc
      character*(80) fnameequinc, adfnameequinc
      integer IL, JL, KL, err
      logical  exst

      integer varid_intp1, varid_intp2, varid_intp11 , varid_intp22
      integer varid_intp3, varid_intp4, varid_intp5, q, iINTERP
      _RL tmp_i(1000,NUM_INTERP_POINTS)
      _RL tmp_j(1000,NUM_INTERP_POINTS)
      _RL tmp_weights(1000,NUM_INTERP_POINTS),tmp_sum_weights
      _RL tmp_xC11(1000),tmp_yC11(1000)
      _RL tmp_xCNINJ(1000),tmp_yCNINJ(1000)
      integer stopGenericGrid
      Real*8 xy_buffer_r8(0:sNx+1,0:sNy+1)
      integer vec_start2(2), vec_count2(2)
      integer hh, ProfNo_hh
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      integer varid4
      _RL tmp_avgbin(1000)
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

      stopProfiles=0
      stopGenericGrid=0

      IF ( (.NOT.profilesDoGenGrid).AND.
     &     (.NOT.usingSphericalPolarGrid .OR. rotateGrid) ) THEN
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &  'profilesDoGenGrid=.true. is required'
        CALL PRINT_ERROR( msgBuf , myThid )
        WRITE(msgBuf,'(2A)') 'PROFILES_INIT_FIXED: ',
     &  'unless usingSphericalGrid=.TRUE. and rotateGrid=.FALSE.'
        CALL PRINT_ERROR( msgBuf , myThid )
        CALL ALL_PROC_DIE( myThid )
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
           profiles_data_buff(m,l,k,bi,bj)=0. _d 0
           profiles_weight_buff(m,l,k,bi,bj)=0. _d 0
          enddo
         enddo
        enddo

        do num_file=1,NFILESPROFMAX

      ProfNo_hh=0

      profilesfile=' '
      IL  = ILNBLNK( profilesfiles(num_file) )
      if (IL.NE.0) then
        write(profilesfile(1:80),'(1a)')
     &     profilesfiles(num_file)(1:IL)
        write(msgbuf,'(a)') ' '
        call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
        write(msgbuf,'(a,i3,a,a)')
     &     'profiles file #',num_file,' is ', profilesfile(1:IL)
        call print_message(
     &     msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      endif

      IL  = ILNBLNK( profilesfile )
      if (IL.NE.0) then

C===========================================================
c open data files and read information
C===========================================================

      write(fnamedatanc(1:80),'(2a)') profilesfile(1:IL),'.nc'
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
      if(ProfDepthNo(num_file,bi,bj).GT.NLEVELMAX)then
       write(msgbuf,'(a,i9)')
     &   '  ERROR: # of depth levels in file too large   =',
     &   ProfDepthNo(num_file,bi,bj)
       CALL PRINT_ERROR( msgBuf, myThid)
       stopProfiles=1
      endif
      err = NF_INQ_DIMID(fid,'iINTERP', dimid )
      if (err.EQ.NF_NOERR) then
        err = NF_INQ_DIMLEN(fid, dimid, iINTERP )
      else
        iINTERP=NUM_INTERP_POINTS
      endif

      write(msgbuf,'(a,i4,a,i4)')
     &   '  current tile is bi,bj                      =',
     &   bi,',',bj
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9)')
     &   '  # of depth levels in file                  =',
     &   ProfDepthNo(num_file,bi,bj)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)
      write(msgbuf,'(a,i9)')
     &   '  # of profiles in file                      =',
     &   ProfNo(num_file,bi,bj)
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
        stopProfiles=1
      endif

      do k=1,ProfDepthNo(num_file,bi,bj)
      err = NF_GET_VAR1_DOUBLE(fid,varid1a,k,
     & prof_depth(num_file,k,bi,bj))
      enddo

      err = NF_INQ_VARID(fid,'prof_YYYYMMDD', varid1a )
      err = NF_INQ_VARID(fid,'prof_HHMMSS', varid1b )
      err = NF_INQ_VARID(fid,'prof_lon', varid2 )
      err = NF_INQ_VARID(fid,'prof_lat', varid3 )
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      err = NF_INQ_VARID(fid,'prof_bin_id_a', varid4 )
#endif

      if (err.NE.NF_NOERR) then
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc is not in the pkg/profiles format (no prof_YYYYMMDD etc.)'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopProfiles=1
      endif

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
      stopGenericGrid=2
      endif
      endif

c4) default values
      do k=1,NOBSGLOB
      prof_time(num_file,k,bi,bj)=-999. _d 0
      prof_lon(num_file,k,bi,bj)=-999. _d 0
      prof_lat(num_file,k,bi,bj)=-999. _d 0
      prof_ind_glob(num_file,k,bi,bj)=0
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      prof_ind_avgbin(num_file,k,bi,bj)=-999
#endif
      do q = 1,NUM_INTERP_POINTS
         prof_interp_i(num_file,k,q,bi,bj) = 1
         prof_interp_j(num_file,k,q,bi,bj) = 1
         prof_interp_weights(num_file,k,q,bi,bj) = 0. _d 0
      enddo
      prof_interp_xC11(num_file,k,bi,bj)=-999. _d 0
      prof_interp_yC11(num_file,k,bi,bj)=-999. _d 0
      prof_interp_xCNINJ(num_file,k,bi,bj)=-999. _d 0
      prof_interp_yCNINJ(num_file,k,bi,bj)=-999. _d 0
      enddo

c5) main loop: look for profiles in this tile
      ProfNo_tile=0
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
      stopProfiles=1
      endif

      err = NF_GET_VARA_DOUBLE(fid,varid1a,vec_start(2),
     & vec_count(2), tmpyymmdd)
      err = NF_GET_VARA_DOUBLE(fid,varid1b,vec_start(2),
     & vec_count(2), tmphhmmss)
      err = NF_GET_VARA_DOUBLE(fid,varid2,vec_start(2),
     & vec_count(2), tmp_lon2)
      err = NF_GET_VARA_DOUBLE(fid,varid3,vec_start(2),
     & vec_count(2), tmp_lat2)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      err = NF_GET_VARA_DOUBLE(fid,varid4,vec_start(2),
     & vec_count(2), tmp_avgbin)
#endif

      if (err.NE.NF_NOERR) then
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc was not read properly (case 2).'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopProfiles=1
      endif

c if profilesDoGenGrid then also read in the interpolation coeffs and indices
      if (profilesDoGenGrid) then
      err = NF_GET_VARA_DOUBLE(fid,varid_intp1,vec_start(2),
     & vec_count(2), tmp_xC11)
      err = NF_GET_VARA_DOUBLE(fid,varid_intp2,vec_start(2),
     & vec_count(2), tmp_yC11)
      err = NF_GET_VARA_DOUBLE(fid,varid_intp11,vec_start(2),
     & vec_count(2), tmp_xCNINJ)
      err = NF_GET_VARA_DOUBLE(fid,varid_intp22,vec_start(2),
     & vec_count(2), tmp_yCNINJ)
      do q=1,iINTERP
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

c5.2) loop through this chunk
      do k=1,min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))

      if ( stopProfiles .EQ. 0) then

      profIsInRunTime=1

      IF (( ( tmpyymmdd(k).GT.yymmddMin ).OR.(( tmpyymmdd(k).EQ.
     &        yymmddMin ).AND.( tmphhmmss(k).GT.hhmmssMin ))).AND.
     &    ( ( tmpyymmdd(k).LT.yymmddMax ).OR.(( tmpyymmdd(k).EQ.
     &        yymmddMax ).AND.( tmphhmmss(k).LT.hhmmssMax ))) .AND.
     &   tmpyymmdd(k).GE.0. _d 0 .AND.
     &   tmphhmmss(k).GE.0. _d 0) THEN
        hh = int(tmphhmmss(k))/10000
        IF ( hh.LT.hoursPerDay ) THEN
          profIsInRunTime=1
          call cal_FullDate( int(tmpyymmdd(k)),int(tmphhmmss(k)),
     &     tmpdate,mythid )
          call cal_TimePassed( modelstartdate,tmpdate,tmpdiff,mythid )
          call cal_ToSeconds (tmpdiff,diffsecs,mythid)
          diffsecs=diffsecs+nIter0*deltaTclock
        ELSE
c if tmphhmmss is out of range then disregard profile
          profIsInRunTime=0
          diffsecs=-deltaTclock
          ProfNo_hh=ProfNo_hh+1
        ENDIF
      ELSE
        profIsInRunTime=0
        diffsecs=-deltaTclock
      ENDIF

c ==============================================================================

c 5.2a) determine whether profiles is in current tile domain (lat-lon grid case)
       if ((.NOT.profilesDoGenGrid).AND.(profIsInRunTime.EQ.1)) then

       if (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) then
        tmp_lon=xC(sNx+1,1,bi,bj)+360. _d 0
       else
        tmp_lon=xC(sNx+1,1,bi,bj)
       endif

       if ((xC(1,1,bi,bj).LE.tmp_lon2(k)).AND.
     & (tmp_lon.GT.tmp_lon2(k)).AND.
     & (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     & (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k)) ) then
         lon_cur=tmp_lon2(k)
         lat_cur=tmp_lat2(k)
       elseif ((xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)).AND.
     &  (xC(1,1,bi,bj).LE.tmp_lon2(k)+360. _d 0).AND.
     &  (tmp_lon.GT.tmp_lon2(k)+360. _d 0).AND.
     &  (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     &  (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k))
     &  ) then
         lon_cur=tmp_lon2(k)+360. _d 0
         lat_cur=tmp_lat2(k)
       else
         profIsInRunTime=0
       endif

c now determine value of i,j to the south-ouest of data point
       prof_i=-10
       prof_j=-10
       lon_1=-10
       lon_2=-10
       lat_1=-10
       lat_2=-10

       if (profIsInRunTime.EQ.1) then
        DO j=1,sNy+1
         DO i=1,sNx+1

c value of j, south of the data point:
        if ((yC(i,j,bi,bj).LE.lat_cur).AND.
     &      (yC(i,j+1,bi,bj).GT.lat_cur)) then
          prof_j=j
          lat_1=yC(i,j,bi,bj)
          lat_2=yC(i,j+1,bi,bj)
        endif

c value of i, west of the data point:
         if (xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj)) then
           lon_tmp2=xC(i+1,j,bi,bj)+360
         else
           lon_tmp2=xC(i+1,j,bi,bj)
         endif
         if (xC(i,j,bi,bj).LT.xC(1,j,bi,bj)) then
           lon_tmp1=xC(i,j,bi,bj)+360
         else
           lon_tmp1=xC(i,j,bi,bj)
         endif

         if ((lon_tmp1.LE.lon_cur).AND.(lon_tmp2.GT.lon_cur)) then
           prof_i=i
           lon_1=lon_tmp1
           lon_2=lon_tmp2
         endif

        ENDDO
       ENDDO
      endif

      if ((prof_i.EQ.-10).OR.(prof_j.EQ.-10)) profIsInRunTime=0

      if (profIsInRunTime.EQ.1) then
c if yes then store prof_time and longitude and latitude:
        ProfNo_tile=ProfNo_tile+1
        prof_time(num_file,ProfNo_tile,bi,bj)=diffsecs
        prof_lon(num_file,ProfNo_tile,bi,bj)=lon_cur
        prof_lat(num_file,ProfNo_tile,bi,bj)=lat_cur
        prof_ind_glob(num_file,ProfNo_tile,bi,bj)=k+1000*(kk-1)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
        prof_ind_avgbin(num_file,ProfNo_tile,bi,bj)=tmp_avgbin(k)
#endif
c then store interpolation coeffs and indices
        lon_fac=(lon_cur-lon_1)/(lon_2-lon_1)
        lat_fac=(lat_cur-lat_1)/(lat_2-lat_1)
        prof_interp_weights(num_file,ProfNo_tile,1,bi,bj)=
     &     (1-lon_fac)*(1-lat_fac)
        prof_interp_i(num_file,ProfNo_tile,1,bi,bj)=prof_i
        prof_interp_j(num_file,ProfNo_tile,1,bi,bj)=prof_j
        prof_interp_weights(num_file,ProfNo_tile,2,bi,bj)=
     &     lon_fac*(1-lat_fac)
        prof_interp_i(num_file,ProfNo_tile,2,bi,bj)=prof_i+1
        prof_interp_j(num_file,ProfNo_tile,2,bi,bj)=prof_j
        prof_interp_weights(num_file,ProfNo_tile,3,bi,bj)=
     &     (1-lon_fac)*lat_fac
        prof_interp_i(num_file,ProfNo_tile,3,bi,bj)=prof_i
        prof_interp_j(num_file,ProfNo_tile,3,bi,bj)=prof_j+1
        prof_interp_weights(num_file,ProfNo_tile,4,bi,bj)=
     &     lon_fac*lat_fac
        prof_interp_i(num_file,ProfNo_tile,4,bi,bj)=prof_i+1
        prof_interp_j(num_file,ProfNo_tile,4,bi,bj)=prof_j+1

      endif

c ==============================================================================

c 5.2a) determine whether profiles is in current tile domain (generic grid case)

       elseif (profIsInRunTime.EQ.1) then

       if (stopGenericGrid.EQ.0) then

       if ( ( abs( tmp_xC11(k) - xC(1,1,bi,bj) ).LT.0.0001 _d 0 ) .AND.
     & ( abs( tmp_yC11(k) - yC(1,1,bi,bj) ).LT.0.0001 _d 0) .AND.
     & ( abs( tmp_xCNINJ(k) - xC(sNx,sNy,bi,bj) ).LT.0.0001 _d 0 ) .AND.
     & ( abs( tmp_yCNINJ(k) - yC(sNx,sNy,bi,bj) ).LT.0.0001 _d 0 )
     & .AND.(profIsInRunTime.EQ.1)) then

c if yes then store prof_time and interpolation coeffs and indices:
       ProfNo_tile=ProfNo_tile+1
       prof_time(num_file,ProfNo_tile,bi,bj)=diffsecs
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
         prof_ind_avgbin(num_file,ProfNo_tile,bi,bj)=tmp_avgbin(k)
#endif
       prof_interp_xC11(num_file,ProfNo_tile,bi,bj)=tmp_xC11(k)
       prof_interp_yC11(num_file,ProfNo_tile,bi,bj)=tmp_yC11(k)
       prof_interp_xCNINJ(num_file,ProfNo_tile,bi,bj)=tmp_xCNINJ(k)
       prof_interp_yCNINJ(num_file,ProfNo_tile,bi,bj)=tmp_yCNINJ(k)
       tmp_sum_weights=0. _d 0
        do q = 1,iINTERP
             prof_interp_weights(num_file,ProfNo_tile,q,bi,bj)
     &       =tmp_weights(k,q)
             prof_interp_i(num_file,ProfNo_tile,q,bi,bj)
     &       =tmp_i(k,q)
             prof_interp_j(num_file,ProfNo_tile,q,bi,bj)
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
          stopGenericGrid=1
       endif
#ifdef ALLOW_PROFILES_EXCLUDE_CORNERS
       if ( tmp_weights(k,q) .NE. 0. _d 0) then
       if ( ((tmp_i(k,q).EQ.0).AND.(tmp_j(k,q).EQ.0))
     & .OR.((tmp_i(k,q).EQ.sNx+1).AND.(tmp_j(k,q).EQ.sNy+1))
     & .OR.((tmp_i(k,q).EQ.0).AND.(tmp_j(k,q).EQ.sNy+1))
     & .OR.((tmp_i(k,q).EQ.sNx+1).AND.(tmp_j(k,q).EQ.0)) ) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'points (profilesDoGenGrid; using overlap corners)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1
       endif
       endif
#endif /* ALLOW_PROFILES_EXCLUDE_CORNERS */
       if ( (tmp_weights(k,q).LT.0. _d 0).OR.
     &    (tmp_weights(k,q).GT.1. _d 0) ) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'weights (profilesDoGenGrid; sum oustide 0-1)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1
       endif

       enddo

       if ( abs(tmp_sum_weights -1. _d 0 ) .GT. 0.0001 _d 0) then
          WRITE(msgBuf,'(4A)')
     &     'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &     '.nc includes inconsistent interpolation ',
     &     'weights (profilesDoGenGrid; dont add up to 1)'
          CALL PRINT_ERROR( msgBuf, myThid)
          stopGenericGrid=1
       endif

         prof_ind_glob(num_file,ProfNo_tile,bi,bj)=k+1000*(kk-1)

       endif
       endif
       endif   !if (.NOT.profilesDoGenGrid) then

c ==============================================================================

c check that maximum size was not reached:
       if (ProfNo_tile.GE.NOBSGLOB) then
         WRITE(msgBuf,'(3A)')
     &    'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &    '.nc was not read properly (increase NOBSGLOB).'
         CALL PRINT_ERROR( msgBuf, myThid)
         stopProfiles=1
       endif

      endif    !if ( stopProfiles .EQ. 0) then
      enddo    !do k=1,min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))
      endif    !if (min(ProfNo(num_file,bi,bj), 1000...
      enddo    !do kk=1,profno_div1000+1

      ProfNo(num_file,bi,bj)=ProfNo_tile

      write(msgbuf,'(a,i9)')
     &   '  # of profiles with erroneous HHMMSS values =',
     &   ProfNo_hh
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)

      write(msgbuf,'(a,i9)')
     &   '  # of profiles within tile and time period  =',
     &   ProfNo(num_file,bi,bj)
      call print_message(
     &   msgbuf, standardmessageunit, SQUEEZE_RIGHT , mythid)

c6) available variablesin the data set

      do k=1,NVARMAX
        prof_num_var_cur(num_file,k,bi,bj)=0
      enddo
      prof_num_var_tot(num_file,bi,bj)=0

      do k=1,NVARMAX
        JL  = ILNBLNK( prof_names(num_file,k) )
        err = NF_INQ_VARID(fid,prof_names(num_file,k)(1:JL), varid1 )
        if (err.EQ.NF_NOERR) then
          vec_quantities(num_file,k,bi,bj)=.TRUE.
          prof_num_var_tot(num_file,bi,bj)=
     &     prof_num_var_tot(num_file,bi,bj)+1
          prof_num_var_cur(num_file,k,bi,bj)=
     &     prof_num_var_tot(num_file,bi,bj)
        else
          vec_quantities(num_file,k,bi,bj)=.FALSE.
        endif
      enddo

      do k=1,NVARMAX
        if (vec_quantities(num_file,k,bi,bj)) then
          KL  = ILNBLNK( prof_names(num_file,k) )
          JL  = ILNBLNK( prof_namesmod(num_file,k) )
          if (prof_namesmod(num_file,k).EQ.'pTracer') then
      write(msgbuf,'(a,I3,5a,I3)') '  variable #',k,' is ' ,
     & prof_names(num_file,k)(1:KL),' and ',
     & prof_namesmod(num_file,k)(1:JL),' #',
     & prof_itracer(num_file,k)
          else
      write(msgbuf,'(a,I3,4a)') '  variable #',k,
     & ' is            ' ,
     & prof_names(num_file,k)(1:KL),' and ',
     & prof_namesmod(num_file,k)(1:JL)
          endif
          call print_message(msgbuf,
     &       standardmessageunit, SQUEEZE_RIGHT , mythid)
        endif
      enddo

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
#ifdef PROFILES_USE_MDSFINDUNITS
      call MDSFINDUNIT( fidforward(num_file,bi,bj) , mythid )
#else
      call PROFILES_FINDUNIT( fidforward(num_file,bi,bj) , mythid )
#endif
      if (.NOT.exst) then
        call profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   fnameequinc,fidforward(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj,myThid)
      else
         open( fidforward(num_file,bi,bj),file=fnameequinc,
     &   form ='unformatted',status='unknown', access='direct',
     &   recl=  (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
      endif
#ifdef ALLOW_ADJOINT_RUN
      inquire( file=adfnameequinc, exist=exst )
#ifdef PROFILES_USE_MDSFINDUNITS
      call MDSFINDUNIT( fidadjoint(num_file,bi,bj) , mythid )
#else
      call PROFILES_FINDUNIT( fidadjoint(num_file,bi,bj) , mythid )
#endif
      if (.NOT.exst) then
        call profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   adfnameequinc, fidadjoint(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj, myThid)
      else
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
      prof_time(num_file,k,bi,bj)=-999. _d 0
      prof_lon(num_file,k,bi,bj)=-999. _d 0
      prof_lat(num_file,k,bi,bj)=-999. _d 0
      prof_ind_glob(num_file,k,bi,bj)=0
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      prof_ind_avgbin(num_file,k,bi,bj)=-999
#endif
      do q = 1,NUM_INTERP_POINTS
         prof_interp_i(num_file,k,q,bi,bj) = 1
         prof_interp_j(num_file,k,q,bi,bj) = 1
         prof_interp_weights(num_file,k,q,bi,bj) = 0. _d 0
      enddo
      prof_interp_xC11(num_file,k,bi,bj)=-999. _d 0
      prof_interp_yC11(num_file,k,bi,bj)=-999. _d 0
      prof_interp_xCNINJ(num_file,k,bi,bj)=-999. _d 0
      prof_interp_yCNINJ(num_file,k,bi,bj)=-999. _d 0
      enddo

      endif !if (IL.NE.0) then
      enddo !      do num_file=1,NFILESPROFMAX

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
C Find the unique depth levels from all profile datasets   
C initialize prof_depth_comb
      if(bi.EQ.1.and.bj.EQ.1)then
         NLEVELCOMB = 0
         NLEVELCOMBRL = NLEVELCOMB
      endif
      do m=1,NLEVELCOMBMAX
         prof_depth_comb(m,bi,bj)=-999. _d 0
      enddo

      m = 1
      do num_file=1,NFILESPROFMAX
       do K=1,ProfDepthNo(num_file,bi,bj)

          if(m.EQ.1) then
           prof_depth_comb(m,bi,bj) = prof_depth(num_file, k,bi,bj)
           m = m + 1
          else
C sort
           do l=1,NLEVELCOMBMAX-1
            if(prof_depth_comb(l,bi,bj) .ne. -999. _d 0) then

              if(prof_depth(num_file, k,bi,bj).lt.
     &           prof_depth_comb(l,bi,bj).and.
     &           l.EQ.1)  then
                 prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &            prof_depth_comb(l,bi,bj)
                 prof_depth_comb(l,bi,bj)=
     &            prof_depth(num_file, k,bi,bj)
                 do il = NLEVELCOMBMAX-1, l+2,-1
                    prof_depth_comb(il,bi,bj)=
     &            prof_depth_comb(il-1,bi,bj)
                 enddo
                 prof_depth_comb(l+1,bi,bj)=
     &            prof_depth_comb(NLEVELCOMBMAX,bi,bj)
              else if(prof_depth(num_file, k,bi,bj).gt.
     &           prof_depth_comb(l,bi,bj).and.
     &           prof_depth(num_file, k,bi,bj).lt.
     &           prof_depth_comb(l+1,bi,bj))  then

                 prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &            prof_depth_comb(l+1,bi,bj)
                 prof_depth_comb(l+1,bi,bj)=
     &            prof_depth(num_file, k,bi,bj)
                 do il = NLEVELCOMBMAX-1, l+3,-1
                    prof_depth_comb(il,bi,bj)=
     &            prof_depth_comb(il-1,bi,bj)
                 enddo
                 prof_depth_comb(l+2,bi,bj)=
     &            prof_depth_comb(NLEVELCOMBMAX,bi,bj)
              else if ( prof_depth(num_file, k,bi,bj).gt.
     &           prof_depth_comb(l,bi,bj).and.
     &           prof_depth_comb(l+1,bi,bj).eq.-999. _d 0)  then
                 prof_depth_comb(l+1,bi,bj) =
     &              prof_depth(num_file, k,bi,bj)
              endif
             endif
           enddo

          endif
          if(m.GE.NLEVELCOMBMAX-2)then
            WRITE(msgBuf,'(A)')
     &      'increase NLEVELCOMBMAX'
            CALL PRINT_ERROR( msgBuf, myThid)
           endif
       enddo ! do K=1,ProfDepthNo(num_file,bi,bj)
      enddo ! do num_file=1,NFILESPROFMAX
      prof_depth_comb(NLEVELCOMBMAX,bi,bj) = -999. _d 0

C diagnostics output
      do m=1,NLEVELCOMBMAX
         if(prof_depth_comb(m,bi,bj) .GE. 0. _d 0
     &     .AND. NLEVELCOMB.LT.m)then
           NLEVELCOMB = m
           if(m.GE.NLEVELCOMBMAX-2)then
            WRITE(msgBuf,'(A,2i6)')
     &      'increase NLEVELCOMBMAX: m,NLEVELCOMBMA  ',
     &      m, NLEVELCOMBMAX
            CALL PRINT_ERROR( msgBuf, myThid)
           endif
         endif
      enddo
      WRITE(msgBuf,'(A, i6,d20.5)')
     &      'NLEVELCOMB = ', NLEVELCOMB
      call print_message( msgbuf, standardmessageunit,
     &                    SQUEEZE_RIGHT , mythid)
#endif

C===========================================================
C error cases:
C===========================================================

c1) you want to provide interpolation information

       if ( stopGenericGrid.EQ.2) then
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
     & 'when using profilesDoGenGrid ',
     & 'you have to provide interpolation coeffs etc. '
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(2A)')
     & 'and some of your nc files dont have them. ',
     & 'You could use profiles_prep_mygrid.m and/or'
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(A)')
     & 'use the grid info in profiles*incl1PointOverlap*data'
       CALL PRINT_ERROR( msgBuf, myThid)
       stopProfiles=1

      endif

      ENDDO
      ENDDO

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      NLEVELCOMBRL = NLEVELCOMB
      _GLOBAL_MAX_RL( NLEVELCOMBRL, myThid )
      NLEVELCOMB = NLEVELCOMBRL
#endif

      _END_MASTER( mythid )
      _BARRIER

c2) stop after other kind of errors
      CALL GLOBAL_SUM_INT( stopProfiles , myThid )
      if ( stopProfiles.GE.1) then
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      endif

      CALL GLOBAL_SUM_INT( stopGenericGrid , myThid )
      if ( stopGenericGrid.GE.1) then
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      endif

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
