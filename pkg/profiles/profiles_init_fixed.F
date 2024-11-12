#include "PROFILES_OPTIONS.h"
#include "AD_CONFIG.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE profiles_init_fixed( myThid )
C     *==========================================================*
C     | SUBROUTINE profiles_init_fixed                           |
C     | o initialization for netcdf profiles data                |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
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

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

c     == external functions ==
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER MDS_RECLEN
      EXTERNAL MDS_RECLEN

#ifdef ALLOW_PROFILES
C     === Local variables ===
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,k,l,m,bi,bj,iG,jG,num_file,num_var,ProfNo_tile
      INTEGER stopProfiles
      INTEGER fid, dimid, varid1, varid1a, varid1b
      INTEGER varid2,varid3
      _RL tmpyymmdd(1000),tmphhmmss(1000),diffsecs
      _RL yymmddMin,yymmddMax
      _RL hhmmssMin,hhmmssMax

      INTEGER tmpdate(4),tmpdiff(4),profIsInRunTime
      _RL  tmp_lon, tmp_lon2(1000), tmp_lat2(1000), lon_cur, lat_cur
      _RL lon_1, lon_2, lat_1, lat_2
      _RL lon_tmp1, lon_tmp2
      _RL lat_fac, lon_fac
      INTEGER prof_i, prof_j
      INTEGER vec_start(2), vec_count(2), profno_div1000, kk
      CHARACTER*(MAX_LEN_FNAM) profilesfile, fnamedatanc
      CHARACTER*(MAX_LEN_FNAM) fnameequinc
      CHARACTER*(MAX_LEN_FNAM) adfnameequinc, tlfnameequinc
      INTEGER IL, JL, KL, err
      LOGICAL  exst

      INTEGER varid_intp1, varid_intp2, varid_intp11 , varid_intp22
      INTEGER varid_intp3, varid_intp4, varid_intp5, q, iINTERP
      _RL tmp_i(1000,NUM_INTERP_POINTS)
      _RL tmp_j(1000,NUM_INTERP_POINTS)
      _RL tmp_weights(1000,NUM_INTERP_POINTS),tmp_sum_weights
      _RL tmp_xC11(1000),tmp_yC11(1000)
      _RL tmp_xCNINJ(1000),tmp_yCNINJ(1000)
      INTEGER stopGenericGrid
      Real*8 xy_buffer_r8(0:sNx+1,0:sNy+1)
      INTEGER vec_start2(2), vec_count2(2)
      INTEGER hh, ProfNo_hh
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      INTEGER varid4
      _RL tmp_avgbin(1000)
#endif

      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &'// insitu profiles model sampling >>> START <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

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

      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') 'general packages parameters :'
      JL  = ILNBLNK( profilesDir )
      IF (JL.NE.0) THEN
        WRITE(msgBuf,'(2a)') '  profilesDir ',profilesDir(1:JL)
      ELSE
        WRITE(msgBuf,'(2a)') '  profilesDir ','./'
      ENDIF
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a,l5)') '  profilesDoGenGrid  ',profilesDoGenGrid
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a,l5)') '  profilesDoNcOutput ',profilesDoNcOutput
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      _BEGIN_MASTER( myThid )

      DO bj=1,nSy
      DO bi=1,nSx

      profiles_curfile_buff(bi,bj)=0
      yymmddMin=modelstartdate(1)
      yymmddMax=modelenddate(1)
      hhmmssMin=modelstartdate(2)
      hhmmssMax=modelenddate(2)

      DO m=1,NLEVELMAX
       DO l=1,1000
        DO num_var=1,NVARMAX
         profiles_data_buff(m,l,num_var,bi,bj)=0. _d 0
         profiles_weight_buff(m,l,num_var,bi,bj)=0. _d 0
        ENDDO
       ENDDO
      ENDDO

      DO num_file=1,NFILESPROFMAX

      ProfNo_hh=0

      profilesfile=' '
      IL  = ILNBLNK( profilesfiles(num_file) )
      IF (IL.NE.0) THEN
       WRITE(profilesfile,'(a)')
     &      profilesfiles(num_file)(1:IL)
       WRITE(msgBuf,'(a)') ' '
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(a,i3,2a)')
     &      'profiles file #',num_file,' is ', profilesfile(1:IL)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
      ENDIF

      IL  = ILNBLNK( profilesfile )
      IF (IL.NE.0) THEN

C===========================================================
c open data files and read information
C===========================================================

      WRITE(fnamedatanc,'(2a)') profilesfile(1:IL),'.nc'
      err = NF_OPEN(fnamedatanc, 0, fiddata(num_file,bi,bj))
      CALL PROFILES_NF_ERROR( 'INIT_FIXED 1: NF_OPEN:',
     &     err,bi,bj,myThid )

c1)  read the number of profiles :
      fid=fiddata(num_file,bi,bj)
      err = NF_INQ_DIMID(fid,'iPROF', dimid )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED 2: NF_INQ_DIMID:',
     &     err,bi,bj,myThid )
      err = NF_INQ_DIMLEN(fid, dimid, ProfNo(num_file,bi,bj) )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED 3: NF_INQ_DIMLEN:',
     &     err,bi,bj,myThid )
      err = NF_INQ_DIMID(fid,'iDEPTH', dimid )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED 4: NF_INQ_DIMID:',
     &     err,bi,bj,myThid )
      IF (err.NE.NF_NOERR) THEN
       err = NF_INQ_DIMID(fid,'Z', dimid )
       CALL PROFILES_NF_ERROR( 'INIT_FIXED 5: NF_INQ_DIMID:',
     &      err,bi,bj,myThid )
      ENDIF
      err = NF_INQ_DIMLEN(fid, dimid, ProfDepthNo(num_file,bi,bj) )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED 6: NF_INQ_DIMLEN:',
     &     err,bi,bj,myThid )
      err = NF_INQ_DIMID(fid,'iINTERP', dimid )
      IF (err.EQ.NF_NOERR) THEN
        err = NF_INQ_DIMLEN(fid, dimid, iINTERP )
        CALL PROFILES_NF_ERROR( 'INIT_FIXED 8: NF_INQ_DIMLEN:',
     &      err,bi,bj,myThid )
      ELSE
       IF (debugLevel .GE. debLevA) THEN
        WRITE(msgBuf,'(2A,I3)')
     &      'S/R PROFILES_INIT_FIXED: no iINTERP dim in data file',
     &      ' using iINTERP = NUM_INTERP_POINTS =', NUM_INTERP_POINTS
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid )
       ENDIF
       iINTERP=NUM_INTERP_POINTS
      ENDIF

      WRITE(msgBuf,'(2(a,i4))')
     &   '  current tile is bi,bj                      =',
     &   bi,',',bj
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a,i9)')
     &   '  # of depth levels in file                  =',
     &   ProfDepthNo(num_file,bi,bj)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a,i9)')
     &   '  # of profiles in file                      =',
     &   ProfNo(num_file,bi,bj)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c2) read the dates and positions :
      err = NF_INQ_VARID(fid,'prof_depth', varid1a )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED 9: NF_INQ_VARID:',
     &      err,bi,bj,myThid )
      IF (err.NE.NF_NOERR) THEN
c       if no prof_depth is found, then try old variable name:
        err = NF_INQ_VARID(fid,'depth', varid1a )
        CALL PROFILES_NF_ERROR( 'INIT_FIXED10: NF_INQ_VARID:',
     &      err,bi,bj,myThid )
      ENDIF
      IF (err.NE.NF_NOERR) THEN
c       if neither is found, then stop
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc is not in the pkg/profiles format (no prof_depth etc.)'
        CALL PRINT_ERROR( msgBuf, myThid)
        stopProfiles=1
      ENDIF

      DO k=1,ProfDepthNo(num_file,bi,bj)
       err = NF_GET_VAR1_DOUBLE(fid,varid1a,k,
     &      prof_depth(num_file,k,bi,bj))
       CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_GET_VAR1_DOUBLE11:',
     &      err,bi,bj,myThid )
      ENDDO

      err = NF_INQ_VARID(fid,'prof_YYYYMMDD', varid1a )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_INQ_VARID12:',
     &     err,bi,bj,myThid )
      err = NF_INQ_VARID(fid,'prof_HHMMSS', varid1b )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_INQ_VARID13:',
     &     err,bi,bj,myThid )
      err = NF_INQ_VARID(fid,'prof_lon', varid2 )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_INQ_VARID14:',
     &     err,bi,bj,myThid )
      err = NF_INQ_VARID(fid,'prof_lat', varid3 )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_INQ_VARID15:',
     &     err,bi,bj,myThid )
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      err = NF_INQ_VARID(fid,'prof_bin_id_a', varid4 )
      CALL PROFILES_NF_ERROR( 'INIT_FIXED: NF_INQ_VARID15a:',
     &     err,bi,bj,myThid )
#endif

      IF (err.NE.NF_NOERR) THEN
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     & 'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     & '.nc is not in the pkg/profiles format (no prof_YYYYMMDD etc.)'
        CALL PRINT_ERROR( msgBuf, myThid)
      stopProfiles=1
      ENDIF

      IF (profilesDoGenGrid) THEN
c3) read interpolattion information (grid points, coeffs, etc.)
       err = NF_INQ_VARID(fid,'prof_interp_XC11',varid_intp1)
       err = NF_INQ_VARID(fid,'prof_interp_YC11',varid_intp2)
       err = NF_INQ_VARID(fid,'prof_interp_XCNINJ',varid_intp11)
       err = NF_INQ_VARID(fid,'prof_interp_YCNINJ',varid_intp22)
       err = NF_INQ_VARID(fid,'prof_interp_weights',varid_intp3)
       err = NF_INQ_VARID(fid,'prof_interp_i',varid_intp4)
       err = NF_INQ_VARID(fid,'prof_interp_j',varid_intp5)
       IF (err.NE.NF_NOERR) THEN
        IL  = ILNBLNK( profilesfile )
        WRITE(msgBuf,'(3A)')
     &   'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &   '.nc is missing interpolation information (profilesDoGenGrid)'
        CALL PRINT_ERROR( msgBuf, myThid)
        stopGenericGrid=2
       ENDIF
      ENDIF

c4) default values
      DO k=1,NOBSGLOB
       prof_time(num_file,k,bi,bj)=-999. _d 0
       prof_lon(num_file,k,bi,bj)=-999. _d 0
       prof_lat(num_file,k,bi,bj)=-999. _d 0
       prof_ind_glob(num_file,k,bi,bj)=0
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
       prof_ind_avgbin(num_file,k,bi,bj)=-999
#endif
       DO q = 1,NUM_INTERP_POINTS
        prof_interp_i(num_file,k,q,bi,bj) = 1
        prof_interp_j(num_file,k,q,bi,bj) = 1
        prof_interp_weights(num_file,k,q,bi,bj) = 0. _d 0
       ENDDO
       prof_interp_xC11(num_file,k,bi,bj)=-999. _d 0
       prof_interp_yC11(num_file,k,bi,bj)=-999. _d 0
       prof_interp_xCNINJ(num_file,k,bi,bj)=-999. _d 0
       prof_interp_yCNINJ(num_file,k,bi,bj)=-999. _d 0
      ENDDO

c5) main loop: look for profiles in this tile
      ProfNo_tile=0
      profno_div1000=max(0,int(ProfNo(num_file,bi,bj)/1000))

      DO kk=1,profno_div1000+1

       IF (min(ProfNo(num_file,bi,bj), 1000*kk).GE.
     &      1+1000*(kk-1)) THEN

c5.1) read a chunk
        vec_start(1)=1
        vec_start(2)=1+1000*(kk-1)
        vec_count(1)=1
        vec_count(2)=min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))

        IF ( (vec_count(2).LE.0).OR.(vec_count(2).GT.1000).OR.
     &       (vec_start(2).LE.0).OR.
     &       (vec_count(2)+vec_start(2)-1.GT.ProfNo(num_file,bi,bj)) )
     &       THEN
         IL  = ILNBLNK( profilesfile )
         WRITE(msgBuf,'(3A)')
     &        'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &        '.nc was not read properly (case 1).'
         CALL PRINT_ERROR( msgBuf, myThid)
         stopProfiles=1
        ENDIF

        err = NF_GET_VARA_DOUBLE(fid,varid1a,vec_start(2),
     &       vec_count(2), tmpyymmdd)
        CALL PROFILES_NF_ERROR( 'INIT_FIXED16: NF_GET_VARA_DOUBLE:',
     &       err,bi,bj,myThid )
        err = NF_GET_VARA_DOUBLE(fid,varid1b,vec_start(2),
     &       vec_count(2), tmphhmmss)
        CALL PROFILES_NF_ERROR( 'INIT_FIXED17: NF_GET_VARA_DOUBLE:',
     &       err,bi,bj,myThid )
        err = NF_GET_VARA_DOUBLE(fid,varid2,vec_start(2),
     &       vec_count(2), tmp_lon2)
        CALL PROFILES_NF_ERROR( 'INIT_FIXED18: NF_GET_VARA_DOUBLE:',
     &       err,bi,bj,myThid )
        err = NF_GET_VARA_DOUBLE(fid,varid3,vec_start(2),
     &       vec_count(2), tmp_lat2)
        CALL PROFILES_NF_ERROR( 'INIT_FIXED19: NF_GET_VARA_DOUBLE:',
     &       err,bi,bj,myThid )
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
        err = NF_GET_VARA_DOUBLE(fid,varid4,vec_start(2),
     &       vec_count(2), tmp_avgbin)
        CALL PROFILES_NF_ERROR( 'INIT_FIXED19a: NF_GET_VARA_DOUBLE:',
     &       err,bi,bj,myThid )
#endif

        IF (err.NE.NF_NOERR) THEN
         WRITE(msgBuf,'(3A)')
     &        'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &        '.nc was not read properly (case 2).'
         CALL PRINT_ERROR( msgBuf, myThid)
         stopProfiles=1
        ENDIF

c if profilesDoGenGrid then also read in the interpolation coeffs and indices
        IF (profilesDoGenGrid) THEN
         err = NF_GET_VARA_DOUBLE(fid,varid_intp1,vec_start(2),
     &        vec_count(2), tmp_xC11)
         err = NF_GET_VARA_DOUBLE(fid,varid_intp2,vec_start(2),
     &        vec_count(2), tmp_yC11)
         err = NF_GET_VARA_DOUBLE(fid,varid_intp11,vec_start(2),
     &        vec_count(2), tmp_xCNINJ)
         err = NF_GET_VARA_DOUBLE(fid,varid_intp22,vec_start(2),
     &        vec_count(2), tmp_yCNINJ)
         DO q=1,iINTERP
          vec_start2(1)=q
          vec_start2(2)=1+1000*(kk-1)
          vec_count2(1)=1
          vec_count2(2)=min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))
          err = NF_GET_VARA_DOUBLE(fid,varid_intp3,vec_start2,
     &         vec_count2, tmp_weights(1,q))
          err = NF_GET_VARA_DOUBLE(fid,varid_intp4,vec_start2,
     &         vec_count2, tmp_i(1,q))
          err = NF_GET_VARA_DOUBLE(fid,varid_intp5,vec_start2,
     &         vec_count2, tmp_j(1,q))
         ENDDO
        ENDIF

c5.2) loop through this chunk
        DO k=1,min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))

         IF ( stopProfiles .EQ. 0) THEN

          profIsInRunTime=1

          IF (( ( tmpyymmdd(k).GT.yymmddMin ).OR.(( tmpyymmdd(k).EQ.
     &            yymmddMin ).AND.( tmphhmmss(k).GT.hhmmssMin ))).AND.
     &        ( ( tmpyymmdd(k).LT.yymmddMax ).OR.(( tmpyymmdd(k).EQ.
     &            yymmddMax ).AND.( tmphhmmss(k).LT.hhmmssMax ))) ) THEN
           hh = int(tmphhmmss(k))/10000
           IF ( hh.LT.hoursPerDay ) THEN
            profIsInRunTime=1
            CALL cal_FullDate( int(tmpyymmdd(k)),int(tmphhmmss(k)),
     &           tmpdate,myThid )
            CALL cal_TimePassed( modelstartdate,tmpdate,tmpdiff,myThid )
            CALL cal_ToSeconds (tmpdiff,diffsecs,myThid)
            diffsecs=diffsecs+nIter0*deltaTClock
           ELSE
c if tmphhmmss is out of range then disregard profile
            profIsInRunTime=0
            diffsecs=-deltaTClock
            ProfNo_hh=ProfNo_hh+1
           ENDIF
          ELSE
           profIsInRunTime=0
           diffsecs=-deltaTClock
          ENDIF

c ==============================================================================

c 5.2a) determine whether profiles is in current tile domain (lat-lon grid case)
          IF ((.NOT.profilesDoGenGrid).AND.(profIsInRunTime.EQ.1)) THEN

           IF (xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)) THEN
            tmp_lon=xC(sNx+1,1,bi,bj)+360. _d 0
           ELSE
            tmp_lon=xC(sNx+1,1,bi,bj)
           ENDIF

           IF ((xC(1,1,bi,bj).LE.tmp_lon2(k)).AND.
     &          (tmp_lon.GT.tmp_lon2(k)).AND.
     &          (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     &          (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k)) ) THEN
            lon_cur=tmp_lon2(k)
            lat_cur=tmp_lat2(k)
           ELSEIF ((xC(sNx+1,1,bi,bj).LT.xC(1,1,bi,bj)).AND.
     &           (xC(1,1,bi,bj).LE.tmp_lon2(k)+360. _d 0).AND.
     &           (tmp_lon.GT.tmp_lon2(k)+360. _d 0).AND.
     &           (yC(1,1,bi,bj).LE.tmp_lat2(k)).AND.
     &           (yC(1,sNy+1,bi,bj).GT.tmp_lat2(k))
     &           ) THEN
            lon_cur=tmp_lon2(k)+360. _d 0
            lat_cur=tmp_lat2(k)
           ELSE
            profIsInRunTime=0
           ENDIF

c now determine value of i,j to the south-ouest of data point
           prof_i=-10
           prof_j=-10
           lon_1=-10
           lon_2=-10
           lat_1=-10
           lat_2=-10

           IF (profIsInRunTime.EQ.1) THEN
            DO j=1,sNy+1
             DO i=1,sNx+1

c value of j, south of the data point:
              IF ((yC(i,j,bi,bj).LE.lat_cur).AND.
     &             (yC(i,j+1,bi,bj).GT.lat_cur)) THEN
               prof_j=j
               lat_1=yC(i,j,bi,bj)
               lat_2=yC(i,j+1,bi,bj)
              ENDIF

c value of i, west of the data point:
              IF (xC(i+1,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
               lon_tmp2=xC(i+1,j,bi,bj)+360
              ELSE
               lon_tmp2=xC(i+1,j,bi,bj)
              ENDIF
              IF (xC(i,j,bi,bj).LT.xC(1,j,bi,bj)) THEN
               lon_tmp1=xC(i,j,bi,bj)+360
              ELSE
               lon_tmp1=xC(i,j,bi,bj)
              ENDIF

              IF ((lon_tmp1.LE.lon_cur).AND.(lon_tmp2.GT.lon_cur)) THEN
               prof_i=i
               lon_1=lon_tmp1
               lon_2=lon_tmp2
              ENDIF

             ENDDO
            ENDDO
           ENDIF

           IF ((prof_i.EQ.-10).OR.(prof_j.EQ.-10)) profIsInRunTime=0

           IF (profIsInRunTime.EQ.1) THEN
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
     &           (1-lon_fac)*(1-lat_fac)
            prof_interp_i(num_file,ProfNo_tile,1,bi,bj)=prof_i
            prof_interp_j(num_file,ProfNo_tile,1,bi,bj)=prof_j
            prof_interp_weights(num_file,ProfNo_tile,2,bi,bj)=
     &           lon_fac*(1-lat_fac)
            prof_interp_i(num_file,ProfNo_tile,2,bi,bj)=prof_i+1
            prof_interp_j(num_file,ProfNo_tile,2,bi,bj)=prof_j
            prof_interp_weights(num_file,ProfNo_tile,3,bi,bj)=
     &           (1-lon_fac)*lat_fac
            prof_interp_i(num_file,ProfNo_tile,3,bi,bj)=prof_i
            prof_interp_j(num_file,ProfNo_tile,3,bi,bj)=prof_j+1
            prof_interp_weights(num_file,ProfNo_tile,4,bi,bj)=
     &           lon_fac*lat_fac
            prof_interp_i(num_file,ProfNo_tile,4,bi,bj)=prof_i+1
            prof_interp_j(num_file,ProfNo_tile,4,bi,bj)=prof_j+1

           ENDIF

c ==============================================================================

c 5.2a) determine whether profiles is in current tile domain (generic grid case)

          ELSEIF (profIsInRunTime.EQ.1) THEN

           IF (stopGenericGrid.EQ.0) THEN

            IF (
     & ( abs( tmp_xC11(k) - xC(1,1,bi,bj) ).LT.0.0001 _d 0 ) .AND.
     & ( abs( tmp_yC11(k) - yC(1,1,bi,bj) ).LT.0.0001 _d 0) .AND.
     & ( abs( tmp_xCNINJ(k) - xC(sNx,sNy,bi,bj) ).LT.0.0001 _d 0 ) .AND.
     & ( abs( tmp_yCNINJ(k) - yC(sNx,sNy,bi,bj) ).LT.0.0001 _d 0 )
     & .AND.(profIsInRunTime.EQ.1)) THEN

c if yes then store prof_time and interpolation coeffs and indices:
             ProfNo_tile=ProfNo_tile+1
             prof_time(num_file,ProfNo_tile,bi,bj)=diffsecs
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
             prof_ind_avgbin(num_file,ProfNo_tile,bi,bj)=tmp_avgbin(k)
#endif
             prof_interp_xC11(num_file,ProfNo_tile,bi,bj)=tmp_xC11(k)
             prof_interp_yC11(num_file,ProfNo_tile,bi,bj)=tmp_yC11(k)
             prof_interp_xCNINJ(num_file,ProfNo_tile,bi,bj)
     &            =tmp_xCNINJ(k)
             prof_interp_yCNINJ(num_file,ProfNo_tile,bi,bj)
     &            =tmp_yCNINJ(k)
             tmp_sum_weights=0. _d 0
             DO q = 1,iINTERP
              prof_interp_weights(num_file,ProfNo_tile,q,bi,bj)
     &             =tmp_weights(k,q)
              prof_interp_i(num_file,ProfNo_tile,q,bi,bj)
     &             =tmp_i(k,q)
              prof_interp_j(num_file,ProfNo_tile,q,bi,bj)
     &             =tmp_j(k,q)
              tmp_sum_weights=tmp_sum_weights+tmp_weights(k,q)
c more test of the inputs: is the offline-computed
c interpolation information consistent (self and with grid)
              IF ( (tmp_i(k,q).LT.0).OR.(tmp_j(k,q).LT.0)
     &         .OR.(tmp_i(k,q).GT.sNx+1).OR.(tmp_j(k,q).GT.sNy+1)
     &             ) THEN
               WRITE(msgBuf,'(4A)')
     &              'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &              '.nc includes inconsistent interpolation ',
     &              'points (profilesDoGenGrid; out of tile)'
               CALL PRINT_ERROR( msgBuf, myThid)
               stopGenericGrid=1
              ENDIF
#ifdef ALLOW_PROFILES_EXCLUDE_CORNERS
              IF ( tmp_weights(k,q) .NE. 0. _d 0) THEN
               IF (
     &     ((tmp_i(k,q).EQ.0).AND.(tmp_j(k,q).EQ.0))
     & .OR.((tmp_i(k,q).EQ.sNx+1).AND.(tmp_j(k,q).EQ.sNy+1))
     & .OR.((tmp_i(k,q).EQ.0).AND.(tmp_j(k,q).EQ.sNy+1))
     & .OR.((tmp_i(k,q).EQ.sNx+1).AND.(tmp_j(k,q).EQ.0)) ) THEN
                WRITE(msgBuf,'(4A)')
     &               'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &               '.nc includes inconsistent interpolation ',
     &               'points (profilesDoGenGrid; using overlap corners)'
                CALL PRINT_ERROR( msgBuf, myThid)
                stopGenericGrid=1
               ENDIF
              ENDIF
#endif /* ALLOW_PROFILES_EXCLUDE_CORNERS */
              IF ( (tmp_weights(k,q).LT.0. _d 0).OR.
     &             (tmp_weights(k,q).GT.1. _d 0) ) THEN
               WRITE(msgBuf,'(4A)')
     &              'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &              '.nc includes inconsistent interpolation ',
     &              'weights (profilesDoGenGrid; sum oustide 0-1)'
               CALL PRINT_ERROR( msgBuf, myThid)
               stopGenericGrid=1
              ENDIF

             ENDDO

             IF ( abs(tmp_sum_weights -1. _d 0 ) .GT. 0.0001 _d 0) THEN
              WRITE(msgBuf,'(4A)')
     &             'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &             '.nc includes inconsistent interpolation ',
     &             'weights (profilesDoGenGrid; dont add up to 1)'
              CALL PRINT_ERROR( msgBuf, myThid)
              stopGenericGrid=1
             ENDIF

             prof_ind_glob(num_file,ProfNo_tile,bi,bj)=k+1000*(kk-1)

            ENDIF
           ENDIF
          ENDIF                 !if (.NOT.profilesDoGenGrid) THEN

c ==============================================================================

c check that maximum size was not reached:
          IF (ProfNo_tile.GE.NOBSGLOB) THEN
           WRITE(msgBuf,'(3A)')
     &          'PROFILES_INIT_FIXED: file ', profilesfile(1:IL),
     &          '.nc was not read properly (increase NOBSGLOB).'
           CALL PRINT_ERROR( msgBuf, myThid)
           stopProfiles=1
          ENDIF

         ENDIF !if ( stopProfiles .EQ. 0) THEN
        ENDDO  !do k=1,min(1000,ProfNo(num_file,bi,bj)-1000*(kk-1))
       ENDIF   !if (min(ProfNo(num_file,bi,bj), 1000...
      ENDDO    !do kk=1,profno_div1000+1

      ProfNo(num_file,bi,bj)=ProfNo_tile

      WRITE(msgBuf,'(a,i9)')
     &   '  # of profiles with erroneous HHMMSS values =',
     &   ProfNo_hh
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

      WRITE(msgBuf,'(a,i9)')
     &   '  # of profiles within tile and time period  =',
     &   ProfNo(num_file,bi,bj)
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

c6) available variables in the data set

      DO num_var=1,NVARMAX
        prof_num_var_cur(num_file,num_var,bi,bj)=0
      ENDDO
      prof_num_var_tot(num_file,bi,bj)=0

      DO num_var=1,NVARMAX
       JL  = ILNBLNK( prof_names(num_file,num_var) )
       err = NF_INQ_VARID(fid,prof_names(num_file,num_var)(1:JL),varid1)
       IF (err.EQ.NF_NOERR) THEN
        vec_quantities(num_file,num_var,bi,bj)=.TRUE.
        prof_num_var_tot(num_file,bi,bj)=
     &       prof_num_var_tot(num_file,bi,bj)+1
        prof_num_var_cur(num_file,num_var,bi,bj)=
     &       prof_num_var_tot(num_file,bi,bj)
       ELSE
        IF (debugLevel .GE. debLevA) THEN
         WRITE(msgBuf,'(3A)') 'S/R PROFILES_INIT_FIXED: no',
     &        prof_names(num_file,num_var)(1:JL),
     &        ', setting corresponding vec_quantities = F'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ENDIF
        vec_quantities(num_file,num_var,bi,bj)=.FALSE.
       ENDIF
      ENDDO

      DO num_var=1,NVARMAX
       IF (vec_quantities(num_file,num_var,bi,bj)) THEN
        KL  = ILNBLNK( prof_names(num_file,num_var) )
        JL  = ILNBLNK( prof_namesmod(num_file,num_var) )
        IF (prof_namesmod(num_file,num_var).EQ.'pTracer') THEN
         WRITE(msgBuf,'(a,I3,5a,I3)') '  variable #',num_var,' is ' ,
     &        prof_names(num_file,num_var)(1:KL),' and ',
     &        prof_namesmod(num_file,num_var)(1:JL),' #',
     &        prof_itracer(num_file,num_var)
        ELSE
         WRITE(msgBuf,'(a,I3,4a)') '  variable #',num_var,
     &        ' is            ' ,
     &        prof_names(num_file,num_var)(1:KL),' and ',
     &        prof_namesmod(num_file,num_var)(1:JL)
        ENDIF
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
       ENDIF
      ENDDO

C===========================================================
c create files for model counterparts to observations
C===========================================================

      IF (ProfNo(num_file,bi,bj).GT.0) THEN
       iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
       jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles

       JL  = ILNBLNK( profilesDir )

       IF (profilesDoNcOutput) THEN

        WRITE(fnameequinc,'(3a,2(i3.3,a))')
     & profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
C#ifdef ALLOW_ADJOINT_RUN
        WRITE(adfnameequinc,'(4a,2(i3.3,a))')
     & profilesDir(1:JL),'ad',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
C#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
        WRITE(tlfnameequinc,'(4a,2(i3.3,a))')
     & profilesDir(1:JL),'tl',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
#endif

        JL  = ILNBLNK( fnameequinc )
        inquire( file=fnameequinc(1:JL), exist=exst )
        IF (.NOT.exst) THEN
         CALL profiles_init_ncfile(num_file,
     &   fiddata(num_file,bi,bj),fnameequinc(1:JL),
     &   fidforward(num_file,bi,bj),ProfNo(num_file,bi,bj),
     &   ProfDepthNo(num_file,bi,bj),
     &   bi,bj,myThid)
        ELSE
C obtain existing NetCDF file id
         err = NF_OPEN( fnameequinc(1:JL), NF_WRITE,
     &        fidforward(num_file,bi,bj))
         CALL PROFILES_NF_ERROR( 'INIT_FIXED21: NF_OPEN:',
     &        err,bi,bj,myThid )
        ENDIF
#ifdef ALLOW_ADJOINT_RUN
        JL  = ILNBLNK( adfnameequinc )
        inquire( file=adfnameequinc(1:JL), exist=exst )
        IF (.NOT.exst) THEN
         CALL profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   adfnameequinc(1:JL), fidadjoint(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj, myThid)
        ELSE
C obtain existing NetCDF file id
         err = NF_OPEN( adfnameequinc(1:JL), NF_WRITE,
     &        fidadjoint(num_file,bi,bj))
         CALL PROFILES_NF_ERROR( 'INIT_FIXED22: NF_OPEN:',
     &        err,bi,bj,myThid )
        ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
        JL  = ILNBLNK( tlfnameequinc )
        inquire( file=tlfnameequinc(1:JL), exist=exst )
        IF (.NOT.exst) THEN
         CALL profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &        tlfnameequinc(1:JL), fidtangent(num_file,bi,bj),
     &        ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &        bi,bj, myThid)
        ELSE
C obtain existing NetCDF file id
         err = NF_OPEN( tlfnameequinc(1:JL), NF_WRITE,
     &        fidtangent(num_file,bi,bj))
         CALL PROFILES_NF_ERROR( 'INIT_FIXED23: NF_OPEN:',
     &        err,bi,bj,myThid )
        ENDIF
#endif
       ELSE

        WRITE(fnameequinc,'(3a,2(i3.3,a))')
     & profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.data'
C#ifdef ALLOW_ADJOINT_RUN
        WRITE(adfnameequinc,'(4a,2(i3.3,a))')
     & profilesDir(1:JL),'ad',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.data'
C#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
        WRITE(tlfnameequinc,'(4a,2(i3.3,a))')
     & profilesDir(1:JL),'tl',
     & profilesfile(1:IL),'.',iG,'.',jG,'.equi.data'
#endif

        JL  = ILNBLNK( fnameequinc )
        inquire( file=fnameequinc(1:JL), exist=exst )
#ifdef PROFILES_USE_MDSFINDUNITS
        CALL MDSFINDUNIT( fidforward(num_file,bi,bj) , myThid )
#else
        CALL PROFILES_FINDUNIT( fidforward(num_file,bi,bj) , myThid )
#endif
        IF (.NOT.exst) THEN
         CALL profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &        fnameequinc(1:JL),fidforward(num_file,bi,bj),
     &        ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &        bi,bj,myThid)
        ELSE
         open( fidforward(num_file,bi,bj),file=fnameequinc(1:JL),
     &   form ='unformatted',status='unknown', access='direct',
     &   recl=  (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
        ENDIF
#ifdef ALLOW_ADJOINT_RUN
        JL  = ILNBLNK( adfnameequinc )
        inquire( file=adfnameequinc(1:JL), exist=exst )
# ifdef PROFILES_USE_MDSFINDUNITS
        CALL MDSFINDUNIT( fidadjoint(num_file,bi,bj) , myThid )
# else
        CALL PROFILES_FINDUNIT( fidadjoint(num_file,bi,bj) , myThid )
# endif
        IF (.NOT.exst) THEN
         CALL profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   adfnameequinc(1:JL), fidadjoint(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj, myThid)
        ELSE
         open( fidadjoint(num_file,bi,bj),file=adfnameequinc(1:JL),
     &   form ='unformatted',status='unknown', access='direct',
     &   recl=  (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
        ENDIF
#endif
#ifdef ALLOW_TANGENTLINEAR_RUN
        JL  = ILNBLNK( tlfnameequinc )
        inquire( file=tlfnameequinc(1:JL), exist=exst )
# ifdef PROFILES_USE_MDSFINDUNITS
        CALL MDSFINDUNIT( fidtangent(num_file,bi,bj) , myThid )
# else
        CALL PROFILES_FINDUNIT( fidtangent(num_file,bi,bj) , myThid )
# endif
        IF (.NOT.exst) THEN
         CALL profiles_init_ncfile(num_file,fiddata(num_file,bi,bj),
     &   tlfnameequinc(1:JL), fidtangent(num_file,bi,bj),
     &   ProfNo(num_file,bi,bj),ProfDepthNo(num_file,bi,bj),
     &   bi,bj, myThid)
        ELSE
         open( fidtangent(num_file,bi,bj),file=tlfnameequinc(1:JL),
     &   form ='unformatted',status='unknown', access='direct',
     &   recl=  (ProfDepthNo(num_file,bi,bj)+1)*WORDLENGTH*2 )
        ENDIF
#endif

       ENDIF

      ENDIF

C===========================================================
      ELSE
       ProfNo(num_file,bi,bj)=0
       DO num_var=1,NVARMAX
        prof_num_var_cur(num_file,num_var,bi,bj)=0
        vec_quantities(num_file,num_var,bi,bj)=.FALSE.
       ENDDO
       prof_num_var_tot(num_file,bi,bj)=0
       DO k=1,NOBSGLOB
        prof_time(num_file,k,bi,bj)=-999. _d 0
        prof_lon(num_file,k,bi,bj)=-999. _d 0
        prof_lat(num_file,k,bi,bj)=-999. _d 0
        prof_ind_glob(num_file,k,bi,bj)=0
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
        prof_ind_avgbin(num_file,k,bi,bj)=-999
#endif
        DO q = 1,NUM_INTERP_POINTS
         prof_interp_i(num_file,k,q,bi,bj) = 1
         prof_interp_j(num_file,k,q,bi,bj) = 1
         prof_interp_weights(num_file,k,q,bi,bj) = 0. _d 0
        ENDDO
        prof_interp_xC11(num_file,k,bi,bj)=-999. _d 0
        prof_interp_yC11(num_file,k,bi,bj)=-999. _d 0
        prof_interp_xCNINJ(num_file,k,bi,bj)=-999. _d 0
        prof_interp_yCNINJ(num_file,k,bi,bj)=-999. _d 0
       ENDDO

      ENDIF ! IF (IL.NE.0) THEN
      ENDDO ! DO num_file=1,NFILESPROFMAX

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
C Find the unique depth levels from all profile datasets
C initialize prof_depth_comb
      IF(bi.EQ.1.AND.bj.EQ.1)THEN
       NLEVELCOMB = 0
       NLEVELCOMBRL = NLEVELCOMB
      ENDIF
      DO m=1,NLEVELCOMBMAX
       prof_depth_comb(m,bi,bj)=-999. _d 0
      ENDDO

      m = 1
      DO num_file=1,NFILESPROFMAX
       DO k=1,ProfDepthNo(num_file,bi,bj)

        IF (m.EQ.1) THEN
         prof_depth_comb(m,bi,bj) = prof_depth(num_file, k,bi,bj)
         m = m + 1
        ELSE
C sort
         DO l=1,NLEVELCOMBMAX-1
          IF (prof_depth_comb(l,bi,bj) .NE. -999. _d 0) THEN

           IF (prof_depth(num_file, k,bi,bj).LT.
     &          prof_depth_comb(l,bi,bj).AND.
     &          l.EQ.1) THEN
            prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &           prof_depth_comb(l,bi,bj)
            prof_depth_comb(l,bi,bj)=
     &           prof_depth(num_file, k,bi,bj)
            DO il = NLEVELCOMBMAX-1, l+2,-1
             prof_depth_comb(il,bi,bj)=
     &            prof_depth_comb(il-1,bi,bj)
            ENDDO
            prof_depth_comb(l+1,bi,bj)=
     &           prof_depth_comb(NLEVELCOMBMAX,bi,bj)
           ELSE if(prof_depth(num_file, k,bi,bj).GT.
     &           prof_depth_comb(l,bi,bj).AND.
     &           prof_depth(num_file, k,bi,bj).LT.
     &           prof_depth_comb(l+1,bi,bj))  THEN

            prof_depth_comb(NLEVELCOMBMAX,bi,bj) =
     &           prof_depth_comb(l+1,bi,bj)
            prof_depth_comb(l+1,bi,bj)=
     &           prof_depth(num_file, k,bi,bj)
            DO il = NLEVELCOMBMAX-1, l+3,-1
             prof_depth_comb(il,bi,bj)=
     &            prof_depth_comb(il-1,bi,bj)
            ENDDO
            prof_depth_comb(l+2,bi,bj)=
     &            prof_depth_comb(NLEVELCOMBMAX,bi,bj)
           ELSE IF ( prof_depth(num_file, k,bi,bj).GT.
     &           prof_depth_comb(l,bi,bj).AND.
     &           prof_depth_comb(l+1,bi,bj).EQ.-999. _d 0)  THEN
            prof_depth_comb(l+1,bi,bj) =
     &           prof_depth(num_file, k,bi,bj)
           ENDIF
          ENDIF
         ENDDO

        ENDIF
        if(m.GE.NLEVELCOMBMAX-2)THEN
         WRITE(msgBuf,'(A)')
     &        'increase NLEVELCOMBMAX'
         CALL PRINT_ERROR( msgBuf, myThid)
        ENDIF
       ENDDO ! DO k=1,ProfDepthNo(num_file,bi,bj)
      ENDDO ! DO num_file=1,NFILESPROFMAX
      prof_depth_comb(NLEVELCOMBMAX,bi,bj) = -999. _d 0

C diagnostics output
      DO m=1,NLEVELCOMBMAX
       IF(prof_depth_comb(m,bi,bj) .GE. 0. _d 0
     &      .AND. NLEVELCOMB.LT.m)THEN
        NLEVELCOMB = m
        if(m.GE.NLEVELCOMBMAX-2)THEN
         WRITE(msgBuf,'(A,2i6)')
     &        'increase NLEVELCOMBMAX: m,NLEVELCOMBMA  ',
     &        m, NLEVELCOMBMAX
         CALL PRINT_ERROR( msgBuf, myThid)
        ENDIF
       ENDIF
      ENDDO
      WRITE(msgBuf,'(A, i6,d20.5)') 'NLEVELCOMB = ', NLEVELCOMB
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
#endif

C===========================================================
C error cases:
C===========================================================

c1) you want to provide interpolation information

      IF ( stopGenericGrid.EQ.2) THEN
       iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
       jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
cgf XC grid
       CALL MDSFINDUNIT( fid , myThid )
       WRITE(fnameequinc,'(a,2(i3.3,a),2(i4.4,a))')
     &  'profilesXCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
       k=MDS_RECLEN(64,(sNx+2)*(sNy+2),myThid)
       WRITE(standardMessageUnit,'(A,/,2A)')
     &      'PROFILES_INIT_FIXED: creating grid from profiles; file:',
     &      fnameequinc
       JL  = ILNBLNK( fnameequinc )
       OPEN( fid, file= fnameequinc(1:JL), form ='unformatted',
     &      status='unknown',access='direct', recl= k)
       DO m=0,sNy+1
        DO l=0,sNx+1
         xy_buffer_r8(l,m)=xC(l,m,bi,bj)
        ENDDO
       ENDDO
#ifdef _BYTESWAPIO
       CALL MDS_BYTESWAPR8((sNx+2)*(sNy+2),xy_buffer_r8)
#endif
       WRITE(fid,rec=1) xy_buffer_r8
       CLOSE(fid)
cgf YC grid
       CALL MDSFINDUNIT( fid , myThid )
       WRITE(fnameequinc,'(a,2(i3.3,a),2(i4.4,a))')
     & 'profilesYCincl1PointOverlap.',iG,'.',jG,'.',sNx,'.',sNy,'.data'
       k=MDS_RECLEN(64,(sNx+2)*(sNy+2),myThid)
       WRITE(standardMessageUnit,'(A,/,A)')
     &      'PROFILES_INIT_FIXED: creating grid from profiles; file:',
     &      fnameequinc
       JL  = ILNBLNK( fnameequinc )
       OPEN( fid, file= fnameequinc(1:JL), form ='unformatted',
     &      status='unknown', access='direct', recl= k)
       DO m=0,sNy+1
        DO l=0,sNx+1
         xy_buffer_r8(l,m)=yC(l,m,bi,bj)
        ENDDO
       ENDDO
#ifdef _BYTESWAPIO
       CALL MDS_BYTESWAPR8((sNx+2)*(sNy+2),xy_buffer_r8)
#endif
       WRITE(fid,rec=1) xy_buffer_r8
       CLOSE(fid)

       WRITE(msgBuf,'(3A)')
     &      'PROFILES_INIT_FIXED : ',
     &      'when using profilesDoGenGrid ',
     &      'you have to provide interpolation coeffs etc. '
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(2A)')
     &      'and some of your nc files dont have them. ',
     &      'You could use profiles_prep_mygrid.m and/or'
       CALL PRINT_ERROR( msgBuf, myThid)
       WRITE(msgBuf,'(A)')
     &      'use the grid info in profiles*incl1PointOverlap*data'
       CALL PRINT_ERROR( msgBuf, myThid)
       stopProfiles=1

C     end of IF ( stopGenericGrid.EQ.2) THEN
      ENDIF

C     bi/bj
      ENDDO
      ENDDO

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      NLEVELCOMBRL = NLEVELCOMB
      _GLOBAL_MAX_RL( NLEVELCOMBRL, myThid )
      NLEVELCOMB = NLEVELCOMBRL
#endif

      _END_MASTER( myThid )
      _BARRIER

c2) stop after other kind of errors
      CALL GLOBAL_SUM_INT( stopProfiles , myThid )
      IF ( stopProfiles.GE.1) THEN
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      CALL GLOBAL_SUM_INT( stopGenericGrid , myThid )
      IF ( stopGenericGrid.GE.1) THEN
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R PROFILES_INIT_FIXED'
      ENDIF

      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &'// insitu profiles model sampling >>> END <<<'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)')
     &'// ======================================================='
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* ALLOW_PROFILES */

      RETURN
      END
