#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C     o==========================================================o
C     | subroutine cost_profiles                                 |
C     | o computes the cost for netcdf profiles data             |
C     | started: Gael Forget 15-March-2006                       |
C     o==========================================================o

      SUBROUTINE cost_profiles( myIter, myTime, myThid )

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
# include "OPTIMCYCLE.h"
#endif

c     == routine arguments ==
      integer myIter
      _RL     myTime
      integer myThid

#ifdef ALLOW_PROFILES

C     ========= Local variables =======================
      integer k,num_file,num_var,prof_num
      integer bi,bj,iG,jG,fid
      _RL prof_traj1D(NLEVELMAX), prof_traj1D_mean(NLEVELMAX)
      _RL prof_data1D(NLEVELMAX), prof_weights1D(NLEVELMAX)
#ifndef ALLOW_CTRL
      integer optimcycle
#endif
      CHARACTER*(MAX_LEN_MBUF) msgbuf
      CHARACTER*(MAX_LEN_FNAM) profilesfile, fnameequinc
      integer IL, JL, err

      _RL  objf_prof_tile (nSx,nSy)
      _RL  objf_prof_glo
      _RL  num_prof_tile (nSx,nSy)
      _RL  num_prof_glo

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      integer iavgbin,ikzz
      integer itmp
      integer k2, ix9, iy9, ktmp
      integer cunit
      CHARACTER*(MAX_LEN_FNAM) cfile

      _RL prof_data1D_mean(NLEVELMAX)
      _RL prof_count1D(NLEVELMAX)
      _RL prof_weights1D_mean(NLEVELMAX)
      _RL recip_profiles_mean_indsamples(NVARMAX)

      _RL tmpr6, tmpr7, tmpr8, tmpr9
      Real*4 tmp99(NAVGBINMAX)
      _RL tmp11, tmp12, tmp_recip_count
      LOGICAL doglbsum

      _RL  objf_prof_mean_tile (nSx,nSy)
      _RL  objf_prof_mean_glo
      _RL  num_prof_mean_tile (nSx,nSy)
      _RL  num_prof_mean_glo
#endif

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

c     == end of interface ==

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

      write(msgbuf,'(a)') ' '
      call print_message( msgbuf,
     &  standardMessageUnit,SQUEEZE_RIGHT , myThid)
      write(msgbuf,'(a)') '== cost_profiles: begin =='
      call print_message( msgbuf,
     &  standardMessageUnit,SQUEEZE_RIGHT , myThid)

        _BEGIN_MASTER( myThid )

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      NAVGBIN = 0
C initialize
      DO iavgbin = 1, NAVGBINMAX
       avgbinglbsum(iavgbin) = 0
       DO ikzz = 1, NLEVELCOMBMAX
        DO num_var=1,NVARMAX
           prof_traj1D_all_mean(iavgbin,ikzz,num_var)
     &      = 0. _d 0
           prof_data1D_all_mean(iavgbin,ikzz,num_var)
     &      = 0. _d 0
           prof_weights1D_all_mean(iavgbin,ikzz,num_var)
     &      = 0. _d 0
           prof_count1D_all_mean(iavgbin,ikzz,num_var)
     &      = 0. _d 0
        ENDDO
       ENDDO
      ENDDO

      DO num_var=1,NVARMAX
         recip_profiles_mean_indsamples(num_var) = 0. _d 0
         IF(profiles_mean_indsamples(num_var).GT. 0. _d 0) THEN
          recip_profiles_mean_indsamples(num_var) = 1. _d 0 /
     &     profiles_mean_indsamples(num_var)
         ENDIF
      ENDDO

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
           write(profilesfile,'(a)')
     &     profilesfiles(num_file)(1:IL)
           IL  = ILNBLNK( profilesfile )
           JL  = ILNBLNK( profilesDir )
           write(fnameequinc,'(3a,2(i3.3,a))')
     &     profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
c
           err = NF_OPEN(fnameequinc,NF_NOWRITE,
     &     fidforward(num_file,bi,bj))
         endif

C find the vertical indices
         do k=1,NLEVELMAX
          prof_lev_comb(k,num_file,bi,bj) = -999
          if(k.LE.ProfDepthNo(num_file,bi,bj)) then
           do k2 = 1, NLEVELCOMB
             if(prof_depth(num_file, k,bi,bj).EQ.
     &          prof_depth_comb(k2,bi,bj).AND.
     &          prof_depth_comb(k2,bi,bj).GE.0. _d 0.AND.
     &          prof_lev_comb(k,num_file,bi,bj).EQ.-999) then
              prof_lev_comb(k,num_file,bi,bj) = k2
             endif
           enddo
          endif
         enddo

         do num_var=1,NVARMAX
          if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
           do prof_num=1,NOBSGLOB
            if (prof_num.LE.ProfNo(num_file,bi,bj)) then

              do k=1,NLEVELMAX
                prof_traj1D(k)=0.
C             prof_traj1D_mean(k)=0.
C             prof_mask1D_cur(k,bi,bj)=0.
               prof_data1D(k)=0.
C             prof_data1D_mean(k)=0.
               prof_weights1D(k)=0.
              enddo
              ix9 = prof_interp_i(num_file,prof_num,1,bi,bj)
              iy9 = prof_interp_j(num_file,prof_num,1,bi,bj)

              if(prof_ind_avgbin(num_file,prof_num,bi,bj).GT.NAVGBIN)
     &         NAVGBIN = prof_ind_avgbin(num_file,prof_num,bi,bj)

              if(ix9 .GE. 0. _d 0 .AND. iy9 .GE. 0. _d 0) then
               itmp = prof_ind_avgbin(num_file,prof_num,bi,bj)
               if(avgbinglbsum(itmp).EQ.0)
     &          avgbinglbsum(itmp) = 1

               call active_read_profile(num_file,
     &          ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &          prof_num,.false.,optimcycle,bi,bj,myThid,
     &          profiles_dummy(num_file,num_var,bi,bj))

               call profiles_readvector(num_file,num_var,
     &          prof_ind_glob(num_file,prof_num,bi,bj),
     &          ProfDepthNo(num_file,bi,bj),prof_data1D,bi,bj,myThid)

               call profiles_readvector(num_file,-num_var,
     &          prof_ind_glob(num_file,prof_num,bi,bj),
     &          ProfDepthNo(num_file,bi,bj),
     &          prof_weights1D,bi,bj,myThid)

               do k=1,ProfDepthNo(num_file,bi,bj)
                if (prof_weights1D(k).GT.0. _d 0
     &           .AND. prof_mask1D_cur(k,bi,bj).NE. 0. _d 0
     &             ) then
                 prof_traj1D_all_mean(itmp,
     &            prof_lev_comb(k,num_file,bi,bj),num_var)
     &            = prof_traj1D_all_mean(itmp,
     &               prof_lev_comb(k,num_file,bi,bj), num_var)
     &            + prof_traj1D(k)

                 prof_data1D_all_mean(itmp,
     &            prof_lev_comb(k,num_file,bi,bj), num_var)
     &            = prof_data1D_all_mean(itmp,
     &               prof_lev_comb(k,num_file,bi,bj), num_var)
     &            + prof_data1D(k)

                 prof_weights1D_all_mean(itmp,
     &            prof_lev_comb(k,num_file,bi,bj), num_var)
     &            = prof_weights1D_all_mean(itmp,
     &               prof_lev_comb(k,num_file,bi,bj), num_var)
     &            + 1. _d 0 /prof_weights1D(k)

                 prof_count1D_all_mean(itmp,
     &            prof_lev_comb(k,num_file,bi,bj), num_var)
     &            = prof_count1D_all_mean(itmp,
     &               prof_lev_comb(k,num_file,bi,bj), num_var)
     &            + 1. _d 0
                endif
               enddo !do k=1,ProfDepthNo
              endif !      if(ix9 .GE. 0. _d 0 .AND. iy9 .GE. 0. _d 0) then

            endif !if (prof_num.LE.ProfNo(num_file,bi,bj)) then
           enddo !do prof_num=..
          endif !if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
         enddo !do num_var...

        enddo !do num_file=1,NFILESPROFMAX

       ENDDO !DO bj
       ENDDO !DO bj

       NAVGBINRL = NAVGBIN
       _GLOBAL_MAX_RL( NAVGBINRL, myThid )
       NAVGBIN = NAVGBINRL
       DO iavgbin = 1, NAVGBIN
          tmpr6 = avgbinglbsum(iavgbin)
          _GLOBAL_SUM_RL (tmpr6, myThid)
          if(tmpr6.GT.1.1) avgbinglbsum(iavgbin) = tmpr6
       ENDDO

C accumulate globally
       DO num_var=1,NVARMAX
        doglbsum = .FALSE.
        DO bj=1,nSy
         DO bi=1,nSx
          do num_file=1,NFILESPROFMAX
            if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then
              doglbsum = .TRUE.
            endif
          enddo
         ENDDO
        ENDDO

        if(doglbsum) then
         DO iavgbin = 1, NAVGBIN
           DO ikzz = 1, NLEVELCOMB
            tmpr6 = prof_count1D_all_mean(iavgbin,ikzz,num_var)
            _GLOBAL_SUM_RL (tmpr6, myThid)
            prof_count1D_all_mean(iavgbin,ikzz,num_var) = tmpr6

            tmpr9 = prof_weights1D_all_mean(iavgbin,ikzz,num_var)
            _GLOBAL_SUM_RL (tmpr9, myThid)
            prof_weights1D_all_mean(iavgbin,ikzz,num_var) = tmpr9

            tmpr7 = prof_traj1D_all_mean(iavgbin,ikzz,num_var)
            _GLOBAL_SUM_RL (tmpr7, myThid)
            prof_traj1D_all_mean(iavgbin,ikzz,num_var) = tmpr7

            tmpr8 = prof_data1D_all_mean(iavgbin,ikzz,num_var)
            _GLOBAL_SUM_RL (tmpr8, myThid)
            prof_data1D_all_mean(iavgbin,ikzz,num_var) = tmpr8
           ENDDO
         ENDDO
        endif
       ENDDO

C Now do the averaging
       DO iavgbin = 1, NAVGBIN
        DO ikzz = 1, NLEVELCOMB
         DO num_var=1,NVARMAX
            tmp_recip_count = 0. _d 0
            IF(prof_count1D_all_mean(iavgbin,ikzz,num_var).GT.0)THEN
             tmp_recip_count = 1. _d 0 /
     &          prof_count1D_all_mean(iavgbin,ikzz,num_var)
             prof_traj1D_all_mean(iavgbin,ikzz,num_var)
     &        = prof_traj1D_all_mean(iavgbin,ikzz,num_var)*
     &          tmp_recip_count
             prof_data1D_all_mean(iavgbin,ikzz,num_var)
     &        = prof_data1D_all_mean(iavgbin,ikzz,num_var)*
     &          tmp_recip_count
             prof_weights1D_all_mean(iavgbin,ikzz,num_var)
     &        = prof_weights1D_all_mean(iavgbin,ikzz,num_var)*
     &          tmp_recip_count
            ENDIF
         ENDDO
        ENDDO
       ENDDO

       DO iavgbin = 1, NAVGBIN
        DO ikzz = 1, NLEVELCOMB
         DO num_var=1,NVARMAX
            IF(prof_count1D_all_mean(iavgbin,ikzz,num_var).GT.0)THEN
C Assuming each averaging bin has a maximum of 9 independent measurements.
             tmp11 = prof_weights1D_all_mean(iavgbin,ikzz,num_var)
     &             / prof_count1D_all_mean(iavgbin,ikzz,num_var)
             tmp12 = prof_weights1D_all_mean(iavgbin,ikzz,num_var)
     &             * recip_profiles_mean_indsamples(num_var)
             prof_weights1D_all_mean(iavgbin,ikzz,num_var)
     &        = max(tmp11, tmp12)

C note prof_weights1D_all_mean is still sigam^2. Need to convert to weight
            if(prof_weights1D_all_mean(iavgbin,ikzz,num_var).NE.0. _d 0)
     &        prof_weights1D_all_mean(iavgbin,ikzz,num_var) =
     &         1. _d 0 /prof_weights1D_all_mean(iavgbin,ikzz,num_var)
            ENDIF
         ENDDO
        ENDDO
       ENDDO

       IF ( myProcId .eq. 0 ) THEN

        DO num_var=1,NVARMAX
         iL = ILNBLNK( prof_names(1,num_var) )
         write(cfile,'(2a)') prof_names(1,num_var)(1:iL),
     &   '_data_mean.data'
         call mdsfindunit( cunit, myThid )
         open( cunit, file   = cfile,
     &        status = 'unknown',
     &        access  = 'direct',
     &        recl = NAVGBINMAX*4)

         DO ikzz = 1, NLEVELCOMB
          tmp99(1:NAVGBINMAX)=
     &      prof_data1D_all_mean(1:NAVGBINMAX,ikzz,num_var)
          write(cunit,rec=ikzz) tmp99
         ENDDO
         close ( cunit )

         write(cfile,'(2a)')prof_names(1,num_var)(1:iL),
     &    '_model_mean.data'
         call mdsfindunit( cunit, myThid )
         open( cunit, file   = cfile,
     &         status = 'unknown',
C    &         form   = 'unformatted',
     &         access  = 'direct',
     &         recl = NAVGBINMAX*4)
C    &         access  = 'sequential'   )

         DO ikzz = 1, NLEVELCOMB
          tmp99(1:NAVGBINMAX)=
     &      prof_traj1D_all_mean(1:NAVGBINMAX,ikzz,num_var)
          write(cunit,rec=ikzz) tmp99
         ENDDO
         close ( cunit )

         write(cfile,'(2a)')
     &     prof_names(1,num_var)(1:iL),'_weight_mean.data'
         call mdsfindunit( cunit, myThid )
         open( cunit, file   = cfile,
     &         status = 'unknown',
     &         access  = 'direct',
     &         recl = NAVGBINMAX*4)

         DO ikzz = 1, NLEVELCOMB
          tmp99(1:NAVGBINMAX)=
     &      prof_weights1D_all_mean(1:NAVGBINMAX,ikzz,num_var)
          write(cunit,rec=ikzz) tmp99
         ENDDO
         close ( cunit )

         write(cfile,'(2a)')prof_names(1,num_var)(1:iL),
     &    '_count_mean.data'
         call mdsfindunit( cunit, myThid )
         open( cunit, file   = cfile,
     &         status = 'unknown',
     &         access  = 'direct',
     &         recl = NAVGBINMAX*4)

         DO ikzz = 1, NLEVELCOMB
          tmp99(1:NAVGBINMAX)=
     &     prof_count1D_all_mean(1:NAVGBINMAX,ikzz,num_var)
          write(cunit,rec=ikzz) tmp99
         ENDDO
         close ( cunit )
        ENDDO ! DO num_var=1,NVARMAX
       ENDIF ! IF ( myProcId .eq. 0 ) THEN
#endif

       DO bj=1,nSy
        DO bi=1,nSx

         do num_file=1,NFILESPROFMAX
          fid=fiddata(num_file,bi,bj)

          if ( (ProfNo(num_file,bi,bj).GT.0).AND.
     &         (profilesDoNcOutput) ) then
c need to close the file so that the data is not lost when run finishes
           err = NF_CLOSE(fidforward(num_file,bi,bj))
c then re-open it to compute cost function
           iG=bi+(myXGlobalLo-1)/sNx ! Kludge until unstructered tiles
           jG=bj+(myYGlobalLo-1)/sNy ! Kludge until unstructered tiles
           IL  = ILNBLNK( profilesfiles(num_file) )
           write(profilesfile,'(a)')
     &     profilesfiles(num_file)(1:IL)
           IL  = ILNBLNK( profilesfile )
           JL  = ILNBLNK( profilesDir )
           write(fnameequinc,'(3a,2(i3.3,a))')
     &     profilesDir(1:JL),profilesfile(1:IL),'.',iG,'.',jG,'.equi.nc'
c
           err = NF_OPEN(fnameequinc,NF_NOWRITE,
     &     fidforward(num_file,bi,bj))
          endif

          do prof_num=1,NOBSGLOB
           if (prof_num.LE.ProfNo(num_file,bi,bj)) then

c would be needed to call profiles_interp to e.g. get time averages
c           do k=1,NUM_INTERP_POINTS
c           prof_i1D(k)= prof_interp_i(num_file,prof_num,k,bi,bj)
c           prof_j1D(k)= prof_interp_j(num_file,prof_num,k,bi,bj)
c           prof_w1D(k)= prof_interp_weights(num_file,prof_num,k,bi,bj)
c          enddo

           do num_var=1,NVARMAX

            do k=1,NLEVELMAX
             prof_traj1D(k)=0.
             prof_traj1D_mean(k)=0.
             prof_mask1D_cur(k,bi,bj)=0.
             prof_data1D(k)=0.
             prof_weights1D(k)=0.
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
             prof_data1D_mean(k)=0.
             prof_weights1D_mean(k)=0.
#endif
            enddo

            if (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) then

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
             itmp = prof_ind_avgbin(num_file,prof_num,bi,bj)
             if(itmp.GE. 0) then
              do k=1,ProfDepthNo(num_file,bi,bj)

               ktmp = prof_lev_comb(k,num_file,bi,bj)
               prof_traj1D_mean(k) =
     &           prof_traj1D_all_mean(itmp,ktmp,num_var)
               prof_data1D_mean(k) =
     &           prof_data1D_all_mean(itmp,ktmp,num_var)
               prof_weights1D_mean(k) =
     &           prof_weights1D_all_mean(itmp,ktmp,num_var)
              enddo
             endif !if(itmp.GE. 0. _d 0) then
C end of #ifndef ALLOW_PROFILES_SAMPLESPLIT_COST
#endif

             call active_read_profile(num_file,
     &           ProfDepthNo(num_file,bi,bj),prof_traj1D,num_var,
     &           prof_num,.false.,optimcycle,bi,bj,myThid,
     &           profiles_dummy(num_file,num_var,bi,bj))

             call profiles_readvector(num_file,num_var,
     &           prof_ind_glob(num_file,prof_num,bi,bj),
     &           ProfDepthNo(num_file,bi,bj),prof_data1D,bi,bj,myThid)

             call profiles_readvector(num_file,-num_var,
     &           prof_ind_glob(num_file,prof_num,bi,bj),
     &           ProfDepthNo(num_file,bi,bj),
     &           prof_weights1D,bi,bj,myThid)

             do k=1,ProfDepthNo(num_file,bi,bj)
               if (prof_weights1D(k).GT.0.
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &             .AND. prof_data1D_mean(k).NE. 0. _d 0
#endif
     &            ) then
                 objf_profiles(num_file,num_var,bi,bj)=
     &             objf_profiles(num_file,num_var,bi,bj)
     &             +prof_weights1D(k)*prof_mask1D_cur(k,bi,bj)
     &             *(prof_traj1D(k)-prof_data1D(k)-prof_traj1D_mean(k)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &               + prof_data1D_mean(k)
#endif
     &              )
     &             *(prof_traj1D(k)-prof_data1D(k)-prof_traj1D_mean(k)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &               + prof_data1D_mean(k)
#endif
     &              )
                 num_profiles(num_file,num_var,bi,bj)=
     &               num_profiles(num_file,num_var,bi,bj)
     &               +prof_mask1D_cur(k,bi,bj)
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
     &      msgbuf, standardMessageUnit, SQUEEZE_RIGHT , myThid)
           write(msgbuf,'(a,2D22.15)')
     &      prof_names(num_file,num_var),
     &      objf_profiles(num_file,num_var,bi,bj),
     &      num_profiles(num_file,num_var,bi,bj)
          enddo !do num_var...
         endif
      ENDIF
#endif /* ALLOW_DEBUG */
        enddo !do num_file=1,NFILESPROFMAX

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      do num_var=1,NVARMAX
       DO iavgbin = 1, NAVGBINMAX
          do k=1,NLEVELCOMB
           prof_traj1D_mean(1) =
     &      prof_traj1D_all_mean(iavgbin,k,num_var)
           prof_data1D_mean(1) =
     &      prof_data1D_all_mean(iavgbin,k,num_var)
           prof_weights1D_mean(1) =
     &      prof_weights1D_all_mean(iavgbin,k,num_var)

           if (prof_weights1D_mean(1).GT.0.
     &         .AND. prof_data1D_mean(1).NE. 0. _d 0
     &         .AND. prof_traj1D_mean(1).NE. 0. _d 0
C    &         .AND. myProcId .eq. 0
     &         .AND. avgbinglbsum(iavgbin).GT.0
     &        ) then
             if(avgbinglbsum(iavgbin).EQ.1) then
              objf_profiles_mean(num_var,bi,bj)=
     &          objf_profiles_mean(num_var,bi,bj)
     &          +prof_weights1D_mean(1)
     &          *(prof_traj1D_mean(1)
     &            - prof_data1D_mean(1)
     &           )
     &          *(prof_traj1D_mean(1)
     &            - prof_data1D_mean(1)
     &           )
              num_profiles_mean(num_var,bi,bj)=
     &            num_profiles_mean(num_var,bi,bj)
     &            +1. _d 0
             else
              objf_profiles_mean(num_var,bi,bj)=
     &          objf_profiles_mean(num_var,bi,bj)
     &          +prof_weights1D_mean(1)
     &          *(prof_traj1D_mean(1)
     &            - prof_data1D_mean(1)
     &           )
     &          *(prof_traj1D_mean(1)
     &            - prof_data1D_mean(1)
     &           )/numberOfProcs
              num_profiles_mean(num_var,bi,bj)=
     &            num_profiles_mean(num_var,bi,bj)
     &            +1. _d 0/numberOfProcs
             endif ! if(avgbinglbsum(iavgbin).EQ.1) then

            endif ! if (prof_weights1D_mean(1).GT.0.
          enddo !do k=1,NLEVELCOMB
       enddo !DO iavgbin = 1
      enddo !do num_var

#ifdef ALLOW_DEBUG
      IF ( debugLevel .GE. debLevD ) THEN
C        if (ProfNo(num_file,bi,bj).GT.0) then
          do num_var=1,NVARMAX

           write(msgbuf,'(a,4I9)') 'bi,bj,num_var ',bi,bj,
     &      num_var
           call print_message(
     &      msgbuf, standardMessageUnit, SQUEEZE_RIGHT , myThid)

           write(msgbuf,'(a,a5,2D22.15)') prof_names(1,num_var),
     &      '_mean',
     &      objf_profiles_mean(num_var,bi,bj),
     &      num_profiles_mean(num_var,bi,bj)
           call print_message(
     &      msgbuf, standardMessageUnit, SQUEEZE_RIGHT , myThid)

          enddo !do num_var...
C        endif
      ENDIF
#endif /* ALLOW_DEBUG */

C        enddo !do num_file

#endif

       ENDDO
      ENDDO

      _END_MASTER( myThid )

c print cost function values
      do num_file=1,NFILESPROFMAX
      do num_var=1,NVARMAX
c
      do bj = myByLo(myThid),myByHi(myThid)
        do bi = myBxLo(myThid),myBxHi(myThid)
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
      write(msgbuf,'(2(a,I2),a,2D12.5)')
     &  ' cost_profiles(',num_file,',',num_var,')= ',
     &  objf_prof_glo,num_prof_glo

      IF ( num_prof_glo .GT. 0. ) call print_message( msgbuf,
     &  standardMessageUnit,SQUEEZE_RIGHT , myThid)
c
      enddo
      enddo

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      do num_var=1,NVARMAX
c
      do bj = myByLo(myThid),myByHi(myThid)
        do bi = myBxLo(myThid),myBxHi(myThid)
          objf_prof_mean_tile(bi,bj) =
     &             objf_profiles_mean(num_var,bi,bj)
          num_prof_mean_tile(bi,bj) =
     &             num_profiles_mean(num_var,bi,bj)
       enddo
      enddo
c
      CALL GLOBAL_SUM_TILE_RL( objf_prof_mean_tile,
     &     objf_prof_mean_glo, myThid )
      CALL GLOBAL_SUM_TILE_RL( num_prof_mean_tile,
     &     num_prof_mean_glo, myThid )
c
      write(msgbuf,'(a,I2,a,2D12.5)')
     &  ' cost_profiles_mean(',num_var,')= ',
     &  objf_prof_mean_glo,num_prof_mean_glo

      IF ( num_prof_mean_glo .GT. 0. ) call print_message( msgbuf,
     &  standardMessageUnit,SQUEEZE_RIGHT , myThid)
c
      enddo

#endif
C! ifdef ALLOW_PROFILES_SAMPLESPLIT_COST

      write(msgbuf,'(a)') '== cost_profiles: end   =='
      call print_message( msgbuf,
     &  standardMessageUnit,SQUEEZE_RIGHT , myThid)
      write(msgbuf,'(a)') ' '
      call print_message( msgbuf,
     &  standardMessageUnit,SQUEEZE_RIGHT , myThid)

      IF ( prof_make_nc ) THEN
       call profiles_make_ncfile(myThid)
      ENDIF

C===========================================================

#endif

      RETURN
      END
