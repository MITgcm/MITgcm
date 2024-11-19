#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: COST_PROFILES

C !INTERFACE:
      SUBROUTINE COST_PROFILES( myIter, myTime, myThid )

C     !DESCRIPTION:
C     Computes the cost for netcdf profiles data
C     Started: Gael Forget 15-March-2006

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
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

C     !INPUT PARAMETERS:
C     myIter: iteration counter for this thread
C     myTime: time counter for this thread
C     myThid: my thread ID number
      INTEGER myIter
      _RL     myTime
      INTEGER myThid
C     !OUTPUT PARAMETERS:
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      INTEGER kLev, kC, kCMax, kProf
      INTEGER num_file,num_var,prof_num
      INTEGER bi,bj
      _RL prof_traj1D(NLEVELMAX), prof_traj1D_mean(NLEVELMAX)
      _RL prof_data1D(NLEVELMAX), prof_weights1D(NLEVELMAX)
#ifndef ALLOW_CTRL
      INTEGER optimcycle
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      INTEGER IL
#endif
      INTEGER err

      _RL  objf_prof_tile (nSx,nSy)
      _RL  objf_prof_glo
      _RL  num_prof_tile (nSx,nSy)
      _RL  num_prof_glo

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      INTEGER iavgbin,ikzz
      INTEGER itmp
      INTEGER k2, ix9, iy9, ktmp
      INTEGER cunit
      CHARACTER*(MAX_LEN_FNAM) cfile

      _RL prof_data1D_mean(NLEVELMAX)
      _RL prof_count1D(NLEVELMAX)
      _RL prof_weights1D_mean(NLEVELMAX)
      _RL recip_profiles_mean_indsamples(NVARMAX)

      _RL tmpr6, tmpr7, tmpr8, tmpr9
      REAL*4 tmp99(NAVGBINMAX)
      _RL tmp11, tmp12, tmp_recip_count
      LOGICAL doglbsum

      _RL  objf_prof_mean_tile (nSx,nSy)
      _RL  objf_prof_mean_glo
      _RL  num_prof_mean_tile (nSx,nSy)
      _RL  num_prof_mean_glo
#endif

#ifndef ALLOW_CTRL
      optimcycle = 0
#endif

      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') '== cost_profiles: begin =='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit, SQUEEZE_RIGHT , myThid )

      _BEGIN_MASTER( myThid )

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      NAVGBIN = 0
C Initialize
      DO iavgbin = 1, NAVGBINMAX
        avgbinglbsum(iavgbin) = 0
        DO kCMax = 1, NLEVELCOMBMAX
          DO num_var = 1, NVARMAX
            prof_traj1D_all_mean(iavgbin,kCMax,num_var) = 0. _d 0
            prof_data1D_all_mean(iavgbin,kCMax,num_var) = 0. _d 0
            prof_weights1D_all_mean(iavgbin,kCMax,num_var) = 0. _d 0
            prof_count1D_all_mean(iavgbin,kCMax,num_var) = 0. _d 0
          ENDDO
        ENDDO
      ENDDO

      DO num_var = 1, NVARMAX
        recip_profiles_mean_indsamples(num_var) = 0. _d 0
        IF (profiles_mean_indsamples(num_var).GT.0. _d 0) THEN
          recip_profiles_mean_indsamples(num_var) = 1. _d 0 /
     &     profiles_mean_indsamples(num_var)
        ENDIF
      ENDDO

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO num_file = 1, NFILESPROFMAX
            IF ( (ProfNo(num_file,bi,bj).GT.0) .AND.
     &           (profilesDoNcOutput) ) THEN
C Write data from buffer to .nc file
              err = NF_SYNC( fidforward(num_file,bi,bj) )
              CALL PROFILES_NF_ERROR( 'COST: NF_SYNC fidforward',
     &             err,bi,bj,myThid )

            ENDIF

C Find vertical indices
            DO kLev = 1, NLEVELMAX
              prof_lev_comb(kLev,num_file,bi,bj) = -999

              IF (kLev.LE.ProfDepthNo(num_file,bi,bj)) THEN
                DO kC = 1, NLEVELCOMB
                  IF (prof_depth(num_file,kLev,bi,bj).EQ.
     &                prof_depth_comb(kC,bi,bj) .AND.
     &                prof_depth_comb(kC,bi,bj).GE.0. _d 0 .AND.
     &                prof_lev_comb(kLev,num_file,bi,bj).EQ.-999) THEN
                    prof_lev_comb(kLev,num_file,bi,bj) = kC
                  ENDIF
                ENDDO
              ENDIF

            ENDDO

            DO num_var = 1, NVARMAX
              IF (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) THEN
                DO prof_num = 1, NOBSGLOB
                  IF (prof_num.LE.ProfNo(num_file,bi,bj)) THEN
                    DO kLev = 1, NLEVELMAX
                      prof_traj1D(kLev) = 0.
                      prof_data1D(kLev) = 0.
                      prof_weights1D(kLev) = 0.
                    ENDDO

                    ix9 = prof_interp_i(num_file,prof_num,1,bi,bj)
                    iy9 = prof_interp_j(num_file,prof_num,1,bi,bj)

                    IF (prof_ind_avgbin(num_file,prof_num,bi,bj).GT.
     &                  NAVGBIN)
     &                NAVGBIN = prof_ind_avgbin(num_file,prof_num,bi,bj)

                    IF (ix9 .GE. 0. _d 0 .AND. iy9 .GE. 0. _d 0) THEN
                      itmp = prof_ind_avgbin(num_file,prof_num,bi,bj)
                      IF (avgbinglbsum(itmp).EQ.0)
     &                  avgbinglbsum(itmp) = 1

                      CALL ACTIVE_READ_PROFILE( num_file,
     &                     ProfDepthNo(num_file,bi,bj),prof_traj1D,
     &                     num_var,prof_num,.false.,optimcycle,
     &                     bi,bj,myThid,
     &                     profiles_dummy(num_file,num_var,bi,bj) )

                      CALL PROFILES_READVECTOR( num_file, num_var,
     &                     prof_ind_glob(num_file,prof_num,bi,bj),
     &                     ProfDepthNo(num_file,bi,bj),prof_data1D,
     &                     bi,bj,myThid )

                      CALL PROFILES_READVECTOR( num_file,-num_var,
     &                     prof_ind_glob(num_file,prof_num,bi,bj),
     &                     ProfDepthNo(num_file,bi,bj),prof_weights1D,
     &                     bi,bj,myThid )

                      DO kProf = 1, ProfDepthNo(num_file,bi,bj)
                        IF (prof_weights1D(kProf).GT.0. _d 0 .AND.
     &                      prof_mask1D_cur(kProf,bi,bj).NE. 0. _d 0 ) THEN
                          prof_traj1D_all_mean(itmp,
     &                     prof_lev_comb(kProf,num_file,bi,bj),num_var)
     &                     = prof_traj1D_all_mean(itmp,
     &                        prof_lev_comb(kProf,num_file,bi,bj),
     &                        num_var)
     &                       + prof_traj1D(kProf)

                          prof_data1D_all_mean(itmp,
     &                     prof_lev_comb(kProf,num_file,bi,bj), num_var)
     &                     = prof_data1D_all_mean(itmp,
     &                        prof_lev_comb(kProf,num_file,bi,bj),
     &                        num_var)
     &                       + prof_data1D(kProf)

                          prof_weights1D_all_mean(itmp,
     &                     prof_lev_comb(kProf,num_file,bi,bj), num_var)
     &                     = prof_weights1D_all_mean(itmp,
     &                        prof_lev_comb(kProf,num_file,bi,bj),
     &                        num_var)
     &                       + 1. _d 0 / prof_weights1D(kProf)

                          prof_count1D_all_mean(itmp,
     &                     prof_lev_comb(kProf,num_file,bi,bj), num_var)
     &                     = prof_count1D_all_mean(itmp,
     &                        prof_lev_comb(kProf,num_file,bi,bj),
     &                        num_var)
     &                       + 1. _d 0

                        ENDIF
                      ENDDO !DO kProf = 1, ProfDepthNo

                    ENDIF !IF (ix9 .GE. 0. _d 0 .AND. iy9 .GE. 0. _d 0)

                  ENDIF !IF (prof_num.LE.ProfNo(num_file,bi,bj))
                ENDDO !DO prof_num...
              ENDIF !IF (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.)
            ENDDO !DO num_var

          ENDDO !DO num_file = 1, NFILESPROFMAX
        ENDDO !DO bi
      ENDDO !DO bj

      NAVGBINRL = NAVGBIN
      _GLOBAL_MAX_RL( NAVGBINRL, myThid )
      NAVGBIN = NAVGBINRL
      DO iavgbin = 1, NAVGBIN
        tmpr6 = avgbinglbsum(iavgbin)
        _GLOBAL_SUM_RL(tmpr6, myThid)
        IF (tmpr6.GT.1.1) avgbinglbsum(iavgbin) = tmpr6
      ENDDO

C Accumulate globally
      DO num_var = 1, NVARMAX
        doglbsum = .FALSE.
        DO bj = 1, nSy
          DO bi = 1, nSx
            DO num_file = 1, NFILESPROFMAX
              IF (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.) THEN
                doglbsum = .TRUE.
              ENDIF
            ENDDO !DO num_file
          ENDDO !DO bi
        ENDDO !DO bj

        IF (doglbsum) THEN
          DO iavgbin = 1, NAVGBIN
             DO kC = 1, NLEVELCOMB
               tmpr6 = prof_count1D_all_mean(iavgbin,kC,num_var)
               _GLOBAL_SUM_RL( tmpr6, myThid )
               prof_count1D_all_mean(iavgbin,kC,num_var) = tmpr6

               tmpr9 = prof_weights1D_all_mean(iavgbin,kC,num_var)
               _GLOBAL_SUM_RL( tmpr9, myThid )
               prof_weights1D_all_mean(iavgbin,kC,num_var) = tmpr9

               tmpr7 = prof_traj1D_all_mean(iavgbin,kC,num_var)
               _GLOBAL_SUM_RL( tmpr7, myThid )
               prof_traj1D_all_mean(iavgbin,kC,num_var) = tmpr7

               tmpr8 = prof_data1D_all_mean(iavgbin,kC,num_var)
               _GLOBAL_SUM_RL( tmpr8, myThid )
               prof_data1D_all_mean(iavgbin,kC,num_var) = tmpr8
             ENDDO !DO kC
          ENDDO !DO iavgbin
        ENDIF
      ENDDO !DO num_var

C Now do the averaging
      DO iavgbin = 1, NAVGBIN
        DO kC = 1, NLEVELCOMB
          DO num_var = 1, NVARMAX
            tmp_recip_count = 0. _d 0
            IF (prof_count1D_all_mean(iavgbin,kC,num_var).GT.0) THEN
              tmp_recip_count = 1. _d 0 /
     &           prof_count1D_all_mean(iavgbin,kC,num_var)

              prof_traj1D_all_mean(iavgbin,kC,num_var)
     &         = prof_traj1D_all_mean(iavgbin,kC,num_var)*
     &           tmp_recip_count

              prof_data1D_all_mean(iavgbin,kC,num_var)
     &         = prof_data1D_all_mean(iavgbin,kC,num_var)*
     &           tmp_recip_count

              prof_weights1D_all_mean(iavgbin,kC,num_var)
     &         = prof_weights1D_all_mean(iavgbin,kC,num_var)*
     &           tmp_recip_count

            ENDIF
          ENDDO !DO num_var
        ENDDO !DO kC
      ENDDO !DO iavgbin

      DO iavgbin = 1, NAVGBIN
        DO kC = 1, NLEVELCOMB
          DO num_var = 1, NVARMAX
            IF (prof_count1D_all_mean(iavgbin,kC,num_var).GT.0) THEN
C Assuming each averaging bin has a maximum of 9 independent measurements.
              tmp11 = prof_weights1D_all_mean(iavgbin,kC,num_var)
     &         / prof_count1D_all_mean(iavgbin,kC,num_var)

              tmp12 = prof_weights1D_all_mean(iavgbin,kC,num_var)
     &         * recip_profiles_mean_indsamples(num_var)

              prof_weights1D_all_mean(iavgbin,kC,num_var) =
     &         MAX(tmp11, tmp12)

C prof_weights1D_all_mean is still sigam^2. Convert to weight
              IF (prof_weights1D_all_mean(iavgbin,kC,num_var)
     &            .NE.0. _d 0)
     &          prof_weights1D_all_mean(iavgbin,kC,num_var) =
     &           1. _d 0 / prof_weights1D_all_mean(iavgbin,kC,num_var)
            ENDIF
          ENDDO !DO num_var
        ENDDO !DO kC
      ENDDO !DO iavgbin

      IF (myProcId.EQ.0) THEN

        DO num_var = 1, NVARMAX
          iL = ILNBLNK( prof_names(1,num_var) )
          WRITE(cfile,'(2A)') prof_names(1,num_var)(1:iL),
     &     '_data_mean.data'
          CALL MDSFINDUNIT( cunit, myThid )
          OPEN( cunit, FILE = cfile, STATUS = 'unknown',
     &          ACCESS = 'direct', RECL = NAVGBINMAX*4 )

          DO kC = 1, NLEVELCOMB
            tmp99(1:NAVGBINMAX) =
     &        prof_data1D_all_mean(1:NAVGBINMAX,kC,num_var)
            WRITE(cunit,REC = kC) tmp99
          ENDDO
          CLOSE( cunit )

          WRITE(cfile,'(2A)') prof_names(1,num_var)(1:iL),
     &     '_model_mean.data'
          CALL MDSFINDUNIT( cunit, myThid )
          OPEN( cunit, FILE = cfile, STATUS = 'unknown',
     &          ACCESS = 'direct', RECL = NAVGBINMAX*4 )

          DO kC = 1, NLEVELCOMB
            tmp99(1:NAVGBINMAX) =
     &        prof_traj1D_all_mean(1:NAVGBINMAX,kC,num_var)
            WRITE(cunit,REC = kC) tmp99
          ENDDO
          CLOSE( cunit )

          WRITE(cfile,'(2A)') prof_names(1,num_var)(1:iL),
     &     '_weight_mean.data'
          CALL MDSFINDUNIT( cunit, myThid )
          OPEN( cunit, FILE = cfile, STATUS = 'unknown',
     &          ACCESS = 'direct', RECL = NAVGBINMAX*4 )

          DO kC = 1, NLEVELCOMB
            tmp99(1:NAVGBINMAX)=
     &        prof_weights1D_all_mean(1:NAVGBINMAX,kC,num_var)
            WRITE(cunit,REC = kC) tmp99
          ENDDO
          CLOSE( cunit )

          WRITE(cfile,'(2A)') prof_names(1,num_var)(1:iL),
     &     '_count_mean.data'
          CALL MDSFINDUNIT( cunit, myThid )
          OPEN( cunit, FILE = cfile, STATUS = 'unknown',
     &          ACCESS = 'direct', RECL = NAVGBINMAX*4 )

          DO kC = 1, NLEVELCOMB
            tmp99(1:NAVGBINMAX) =
     &       prof_count1D_all_mean(1:NAVGBINMAX,kC,num_var)
            WRITE(cunit,REC = kC) tmp99
          ENDDO
          CLOSE( cunit )

        ENDDO !DO num_var
      ENDIF !IF (myProcId.EQ.0)
      _BARRIER
#endif

      DO bj = 1, nSy
        DO bi = 1, nSx
          DO num_file = 1, NFILESPROFMAX
            IF ( (ProfNo(num_file,bi,bj).GT.0) .AND.
     &           (profilesDoNcOutput) ) THEN
C Sync file so that data is not lost when run finishes
              err = NF_SYNC( fidforward(num_file,bi,bj) )
              CALL PROFILES_NF_ERROR( 'COST: NF_SYNC fidforward',
     &             err,bi,bj,myThid )

            ENDIF

            DO prof_num = 1, NOBSGLOB
              IF (prof_num.LE.ProfNo(num_file,bi,bj)) THEN
                DO num_var = 1, NVARMAX
                  DO kLev = 1, NLEVELMAX
                    prof_traj1D(kLev) = 0.
                    prof_traj1D_mean(kLev) = 0.
                    prof_mask1D_cur(kLev,bi,bj) = 0.
                    prof_data1D(kLev) = 0.
                    prof_weights1D(kLev) = 0.
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                    prof_data1D_mean(kLev) = 0.
                    prof_weights1D_mean(kLev) = 0.
#endif

                  ENDDO

                  IF (vec_quantities(num_file,num_var,bi,bj).EQV..TRUE.)
     &              THEN
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
                    itmp = prof_ind_avgbin(num_file,prof_num,bi,bj)

                    IF (itmp.GE. 0) THEN
                      DO kProf = 1, ProfDepthNo(num_file,bi,bj)
                        ktmp = prof_lev_comb(kProf,num_file,bi,bj)

                        prof_traj1D_mean(kProf) =
     &                    prof_traj1D_all_mean(itmp,ktmp,num_var)

                        prof_data1D_mean(kProf) =
     &                    prof_data1D_all_mean(itmp,ktmp,num_var)

                        prof_weights1D_mean(kProf) =
     &                    prof_weights1D_all_mean(itmp,ktmp,num_var)

                      ENDDO
                    ENDIF !IF (itmp.GE.0)
#endif

                    CALL ACTIVE_READ_PROFILE( num_file,
     &                   ProfDepthNo(num_file,bi,bj),prof_traj1D,
     &                   num_var,prof_num,.false.,optimcycle,
     &                   bi,bj,myThid,
     &                   profiles_dummy(num_file,num_var,bi,bj) )

                    CALL PROFILES_READVECTOR( num_file, num_var,
     &                   prof_ind_glob(num_file,prof_num,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),prof_data1D,
     &                   bi,bj,myThid )

                    CALL PROFILES_READVECTOR( num_file,-num_var,
     &                   prof_ind_glob(num_file,prof_num,bi,bj),
     &                   ProfDepthNo(num_file,bi,bj),prof_weights1D,
     &                   bi,bj,myThid )

                    DO kProf = 1, ProfDepthNo(num_file,bi,bj)
                      IF (prof_weights1D(kProf).GT.0.
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &                    .AND. prof_data1D_mean(kProf).NE. 0. _d 0
#endif
     &                   ) THEN
                        objf_profiles(num_file,num_var,bi,bj) =
     &                    objf_profiles(num_file,num_var,bi,bj)
     &                    +prof_weights1D(kProf)
     &                    *prof_mask1D_cur(kProf,bi,bj)
     &                    *(prof_traj1D(kProf)-prof_data1D(kProf)
     &                    -prof_traj1D_mean(kProf)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &                    + prof_data1D_mean(kProf)
#endif
     &                    )
     &                    *(prof_traj1D(kProf)-prof_data1D(kProf)
     &                    -prof_traj1D_mean(kProf)
#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
     &                    + prof_data1D_mean(kProf)
#endif
     &                    )

                        num_profiles(num_file,num_var,bi,bj) =
     &                    num_profiles(num_file,num_var,bi,bj)
     &                    +prof_mask1D_cur(kProf,bi,bj)

                      ENDIF
                    ENDDO !DO kProf

                  ENDIF !IF (vec_quantitities

                ENDDO !DO num_var
              ENDIF !IF (prof_num.LE.ProfNo(num_file,bi,bj))
            ENDDO !DO prof_num

#ifdef ALLOW_DEBUG
            IF ( debugLevel .GE. debLevD ) THEN
              IF (ProfNo(num_file,bi,bj).GT.0) THEN
                DO num_var = 1, NVARMAX
                  WRITE(msgBuf,'(A,4I9)') 'bi,bj,prof_num,num_var ',
     &             bi,bj,ProfNo(num_file,bi,bj),num_var
                  CALL PRINT_MESSAGE( msgBuf,
     &                 standardMessageUnit, SQUEEZE_RIGHT , myThid )
                  WRITE(msgBuf,'(A,2D22.15)')
     &             prof_names(num_file,num_var),
     &             objf_profiles(num_file,num_var,bi,bj),
     &             num_profiles(num_file,num_var,bi,bj)
                  CALL PRINT_MESSAGE( msgBuf,
     &                 standardMessageUnit, SQUEEZE_RIGHT , myThid )
                ENDDO !DO num_var
              ENDIF
            ENDIF
#endif /* ALLOW_DEBUG */
          ENDDO !DO num_file

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
          DO num_var = 1, NVARMAX
            DO iavgbin = 1, NAVGBINMAX
              DO kC = 1, NLEVELCOMB
                prof_traj1D_mean(1) =
     &           prof_traj1D_all_mean(iavgbin,kC,num_var)
                prof_data1D_mean(1) =
     &           prof_data1D_all_mean(iavgbin,kC,num_var)
                prof_weights1D_mean(1) =
     &           prof_weights1D_all_mean(iavgbin,kC,num_var)

                IF (prof_weights1D_mean(1).GT.0. .AND.
     &              prof_data1D_mean(1).NE. 0. _d 0 .AND.
     &              prof_traj1D_mean(1).NE. 0. _d 0 .AND.
     &              avgbinglbsum(iavgbin).GT.0 ) THEN
                  IF (avgbinglbsum(iavgbin).EQ.1) THEN
                    objf_profiles_mean(num_var,bi,bj) =
     &                objf_profiles_mean(num_var,bi,bj)
     &                + prof_weights1D_mean(1)
     &                * (prof_traj1D_mean(1) - prof_data1D_mean(1))
     &                * (prof_traj1D_mean(1) - prof_data1D_mean(1))

                    num_profiles_mean(num_var,bi,bj) =
     &                num_profiles_mean(num_var,bi,bj) + 1. _d 0

                  ELSE
                    objf_profiles_mean(num_var,bi,bj) =
     &                objf_profiles_mean(num_var,bi,bj)
     &                + prof_weights1D_mean(1)
     &                * (prof_traj1D_mean(1) - prof_data1D_mean(1))
     &                * (prof_traj1D_mean(1) - prof_data1D_mean(1))
     &                / numberOfProcs

                    num_profiles_mean(num_var,bi,bj) =
     &               num_profiles_mean(num_var,bi,bj) + 1. _d 0
     &               /numberOfProcs

                  ENDIF !IF (avgbinglbsum(iavgbin).EQ.1)
                 ENDIF !IF (prof_weights1D_mean(1).GT.0.)

              ENDDO !DO kC
            ENDDO !DO iavgbin
          ENDDO !DO num_var

# ifdef ALLOW_DEBUG
          IF ( debugLevel .GE. debLevD ) THEN
            DO num_var = 1, NVARMAX
              WRITE(msgBuf,'(A,4I9)') 'bi,bj,num_var ',bi,bj,num_var
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT , myThid )

              WRITE(msgBuf,'(A,A5,2D22.15)') prof_names(1,num_var),
     &         '_mean',
     &         objf_profiles_mean(num_var,bi,bj),
     &         num_profiles_mean(num_var,bi,bj)
              CALL PRINT_MESSAGE( msgBuf,
     &             standardMessageUnit, SQUEEZE_RIGHT , myThid )

            ENDDO !DO num_var
          ENDIF
# endif /* ALLOW_DEBUG */

#endif /* ALLOW_PROFILES_SAMPLESPLIT_COST */

        ENDDO !DO bi
      ENDDO !DO bj

      _END_MASTER( myThid )

C Print cost function values
      DO num_file = 1, NFILESPROFMAX
        DO num_var = 1, NVARMAX

          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              objf_prof_tile(bi,bj) =
     &          objf_profiles(num_file,num_var,bi,bj)

              num_prof_tile(bi,bj) =
     &          num_profiles(num_file,num_var,bi,bj)

           ENDDO
          ENDDO

        CALL GLOBAL_SUM_TILE_RL( objf_prof_tile, objf_prof_glo, myThid )
        CALL GLOBAL_SUM_TILE_RL( num_prof_tile, num_prof_glo, myThid )

        WRITE(msgBuf,'(2(A,I2),A,2D22.15)')
     &   ' cost_profiles(',num_file,',',num_var,') = ',
     &   objf_prof_glo,num_prof_glo

        IF (num_prof_glo .GT. 0.)
     &    CALL PRINT_MESSAGE( msgBuf,
     &         standardMessageUnit,SQUEEZE_RIGHT , myThid )

        ENDDO !DO num_var
      ENDDO !DO num_file

#ifdef ALLOW_PROFILES_SAMPLESPLIT_COST
      DO num_var = 1, NVARMAX

        DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
            objf_prof_mean_tile(bi,bj) =
     &        objf_profiles_mean(num_var,bi,bj)

            num_prof_mean_tile(bi,bj) =
     &        num_profiles_mean(num_var,bi,bj)

         ENDDO
        ENDDO

        CALL GLOBAL_SUM_TILE_RL( objf_prof_mean_tile,
     &       objf_prof_mean_glo, myThid )
        CALL GLOBAL_SUM_TILE_RL( num_prof_mean_tile,
     &       num_prof_mean_glo, myThid )

        WRITE(msgBuf,'(A,I2,A,2D22.15)')
     &   ' cost_profiles_mean(',num_var,') = ',
     &   objf_prof_mean_glo,num_prof_mean_glo

        IF (num_prof_mean_glo .GT. 0.)
     &    CALL PRINT_MESSAGE( msgBuf,
     &         standardMessageUnit,SQUEEZE_RIGHT , myThid )

      ENDDO !DO num_var

#endif

      WRITE(msgBuf,'(A)') '== cost_profiles: end   =='
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit,SQUEEZE_RIGHT , myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf,
     &     standardMessageUnit,SQUEEZE_RIGHT , myThid )

      IF (prof_make_nc) THEN
        CALL PROFILES_MAKE_NCFILE( myThid )
      ENDIF

#endif

      RETURN
      END
