#include "PROFILES_OPTIONS.h"

C     ==================================================================
C     active_file_control_profiles.F: Routines that handle the I/O of
C                                     active variables for the adjoint
C                                     calculations, related to netcdf
C                                     profiles data files
C
C     Routines
C     o  active_read_profile_rl  - Read an active 1 record from file
C                                  fwd-mode only: including a mask
C     o  active_write_profile_rl - Write nn active 1D record to file.
C                                  fwd-mode only: including a mask
C
C     started: Gael Forget 15-March-2006
C     ==================================================================

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ACTIVE_READ_PROFILE_RL

C !INTERFACE:
      SUBROUTINE ACTIVE_READ_PROFILE_RL(
     I                                fid,
     I                                active_num_file,
     I                                nactive_var,
     O                                active_var,
     I                                active_varnum,
     I                                lAdInit,
     I                                irec,
     I                                irecglob,
     I                                theSimulationMode,
     I                                myOptimIter,
     I                                bi,
     I                                bj,
     I                                myThid
     &                              )

C     !DESCRIPTION:
C     Read an active 1D record from file. In forward-mode
C     (theSimulationMode = FORWARD_SIMULATION) also read a mask from
C     file.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_PROFILES
# include "netcdf.inc"
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT PARAMETERS:
C     myThid :: my thread ID number
      INTEGER fid
      INTEGER active_num_file, nactive_var, active_varnum
      LOGICAL lAdInit
      INTEGER irec, irecglob
      INTEGER theSimulationMode
      INTEGER myOptimIter
      INTEGER bi, bj, myThid
C     !OUTPUT PARAMETERS:
      _RL     active_var(nactive_var)
CEOP

#ifdef ALLOW_PROFILES
C     !LOCAL VARIABLES:
      INTEGER err, varId, maskId, vec_start(2), vec_count(2)
      INTEGER i, ivar, jrec
      _RL     active_data_t(nactive_var)
      REAL*8  vec_tmp(nactive_var+1)

      IF (profilesDoNcOutput) THEN
        vec_start(1)=1
        vec_start(2)=irec
        vec_count(1)=nactive_var
        vec_count(2)=1

C ESCO: Put a single NF_INQ_VARID for prof_names
        err = NF_INQ_VARID( fid,
     &        prof_names(active_num_file,active_varnum), varId )
        CALL PROFILES_NF_ERROR(
     &       'ACTIVE_READ: NF_INQ_VARID prof_names',
     &       err,bi,bj,myThid )

      ELSE
        jrec = 2 * ( (irec-1)*prof_num_var_tot(active_num_file,bi,bj)
     &       + prof_num_var_cur(active_num_file,active_varnum,bi,bj)
     &       -1 )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

        _BEGIN_MASTER( myThid )

        IF (profilesDoNcOutput) THEN
          err = NF_GET_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_var )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE active_var',
     &         err,bi,bj,myThid )

          err = NF_INQ_VARID( fid,
     &          prof_namesmask(active_num_file,active_varnum),
     &          maskId )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_READ: NF_INQ_VARID prof_namesmask',
     &         err,bi,bj,myThid )
          err = NF_GET_VARA_DOUBLE( fid, maskId, vec_start, vec_count,
     &          prof_mask1D_cur(1,bi,bj) )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE prof_mask1D_cur',
     &         err,bi,bj,myThid )

        ELSE

          READ(fid,rec=jrec+1) vec_tmp
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          DO ivar=1,nactive_var
            active_var(ivar)=vec_tmp(ivar)
          ENDDO

          READ(fid,rec=jrec+2) vec_tmp
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          DO ivar=1,nactive_var
            prof_mask1D_cur(ivar,bi,bj)=vec_tmp(ivar)
          ENDDO

        ENDIF

        _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        _BEGIN_MASTER( myThid )

        IF (profilesDoNcOutput) THEN
          err = NF_GET_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_data_t )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE AD active_data_t',
     &         err,bi,bj,myThid )

C     Add active_var from appropriate location to data.
          DO i = 1,nactive_var
            active_data_t(i) = active_data_t(i) + active_var(i)
          ENDDO
C     Store the result on disk.

          err = NF_PUT_VARA_DOUBLE(fid, varId, vec_start, vec_count,
     &          active_data_t )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_READ: NF_PUT_VARA_DOUBLE AD active_data_t',
     &         err,bi,bj,myThid )

C     Set active_var to zero.
          DO i = 1,nactive_var
            active_var(i) = 0. _d 0
          ENDDO

        ELSE

          READ(fid,rec=jrec+1) vec_tmp
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          DO ivar=1,nactive_var
            active_data_t(ivar)=vec_tmp(ivar)
          ENDDO

C     Add active_var from appropriate location to data.
          DO i = 1,nactive_var
            active_data_t(i) = active_data_t(i) + active_var(i)
          ENDDO

C     Store the result on disk.
          DO ivar=1,nactive_var
            vec_tmp(ivar)=active_data_t(ivar)
          ENDDO
#ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
          WRITE(fid,rec=jrec+1) vec_tmp

C     Set active_var to zero.
          DO i = 1,nactive_var
            active_var(i) = 0. _d 0
          ENDDO

        ENDIF

        _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN

        _BEGIN_MASTER( myThid )

        IF (profilesDoNcOutput) THEN
          err = NF_GET_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_var )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_READ: NF_GET_VARA_DOUBLE TL active_var',
     &         err,bi,bj,myThid )

        ELSE

          READ(fid,rec=jrec+1) vec_tmp
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          DO ivar=1,nactive_var
            active_var(ivar)=vec_tmp(ivar)
          ENDDO

        ENDIF

        _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_PROFILES */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: ACTIVE_WRITE_PROFILE_RL

C !INTERFACE:
      SUBROUTINE ACTIVE_WRITE_PROFILE_RL(
     I                                 fid,
     I                                 active_num_file,
     I                                 nactive_var,
     I                                 active_var,
     I                                 active_varnum,
     I                                 irec,
     I                                 irecglob,
     I                                 theSimulationMode,
     I                                 myOptimIter,
     I                                 bi,
     I                                 bj,
     I                                 myThid
     &                               )

C     !DESCRIPTION:
C     Write an active 1D record to file. In forward-mode
C     (theSimulationMode = FORWARD_SIMULATION) also write a mask to file.

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_PROFILES
# include "netcdf.inc"
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT PARAMETERS:
C     myThid :: my thread ID number
      INTEGER fid
      INTEGER active_num_file, nactive_var, active_varnum
      INTEGER irec, irecglob
      INTEGER theSimulationMode
      INTEGER myOptimIter
      INTEGER bi,bj,myThid
      _RL     active_var(nactive_var)
C     !OUTPUT PARAMETERS:
CEOP

#ifdef ALLOW_PROFILES
C     !LOCAL VARIABLES:
      INTEGER err, varId, maskId, vec_start(2), vec_count(2)
      INTEGER i, ivar, jrec
      _RL     active_data_t(nactive_var)
      real*8  vec_tmp(nactive_var+1)

      IF (profilesDoNcOutput) THEN
        vec_start(1)=1
        vec_start(2)=irec
        vec_count(1)=nactive_var
        vec_count(2)=1

C ESCO: Put a single NF_INQ_VARID for prof_names
        err = NF_INQ_VARID( fid,
     &        prof_names(active_num_file,active_varnum), varId )
        CALL PROFILES_NF_ERROR(
     &       'ACTIVE_WRITE: NF_INQ_VARID prof_names',
     &       err,bi,bj,myThid )

      ELSE
        jrec = 2 * ( (irec-1)*prof_num_var_tot(active_num_file,bi,bj)
     &       + prof_num_var_cur(active_num_file,active_varnum,bi,bj)
     &       -1 )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

        _BEGIN_MASTER( myThid )

        IF (profilesDoNcOutput) THEN
          err = NF_PUT_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_var )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE active_var',
     &         err,bi,bj,myThid )

          err = NF_INQ_VARID( fid,
     &          prof_namesmask(active_num_file,active_varnum),
     &          maskId )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_INQ_VARID prof_namesmask',
     &         err,bi,bj,myThid )
          err = NF_PUT_VARA_DOUBLE(fid, maskId, vec_start, vec_count,
     &          prof_mask1D_cur(1,bi,bj) )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE prof_mask1D_cur',
     &         err,bi,bj,myThid )

          err = NF_INQ_VARID( fid, 'prof_ind_glob', maskId )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_INQ_VARID prof_ind_glob',
     &         err,bi,bj,myThid )
          err = NF_PUT_VAR1_INT( fid, maskId, vec_start(2), irecglob )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VAR1_INT irecglob',
     &         err,bi,bj,myThid )

        ELSE

          DO ivar=1,nactive_var
            vec_tmp(ivar)=active_var(ivar)
          ENDDO
          vec_tmp(nactive_var+1)=irecglob
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          WRITE(fid,rec=jrec+1) vec_tmp
          DO ivar=1,nactive_var
            vec_tmp(ivar)=prof_mask1D_cur(ivar,bi,bj)
          ENDDO
          vec_tmp(nactive_var+1)=irecglob
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          WRITE(fid,rec=jrec+2) vec_tmp

        ENDIF

        _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> ADJOINT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. REVERSE_SIMULATION) THEN

        _BEGIN_MASTER( myThid )

        IF (profilesDoNcOutput) THEN
          err = NF_GET_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_data_t )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_GET_VARA_DOUBLE AD active_data_t',
     &         err,bi,bj,myThid )

C     Add active_var to data.
          DO i = 1,nactive_var
            active_var(i) = active_var(i) + active_data_t(i)
            active_data_t(i) = 0. _d 0
          ENDDO

          err = NF_PUT_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_data_t )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE AD active_data_t',
     &         err,bi,bj,myThid )

        ELSE

          READ(fid,rec=jrec+1) vec_tmp
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          DO ivar=1,nactive_var
            active_data_t(ivar)=vec_tmp(ivar)
          ENDDO

C         Add active_var from appropriate location to data.
          DO i = 1,nactive_var
            active_var(i) = active_var(i) + active_data_t(i)
            active_data_t(i) = 0. _d 0
          ENDDO

C         Store the result on disk.
          DO ivar=1,nactive_var
            vec_tmp(ivar)=active_data_t(ivar)
          ENDDO
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          WRITE(fid,rec=jrec+1) vec_tmp

        ENDIF

        _END_MASTER( myThid )

      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> TANGENT RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. TANGENT_SIMULATION) THEN

        _BEGIN_MASTER( myThid )

        IF (profilesDoNcOutput) THEN
          err = NF_PUT_VARA_DOUBLE( fid, varId, vec_start, vec_count,
     &          active_var )
          CALL PROFILES_NF_ERROR(
     &         'ACTIVE_WRITE: NF_PUT_VARA_DOUBLE TL active_var',
     &         err,bi,bj,myThid )

        ELSE

          DO ivar=1,nactive_var
            vec_tmp(ivar)=active_var(ivar)
          ENDDO
          vec_tmp(nactive_var+1)=irecglob
# ifdef _BYTESWAPIO
          CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
# endif
          WRITE(fid,rec=jrec+1) vec_tmp

        ENDIF

        _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_PROFILES */

      RETURN
      END
