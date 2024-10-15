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
C     !ROUTINE: ACTIVE_READ_PROFILE_RL(
C     !INTERFACE:
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

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_read_profile_rl
C     ==================================================================
C     o Read an active 1D record from file.
C       In forward-mode (theSimulationMode = FORWARD_SIMULATION)
C       also read a mask from file.
C
C     \ev

C     !USES:
      IMPLICIT NONE

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_PROFILES
# include "netcdf.inc"
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER fid
      INTEGER active_num_file, nactive_var, active_varnum
      LOGICAL lAdInit
      INTEGER irec, irecglob
      INTEGER theSimulationMode
      INTEGER myOptimIter
      INTEGER bi, bj, myThid
C     !OUTPUT PARAMETERS:
      _RL     active_var(nactive_var)

#ifdef ALLOW_PROFILES
C     !LOCAL VARIABLES:
      INTEGER err, varId , vec_start(2), vec_count(2)
      INTEGER i, ivar, jrec
      _RL     active_data_t(nactive_var)
      REAL*8  vec_tmp(nactive_var+1)
CEOP

      IF (profilesDoNcOutput) THEN
       vec_start(1)=1
       vec_start(2)=irec
       vec_count(1)=nactive_var
       vec_count(2)=1
      ELSE
       jrec = 2 * ( (irec-1)*prof_num_var_tot(active_num_file,bi,bj)
     &      + prof_num_var_cur(active_num_file,active_varnum,bi,bj)
     &      -1 )
      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (profilesDoNcOutput) THEN

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_GET_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_var)

        err = NF_INQ_VARID(fid,prof_namesmask(active_num_file,
     &       active_varnum), varId)
        err = NF_GET_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       prof_mask1D_cur(1,bi,bj))

       ELSE

        READ(fid,rec=jrec+1) vec_tmp
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
        DO ivar=1,nactive_var
         active_var(ivar)=vec_tmp(ivar)
        ENDDO
        READ(fid,rec=jrec+2) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
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

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_GET_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_data_t)

C     Add active_var from appropriate location to data.
        DO i = 1,nactive_var
         active_data_t(i) = active_data_t(i) + active_var(i)
        ENDDO
C     Store the result on disk.

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_PUT_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_data_t)

C     Set active_var to zero.
        DO i = 1,nactive_var
         active_var(i) = 0. _d 0
        ENDDO

       ELSE

        READ(fid,rec=jrec+1) vec_tmp
#ifdef _BYTESWAPIO
        call MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
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

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_GET_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_var)

       ELSE

        READ(fid,rec=jrec+1) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
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
C     !ROUTINE: ACTIVE_WRITE_PROFILE_RL(
C     !INTERFACE:
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

C     !DESCRIPTION: \bv
C     ==================================================================
C     SUBROUTINE active_write_profile_rl
C     ==================================================================
C     o Write an active 1D record to file.
C       In forward-mode (theSimulationMode = FORWARD_SIMULATION)
C       also write a mask to file.
C
C     \ev

C     !USES:
      IMPLICIT NONE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#ifdef ALLOW_PROFILES
# include "netcdf.inc"
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif

C     !INPUT PARAMETERS:
      INTEGER fid
      INTEGER active_num_file, nactive_var, active_varnum
      INTEGER irec, irecglob
      INTEGER theSimulationMode
      INTEGER myOptimIter
      INTEGER bi,bj,myThid
      _RL     active_var(nactive_var)
C     !OUTPUT PARAMETERS:

#ifdef ALLOW_PROFILES
C     !LOCAL VARIABLES:
      INTEGER err, varId , vec_start(2), vec_count(2)
      INTEGER i, ivar, jrec
      _RL     active_data_t(nactive_var)
      real*8 vec_tmp(nactive_var+1)
CEOP

      IF (profilesDoNcOutput) THEN
       vec_start(1)=1
       vec_start(2)=irec
       vec_count(1)=nactive_var
       vec_count(2)=1
      ELSE
       jrec = 2 * ( (irec-1)*prof_num_var_tot(active_num_file,bi,bj)
     &      + prof_num_var_cur(active_num_file,active_varnum,bi,bj)
     &      -1 )
      ENDIF

C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>> FORWARD RUN <<<<<<<<<<<<<<<<<<<
C     >>>>>>>>>>>>>>>>>>>             <<<<<<<<<<<<<<<<<<<

      IF (theSimulationMode .EQ. FORWARD_SIMULATION) THEN

       _BEGIN_MASTER( myThid )

       IF (profilesDoNcOutput) THEN

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_PUT_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_var)

        err = NF_INQ_VARID(fid,prof_namesmask(active_num_file,
     &       active_varnum), varId )
        err = NF_PUT_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       prof_mask1D_cur(1,bi,bj))

        err = NF_INQ_VARID(fid,'prof_ind_glob', varId )
        err = NF_PUT_VAR1_INT(fid, varId , vec_start(2),
     &       irecglob)

       ELSE

        DO ivar=1,nactive_var
         vec_tmp(ivar)=active_var(ivar)
        ENDDO
        vec_tmp(nactive_var+1)=irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
        WRITE(fid,rec=jrec+1) vec_tmp
        DO ivar=1,nactive_var
         vec_tmp(ivar)=prof_mask1D_cur(ivar,bi,bj)
        ENDDO
        vec_tmp(nactive_var+1)=irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
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

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_GET_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_data_t)

C     Add active_var to data.
        DO i = 1,nactive_var
         active_var(i) = active_var(i) + active_data_t(i)
         active_data_t(i) = 0. _d 0
        ENDDO

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_PUT_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_data_t)

       ELSE

        READ(fid,rec=jrec+1) vec_tmp
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
        DO ivar=1,nactive_var
         active_data_t(ivar)=vec_tmp(ivar)
        ENDDO

C       Add active_var from appropriate location to data.
        DO i = 1,nactive_var
         active_var(i) = active_var(i) + active_data_t(i)
         active_data_t(i) = 0. _d 0
        ENDDO

C       Store the result on disk.
        DO ivar=1,nactive_var
         vec_tmp(ivar)=active_data_t(ivar)
        ENDDO
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
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

        err = NF_INQ_VARID(fid,
     &       prof_names(active_num_file,active_varnum), varId )
        err = NF_PUT_VARA_DOUBLE(fid, varId , vec_start, vec_count,
     &       active_var)

       ELSE

        DO ivar=1,nactive_var
         vec_tmp(ivar)=active_var(ivar)
        ENDDO
        vec_tmp(nactive_var+1)=irecglob
#ifdef _BYTESWAPIO
        CALL MDS_BYTESWAPR8(nactive_var+1,vec_tmp)
#endif
        WRITE(fid,rec=jrec+1) vec_tmp

       ENDIF

       _END_MASTER( myThid )

      ENDIF

#endif /* ALLOW_PROFILES */

      RETURN
      END
