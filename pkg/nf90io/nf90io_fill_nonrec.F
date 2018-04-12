
#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#undef  MULTIPLE_RECORD_STATE_FILES

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP

C     !INTERFACE:
      SUBROUTINE NF90IO_FILL_NONREC (
     I                              ncid,
     I                              myThid )
        use netcdf
        IMPLICIT NONE

#include "SIZE.h"
#include "BUILD_INFO.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
C #include "mpif.h"  EESupport already includes this
C #include "NF90IO.h"
#include "PARAMS.h"
#include "GRID.h"
#include "NF90IO.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

        INTEGER ncid
        INTEGER myThid

        INTEGER mode_flag, err, i, j, k, t
        INTEGER bi, bj
        INTEGER i_dimid, j_dimid, k_dimid, dimids(4), varid
        INTEGER ig_dimid, jg_dimid, kl_dimid, ku_dimid, kp1_dimid
        INTEGER start(4), count(4), rec_dimid
        LOGICAL iAmDoingIO
        character*(MAX_LEN_MBUF) msgbuf
        _RL     a(sNx,sNy,Nr)
        _RL     is(sNx+10)
        _RL     js(sNy+10)
        _RL     ks(Nr+10)

C     co-ordinate k:
        err = nf90_inq_varid(ncid, "k", varid)
        CALL nf90ERR(err, "Getting k varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting k variable to par access",myThid)
        ks(1:Nr) = (/  (I, I = 0, Nr-1)  /)
        err = nf90_put_var(ncid, varid, kS(1:Nr),
     $       start = (/ 1 /) , count = (/ Nr /) )
        CALL nf90ERR(err, "Setting k variable values",myThid)

C     co-ordinate i:
        err = nf90_inq_varid(ncid, "i", varid)
        CALL nf90ERR(err, "Getting i varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting i variable to par access",myThid)
        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              is(1:sNx) = (/  (I, I = myXGlobalLo+(bi-1)*sNx-1,
     $             myXGlobalLo+(bi)*sNx-1)  /)
              err = nf90_put_var(ncid, varid,
     &             is(1:sNx),
     &             start = (/ myXGlobalLo+(bi-1)*sNx /) ,
     &             count = (/ sNx/) )
              CALL nf90ERR(err, "Getting i varid",myThid)
           enddo
        enddo

C     co-ordinate i_g:
        err = nf90_inq_varid(ncid, "i_g", varid)
        CALL nf90ERR(err, "Getting i_g varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting i_g variable to par access",myThid)
        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              is(1:sNx) = (/  (I, I = myXGlobalLo+(bi-1)*sNx-1,
     $             myXGlobalLo+(bi)*sNx-1)  /)
              err = nf90_put_var(ncid, varid,
     &             is(1:sNx),
     &             start = (/ myXGlobalLo+(bi-1)*sNx /) ,
     &             count = (/ sNx/) )
              CALL nf90ERR(err, "Getting i_g varid",myThid)
           enddo
        enddo

C     co-ordinate j:
        err = nf90_inq_varid(ncid, "j", varid)
        CALL nf90ERR(err, "Getting j varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting j variable to par access",myThid)

        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              js(1:sNy) = (/  (I, I = myYGlobalLo+(bj-1)*sNy-1,
     &             myYGlobalLo+(bj)*sNy-1)  /)
              err = nf90_put_var(ncid, varid,
     &             js(1:sNy),
     &             start = (/ myYGlobalLo+(bj-1)*sNy /) ,
     &             count = (/ sNy/) )
              CALL nf90ERR(err, "Getting j varid",myThid)
           enddo
        enddo

C     co-ordinate j:
        err = nf90_inq_varid(ncid, "j_g", varid)
        CALL nf90ERR(err, "Getting j_g varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting j_g variable to par access",myThid)

        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              js(1:sNy) = (/  (I, I = myYGlobalLo+(bj-1)*sNy-1,
     &             myYGlobalLo+(bj)*sNy-1)  /)
              err = nf90_put_var(ncid, varid,
     &             js(1:sNy),
     &             start = (/ myYGlobalLo+(bj-1)*sNy /) ,
     &             count = (/ sNy/) )
              CALL nf90ERR(err, "Putting j_g varid",myThid)
           enddo
        enddo


C     co-ordinate ks:
        do t = 1,3
           if (t.EQ.1) then
              err = nf90_inq_varid(ncid, "k", varid)
           else if (t.EQ.2) then
              err = nf90_inq_varid(ncid, "k_u", varid)
           else if (t.EQ.3) then
              err = nf90_inq_varid(ncid, "k_l", varid)
           endif
           CALL nf90ERR(err, "Getting k varid",myThid)
           err = nf90io_var_par_access(ncid, varid)
           CALL nf90ERR(err, "Setting k variable to par access"
     $          ,myThid)

           ks(1:Nr) = (/  (I, I = 0,Nr-1)  /)
           err = nf90_put_var(ncid, varid,
     &          ks(1:Nr),
     &          start = (/ 1 /) ,
     &          count = (/ Nr /) )
           CALL nf90ERR(err, "Getting k varid",myThid)
        enddo

C     co-ordinate k+:
        err = nf90_inq_varid(ncid, "k_p1", varid)
        CALL nf90ERR(err, "Getting k_p1 varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        CALL nf90ERR(err, "Setting k_p1 variable to par access"
     $       ,myThid)
        ks(1:Nr+1) = (/  (I, I = 0,Nr)  /)
        err = nf90_put_var(ncid, varid,
     &       ks(1:Nr+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )
        CALL nf90ERR(err, "Writing k_p1",myThid)


CCCCCCCCCCCCCC
C     Now fill the grid data from variables...
C     z-s
        err = nf90_inq_varid(ncid, "Z", varid)
        CALL nf90ERR(err, "Getting Z varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rC,
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

        err = nf90_inq_varid(ncid, "Zp1", varid)
        CALL nf90ERR(err, "Getting Zp1 varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rF,
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )

        err = nf90_inq_varid(ncid, "Zu", varid)
        CALL nf90ERR(err, "Getting Zu varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rF(2:Nr+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

        err = nf90_inq_varid(ncid, "drC", varid)
        CALL nf90ERR(err, "Getting drC varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       drC(1:Nr+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )
        CALL nf90ERR(err, "writing drC ",myThid)

        err = nf90_inq_varid(ncid, "drF", varid)
        CALL nf90ERR(err, "Getting drF varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       drF(1:Nr),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )
        CALL nf90ERR(err, "writing drF",myThid)


        err = nf90_inq_varid(ncid, "Zl", varid)
        CALL nf90ERR(err, "Getting Zl varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       rF(1:Nr),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

C     Horizontal co-ordinates...

        CALL NF90IO_FILL_2Dnorec(ncid, "XC", xC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "XG", xG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "YC", yC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "YG", yG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dxC", dxC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dxG", dxG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dyC", dxC, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "dyG", dxG, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rA", rA, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rAz", rAz, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rAw", rAw, myThid)
        CALL NF90IO_FILL_2Dnorec(ncid, "rAs", rAs, myThid)

C     Hfacs
        CALL NF90IO_FILL_3Dnorec(ncid, "hFacC", hFacC, myThid)
        CALL NF90IO_FILL_3Dnorec(ncid, "hFacW", hFacW, myThid)
        CALL NF90IO_FILL_3Dnorec(ncid, "hFacS", hFacS, myThid)

C     Depth:  Special because model doesn't store
        err = nf90_inq_varid(ncid, "Depth", varid)
        CALL nf90ERR(err, "Getting varid Depth",myThid)
        err = nf90io_var_par_access(ncid, varid)
        count(1:2) = (/  sNx, sNy /)
        do bj = 1,nSy
           do bi = 1,nSx
C     NOT tested w/ more than one thread be processor, so
C     maybe incorrect.
              start(1:2) = (/ myXGlobalLo+(bi-1)*sNx,
     &             myYGlobalLo+(bj-1)*sNy /)
              err = nf90_put_var(ncid, varid,
     &             Ro_surf(1:sNx, 1:sNy,bi,bj) -
     &             R_low(1:sNx, 1:sNy,bi,bj),
     &             start = start(1:2), count = count(1:2))
              CALL nf90ERR(err, "Putting Depth into file", myThid)
           enddo
        enddo

C     Reference densities...

        DO k=1,Nr+1
           ks(k) = phiRef(2*k-1)
        ENDDO
        err = nf90_inq_varid(ncid, "PHrefF", varid)
        CALL nf90ERR(err, "Getting PHRefF varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       ks(1:NR+1),
     &       start = (/ 1 /) ,
     &       count = (/ Nr+1 /) )

        DO k=1,Nr
           ks(k) = phiRef(2*k)
        ENDDO
        err = nf90_inq_varid(ncid, "PHrefC", varid)
        CALL nf90ERR(err, "Getting PHRefC varid",myThid)
        err = nf90io_var_par_access(ncid, varid)
        err = nf90_put_var(ncid, varid,
     &       ks(1:NR),
     &       start = (/ 1 /) ,
     &       count = (/ Nr /) )

        RETURN
        END
