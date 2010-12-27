C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_up.F,v 1.11 2010/12/27 19:21:23 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_UP (
     I                    myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE FLT_UP
C     ==================================================================
C     o This routine moves particles vertical from the target depth to
C       the surface and samples the model state over the full water
C       column at horizontal float position every flt_int_prof time steps
C       and writes output.
C     ==================================================================

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FLT_SIZE.h"
#include "FLT.h"

C     == routine arguments ==
      _RL myTime
      INTEGER myIter, myThid

C     == Functions ==
      _RL FLT_MAP_K2R
      EXTERNAL FLT_MAP_K2R

C     == local variables ==
      INTEGER bi, bj
      INTEGER imax
      PARAMETER (imax=(9+4*Nr))
      INTEGER ip, k, ii
      _RL ix, jy, i0x, j0y, xx, yy, zz
      _RL uu,vv,tt,ss, pp
      _RL tmp(imax)
      _RL npart_read, npart_times
      _RS dummyRS(1)
      INTEGER fp, ioUnit, irecord
      CHARACTER*(MAX_LEN_FNAM) fn

C     == end of interface ==

      fn = 'float_profiles'
      fp = writeBinaryPrec

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C (1) read actual number floats from file (if exists)
         ioUnit = -2
         CALL MDS_READVEC_LOC(  fn, fp, ioUnit,
     &                          'RL', imax, tmp, dummyRS,
     &                           bi, bj, 1, myThid )
         IF ( ioUnit.GT. 0 ) THEN
            npart_read  = tmp(1)
            npart_times = tmp(5)
C-       close the read-unit (safer to use a different unit for writing)
            CLOSE( ioUnit )
         ELSE
            npart_read  = 0.
            npart_times = 0.
            tmp(2)      = myTime
         ENDIF

C the standard routine mds_writevec_loc can be used here
C (2) write new actual number floats and time into file
C
C total number of records in this file
         tmp(1) = DBLE(npart_tile(bi,bj))+npart_read
C first time of writing floats (do not change when written)
c        tmp(2) = tmp(2)
C current time
         tmp(3) = myTime
C timestep
         tmp(4) = flt_int_prof
C total number of timesteps
         tmp(5) = npart_times + 1.
C total number of floats
         tmp(6) = max_npart
         DO ii=7,imax
            tmp(ii) = 0.
         ENDDO
         ioUnit = -1
         CALL MDS_WRITEVEC_LOC( fn, fp, ioUnit,
     &                          'RL', imax, tmp, dummyRS,
     &                          bi,bj,-1, myIter, myThid )

         i0x = DFLOAT( myXGlobalLo-1 + (bi-1)*sNx )
         j0y = DFLOAT( myYGlobalLo-1 + (bj-1)*sNy )
         DO ip=1,npart_tile(bi,bj)

C     Move float to the surface
           IF ( myTime.GE.tstart(ip,bi,bj) .AND.
     &         (tend(ip,bi,bj).EQ.-1..OR.myTime.LE.tend(ip,bi,bj))
     &         .AND.
     &          kpart(ip,bi,bj).EQ.kfloat(ip,bi,bj) .AND.
     &          iup(ip,bi,bj).GT.0.
     &        ) THEN

             IF ( MOD(myTime,iup(ip,bi,bj)).EQ.0.)
     &       kpart(ip,bi,bj) = flt_surf

           ENDIF

C     If float has died move to level 0
           IF ( tend(ip,bi,bj).NE.-1..AND.myTime.GT.tend(ip,bi,bj)
     &        ) THEN
             kpart(ip,bi,bj) = 0.
           ENDIF

C     Convert to coordinates
           ix = ipart(ip,bi,bj)
           jy = jpart(ip,bi,bj)
           CALL FLT_MAP_IJLOCAL2XY( xx, yy,
     I                              ix, jy, bi,bj, myThid )
           zz = FLT_MAP_K2R( kpart(ip,bi,bj),bi,bj,myThid )

           tmp(1) = npart(ip,bi,bj)
           tmp(2) = myTime
           tmp(3) = xx
           tmp(4) = yy
           tmp(5) = zz
           tmp(6) = ix + i0x
           tmp(7) = jy + j0y
           tmp(8) = kpart(ip,bi,bj)

           IF ( myTime.GE.tstart(ip,bi,bj) .AND.
     &         (tend(ip,bi,bj).EQ.-1..OR.myTime.LE.tend(ip,bi,bj))
     &        ) THEN

             CALL FLT_BILINEAR2D(ix,jy,pp,etaN,0,bi,bj,myThid)
             tmp(9)   = pp
             DO k=1,Nr
               CALL FLT_BILINEAR  (ix,jy,uu,uVel,  k,1,bi,bj,myThid)
               CALL FLT_BILINEAR  (ix,jy,vv,vVel,  k,2,bi,bj,myThid)
               CALL FLT_BILINEAR  (ix,jy,tt,theta, k,0,bi,bj,myThid)
               CALL FLT_BILINEAR  (ix,jy,ss,salt,  k,0,bi,bj,myThid)
               tmp(9+k)      = uu
               tmp(9+1*Nr+k) = vv
               tmp(9+2*Nr+k) = tt
               tmp(9+3*Nr+k) = ss
             ENDDO

           ELSE
             DO ii=9,imax
               tmp(ii) = flt_nan
             ENDDO
           ENDIF

C (3) write float positions into file
           irecord = npart_read+ip+1
           IF ( ip.NE.npart_tile(bi,bj) ) irecord = -irecord
           CALL MDS_WRITEVEC_LOC( fn, fp, ioUnit,
     &                            'RL', imax, tmp, dummyRS,
     &                            bi,bj,irecord, myIter, myThid )

         ENDDO
         CLOSE( ioUnit )

       ENDDO
      ENDDO

      RETURN
      END
