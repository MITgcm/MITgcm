C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_up.F,v 1.13 2012/03/30 18:25:03 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

CBOP 0
C !ROUTINE: FLT_UP

C !INTERFACE:
      SUBROUTINE FLT_UP (
     I                    myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE FLT_UP
C     | o This routine moves particles vertical from the target
C     |   depth to the surface and samples the model state over
C     |   the full water column at horizontal float position
C     |   every flt_int_prof time steps and writes output.
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "FLT_SIZE.h"
#include "FLT.h"
#include "FLT_BUFF.h"

C     !INPUT PARAMETERS:
C     myTime :: current time in simulation
C     myIter :: current iteration number
C     myThid :: my Thread Id number
      _RL myTime
      INTEGER myIter, myThid

C     !FUNCTIONS:
      _RL FLT_MAP_K2R
      EXTERNAL FLT_MAP_K2R

C     !LOCAL VARIABLES:
      INTEGER bi, bj, nFlds
      INTEGER ip, k, ii
      INTEGER imax
      PARAMETER (imax=(9+4*Nr))
      _RL tmp(imax)
      _RL ix, jy, i0x, j0y, xx, yy, zz
      _RL uu,vv,tt,ss, pp
      _RL npart_read, npart_times
      _RS dummyRS(1)
      INTEGER fp, ioUnit, irecord
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   set number of fields to write
      nFlds = 0
      IF ( flt_selectProfOutp.GE.1 ) nFlds = nFlds + 8
      IF ( flt_selectProfOutp.GE.2 ) nFlds = nFlds + 1 + 4*Nr

C--   check buffer size
      IF ( nFlds.GT.fltBufDim ) THEN
         _BEGIN_MASTER(myThid)
         WRITE(msgBuf,'(3(A,I4))') ' FLT_UP: fltBufDim=', fltBufDim,
     &                             ' too small (<', nFlds, ' )'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A)')     ' FLT_UP: => increase fltBufDim',
     &                            ' in "FLT_SIZE.h" & recompile'
         CALL PRINT_ERROR( msgBuf, myThid )
         _END_MASTER(myThid)
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R FLT_UP'
      ENDIF

      IF ( myIter.EQ.nIter0 ) RETURN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Calculate position + other fields at float position and fill up IO-buffer

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

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

           IF ( flt_selectProfOutp.GE.1 ) THEN
C     Convert to coordinates
             ix = ipart(ip,bi,bj)
             jy = jpart(ip,bi,bj)
             CALL FLT_MAP_IJLOCAL2XY( xx, yy,
     I                                ix, jy, bi,bj, myThid )
             zz = FLT_MAP_K2R( kpart(ip,bi,bj),bi,bj,myThid )

             tmp(1) = npart(ip,bi,bj)
             tmp(2) = myTime
             tmp(3) = xx
             tmp(4) = yy
             tmp(5) = zz
             tmp(6) = ix + i0x
             tmp(7) = jy + j0y
             tmp(8) = kpart(ip,bi,bj)
           ENDIF

           IF ( ( flt_selectProfOutp.GE.2 )    .AND.
     &          ( myTime.GE.tstart(ip,bi,bj) ) .AND.
     &          ( tend(ip,bi,bj).EQ.-1..OR.myTime.LE.tend(ip,bi,bj) )
     &        ) THEN
             CALL FLT_BILINEAR2D(ix,jy,pp,etaN,0,bi,bj,myThid)
             tmp(9) = pp
             DO k=1,Nr
               CALL FLT_BILINEAR  (ix,jy,uu,uVel,  k,1,bi,bj,myThid)
               CALL FLT_BILINEAR  (ix,jy,vv,vVel,  k,2,bi,bj,myThid)
               CALL FLT_BILINEAR  (ix,jy,tt,theta, k,0,bi,bj,myThid)
               CALL FLT_BILINEAR  (ix,jy,ss,salt,  k,0,bi,bj,myThid)
               tmp(9+k     ) = uu
               tmp(9+k+1*Nr) = vv
               tmp(9+k+2*Nr) = tt
               tmp(9+k+3*Nr) = ss
             ENDDO
           ELSEIF ( flt_selectProfOutp.GE.2 ) THEN
             DO ii=9,nFlds
               tmp(ii) = flt_nan
             ENDDO
           ENDIF

           DO ii=1,nFlds
             flt_io_buff(ii,ip,bi,bj) = tmp(ii)
           ENDDO

         ENDDO

       ENDDO
      ENDDO

      IF ( flt_selectProfOutp.LE.0 ) RETURN

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Write shared buffer to file

      _BARRIER
      _BEGIN_MASTER(myThid)

      fn = 'float_profiles'
      fp = writeBinaryPrec

      DO bj=1,nSy
       DO bi=1,nSx

C (1) read actual number floats from file (if exists)
         ioUnit = -2
         CALL MDS_READVEC_LOC(  fn, fp, ioUnit, 'RL', nFlds,
     &                          tmp, dummyRS,
     &                          bi, bj, 1, myThid )
         IF ( ioUnit.GT. 0 ) THEN
            npart_read  = tmp(1)
            npart_times = tmp(5)
            ii = NINT(tmp(7))
C-       for backward compatibility with old profile files:
            IF ( ii.EQ.0 ) ii = 9+4*Nr
            IF ( ii.NE.nFlds ) THEN
              WRITE(msgBuf,'(A,I4,A)')
     &            'FLT_UP: nFlds=', nFlds,' different from'
              CALL PRINT_ERROR( msgBuf, myThid )
              WRITE(msgBuf,'(3A,I4,A)')
     &            'previous file (',fn(1:14),') value =',ii
              CALL PRINT_ERROR( msgBuf, myThid )
              CALL ALL_PROC_DIE( 0 )
              STOP 'ABNORMAL END: S/R FLT_UP'
            ENDIF
C-       close the read-unit (safer to use a different unit for writing)
            CLOSE( ioUnit )
         ELSE
            npart_read  = 0.
            npart_times = 0.
            tmp(2)      = myTime
         ENDIF

C (2) write new actual number floats and time into file
C-    the standard routine mds_writevec_loc can be used here

C     total number of records in this file
         tmp(1) = DBLE(npart_tile(bi,bj))+npart_read
C     first time of writing floats (do not change when written)
c        tmp(2) = tmp(2)
C     current time
         tmp(3) = myTime
C     timestep
         tmp(4) = flt_int_prof
C     total number of timesteps
         tmp(5) = npart_times + 1.
C     total number of floats
         tmp(6) = max_npart
C     total number of fields
         tmp(7) = nFlds
         DO ii=8,nFlds
           tmp(ii) = 0.
         ENDDO
         ioUnit = -1
         CALL MDS_WRITEVEC_LOC( fn, fp, ioUnit, 'RL', nFlds,
     &                          tmp, dummyRS,
     &                          bi, bj, -1, myIter, myThid )

         DO ip=1,npart_tile(bi,bj)
C (3) write float positions into file
           irecord = npart_read+ip+1
           IF ( ip.NE.npart_tile(bi,bj) ) irecord = -irecord
           CALL MDS_WRITEVEC_LOC( fn, fp, ioUnit, 'RL', nFlds,
     &                            flt_io_buff(1,ip,bi,bj), dummyRS,
     &                            bi, bj, irecord, myIter, myThid )
         ENDDO
         CLOSE( ioUnit )

       ENDDO
      ENDDO

      _END_MASTER(myThid)
      _BARRIER

      RETURN
      END
