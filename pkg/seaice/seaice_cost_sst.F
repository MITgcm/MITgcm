#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE SEAICE_COST_SST(
     &     nnzbar, localbarfile, localbar, xx_localbar_mean_dummy,
     &             areabarfile,   areabar, xx_areabar_mean_dummy,
     &     nnzobs, localobsfile, localobs, mult_local,
     &     nrecloc, localstartdate, localperiod,
     &     localmask, localweight,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_local, num_local,
     &     myTime, myIter, myThid )

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#include "SIZE.h"
#ifdef ALLOW_COST
# ifdef ALLOW_CAL
#  include "cal.h"
# endif
# ifdef ALLOW_CTRL
#  include "optim.h"
# endif
# ifdef ALLOW_SEAICE
#  include "SEAICE_COST.h"
# endif
#endif

C     == routine arguments ==
      INTEGER nnzbar
      CHARACTER*(MAX_LEN_FNAM) localbarfile
      _RL localbar   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL xx_localbar_mean_dummy
      CHARACTER*(MAX_LEN_FNAM) areabarfile
      _RL areabar    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzbar,nSx,nSy)
      _RL xx_areabar_mean_dummy
      INTEGER nnzobs
      CHARACTER*(MAX_LEN_FNAM) localobsfile
      _RL localobs   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs,nSx,nSy)
      _RL mult_local
      INTEGER nrecloc
      INTEGER localstartdate(4)
      _RL localperiod
      _RS localmask  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL localweight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL objf_local(nSx,nSy)
      _RL num_local(nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#if (defined ALLOW_COST && defined ALLOW_CAL && \
     defined ALLOW_SEAICE_COST_SMR_AREA)

C     == external functions ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     == local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
      INTEGER jmin,jmax
      INTEGER imin,imax
      INTEGER irec
      INTEGER  il
      INTEGER localrec
      INTEGER obsrec
      LOGICAL doglobalread
      LOGICAL ladinit
      _RL spval
      PARAMETER (spval = -9999. )
      _RL localwww
      _RL localcost
      _RL junk
      _RL cmask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nnzobs)
      CHARACTER*(128) fname1, fname2,fname3
Cnew(
      _RL daytime
      _RL diffsecs
      INTEGER dayiter
      INTEGER daydate(4)
      INTEGER difftime(4)
      INTEGER middate(4)
      INTEGER yday, ymod
      INTEGER md, dd, sd, ld, wd
      LOGICAL exst
Cnew)
C     == end of interface ==

C- jmc: params SEAICE_freeze has been retired; set it locally until someone
C       who knows what this cost-cointribution means fix it.
      _RL SEAICE_freeze
       SEAICE_freeze  = -1.96  _d 0

C--   This S/R is called only if useSEAICE & useCAL, otherwise it is meaningless

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

C--   Initialise local variables.

      localwww = 0. _d 0

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
          objf_local(bi,bj) = 0. _d 0
          num_local(bi,bj) = 0. _d 0
        ENDDO
      ENDDO

C--   First, read tiled data.
      doglobalread = .false.
      ladinit      = .false.

      fname1 = ' '
      fname3 = ' '
      il = ILNBLNK( localbarfile )
      WRITE(fname1,'(2A,I10.10)') localbarfile(1:il),'.',optimcycle
      il = ILNBLNK( areabarfile )
      WRITE(fname3,'(2A,I10.10)') areabarfile(1:il),'.',optimcycle

      IF ( localobsfile.NE.' ' ) THEN

C--   Loop over records for the second time.
       DO irec = 1, nrecloc

         IF ( nnzbar .EQ. 1 ) THEN
           CALL active_read_xy( fname1, localbar, irec, doglobalread,
     &                      ladinit, optimcycle, myThid,
     &                      xx_localbar_mean_dummy )
         ELSE
           CALL active_read_xyz( fname1, localbar, irec, doglobalread,
     &                       ladinit, optimcycle, myThid,
     &                       xx_localbar_mean_dummy )
         ENDIF

         IF ( nnzbar .EQ. 1 ) THEN
           CALL active_read_xy( fname3, areabar, irec, doglobalread,
     &                      ladinit, optimcycle, myThid,
     &                      xx_areabar_mean_dummy )
         ELSE
           CALL active_read_xyz( fname3, areabar, irec, doglobalread,
     &                       ladinit, optimcycle, myThid,
     &                       xx_areabar_mean_dummy )
         ENDIF

Cnew(
         obsrec = irec

         daytime = FLOAT(secondsperday*(irec-1)) + modelstart
         dayiter = hoursperday*(irec-1)+modeliter0

         CALL cal_getdate( dayiter, daytime, daydate, myThid )
         CALL cal_convdate( daydate,yday,md,dd,sd,ld,wd,myThid )
         ymod = smrareastartdate(1)/10000

#ifdef SEAICE_DEBUG
         print *,'cost_seaice_sst_r2'
         print *,'daydate        ', daydate
         print *,'areaobsfile:  ', localobsfile
         print *,'nrecloc        ', nrecloc
         print *,'obsrec,daytime ', obsrec,daytime
         print *,'dayiter        ', dayiter
         print *,'yday :         ', yday
         print *,'md,dd,sd       ', md,dd,sd
         print *,'ld,wd          ', ld,wd
         print *,'loclstrtdte(1) ', localstartdate(1)
         print *,'ymod, yday     ', ymod,yday
         print *,'smrarstrtdt(1) ', smrareastartdate(1)
         print *,'smrarstartdate ', smrareastartdate
#endif /* SEAICE_DEBUG */

         IF ( ymod .EQ. yday ) THEN
         middate(1) = smrareastartdate(1)
         ELSE
         middate(1) = yday*10000+100+1
         ENDIF
         middate(2) = 0
         middate(3) = modelstartdate(3)
         middate(4) = modelstartdate(4)

         CALL cal_TimePassed( middate, daydate, difftime, myThid )
         CALL cal_ToSeconds( difftime, diffsecs, myThid )

         localrec = int(diffsecs/localperiod) + 1

#ifdef SEAICE_DEBUG
         print *,'middate(1,2)     ',middate(1),middate(2)
         print *,'middate(3,4)     ', middate(3),middate(4)
         print *,'difftime,diffsecs',difftime,diffsecs
         print *,'localrec         ',localrec
#endif

         fname2 = ' '
         il = ILNBLNK(localobsfile)
         WRITE(fname2,'(2A,I4)') localobsfile(1:il), '_', yday
         INQUIRE( FILE=fname2, EXIST=exst )

#ifdef SEAICE_DEBUG
         print *,'fname2',fname2
#endif
         IF (.NOT. exst) THEN
           WRITE(fname2(1:128),'(a)') localobsfile(1:il)
           localrec = obsrec
#ifdef SEAICE_DEBUG
           print *,'localrec ', localrec
           print *,'not exist'
#endif
         ENDIF

         IF ( localrec .GT. 0 ) THEN
#ifdef SEAICE_DEBUG
          print *,'calling mdsreadfile',fname2,localrec
#endif
          CALL READ_REC_3D_RL( fname2, cost_ice_iprec, nnzobs,
     &                         localobs, localrec, 1, myThid )
         ELSE
          DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
              DO k = 1,nnzobs
                DO j = jmin,jmax
                  DO i = imin,imax
                     localobs(i,j,k,bi,bj) = spval
                  ENDDO
                ENDDO
              ENDDO
            ENDDO
          ENDDO
         ENDIF
Cnew)

#ifdef SEAICE_DEBUG
         DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             DO k = 1,nnzobs
               DO i = imin,imax
                 DO j = jmin,jmax
                   IF (localobs(i,j,k,bi,bj) .LT. -1) THEN
                     print *,'obs rec date: ', -localobs(i,j,1,bi,bj)
                   ENDIF
                 ENDDO
               ENDDO
             ENDDO
           ENDDO
         ENDDO
#endif

         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)

            localcost    = 0. _d 0

C--           Determine the mask on weights
            DO k = 1,nnzobs
             DO j = jmin,jmax
              DO i = imin,imax
               cmask(i,j,k) = cosphi(i,j,bi,bj)*localmask(i,j,k,bi,bj)
               IF ( localobs(i,j,k,bi,bj) .LT. spminloc .OR.
     &              localobs(i,j,k,bi,bj) .GT. spmaxloc .OR.
     &              localobs(i,j,k,bi,bj) .EQ. spzeroloc ) THEN
                  cmask(i,j,k) = 0. _d 0
               ENDIF
              ENDDO
             ENDDO
            ENDDO
C--
            DO k = 1,nnzobs
             DO j = jmin,jmax
              DO i = imin,imax
                localwww  = localweight(i,j,bi,bj)*cmask(i,j,k)

C only accumulate cost if there is ice observed but not simulated..
                IF ( localobs(i,j,k,bi,bj) .GT. 0.0 .AND.
     &                areabar(i,j,1,bi,bj) .LE. 0.0) THEN

                   junk = ( localbar(i,j,k,bi,bj) -
     &                      SEAICE_freeze + SEAICE_clamp_theta )

                ELSE
                   junk = 0.0
                ENDIF

                localcost = localcost + junk*junk*localwww

#ifdef SEAICE_DEBUG
                IF ( ( i.EQ.SEAICE_debugPointX ) .AND.
     &               ( j.EQ.SEAICE_debugPointY ) ) THEN
                   print '(A,2i4,3(1x,1P2E15.3))',
     &                 'costg i j tbar, abar,obs     ',i,j,
     &                 localbar(i,j,k,bi,bj),
     &                 areabar(i,j,k,bi,bj),
     &                 localobs(i,j,k,bi,bj)
                   print '(A,2i4,2(1x,1P2E15.3))',
     &                 'costg i j bar-obs,wgt,loCost ',i,j,
     &                 junk,
     &                 localwww,
     &                 localcost
                ENDIF
#endif

                IF ( localwww .NE. 0. )
     &               num_local(bi,bj) = num_local(bi,bj) + 1. _d 0
              ENDDO
             ENDDO
            ENDDO

            objf_local(bi,bj) = objf_local(bi,bj) + localcost

C-    End bi,bj loops
          ENDDO
         ENDDO

       ENDDO
C--   End of second loop over records.

C--   End of mult_local or localobsfile
      ENDIF

#endif /* ifdef ALLOW_COST and ALLOW_CAL and ALLOW_SEAICE_COST_SMR_AREA */

      RETURN
      END
