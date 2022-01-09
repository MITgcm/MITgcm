#include "ECCO_OPTIONS.h"

      subroutine cost_gencost_moc(myThid)

c     ==================================================================
c     SUBROUTINE cost_gencost_moc
c     ==================================================================
c
c     o Evaluate cost function contributions from MOC defined:
c
c       MOC_max = max_k { cumsum_k { zonally integrated meridional volume transport } }
c
c     o mybar loads trVol from barfile, which is masked by S/W edge mask
c       these masks denote the latitude line for taking zonal integral
c
c     ==================================================================
c     SUBROUTINE cost_gencost_moc
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif

c     == routine arguments ==
      integer myThid

#ifdef ALLOW_GENCOST_CONTRIBUTION

c     == local variables ==

      integer nrecloc, ioUnit

      _RL mybar     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL gencost_mskTemporal
      _RL tmpVar(1), dummyRL
      _RS dummyRS(1)
      _RL tmpCumSumTile(Nr,nSx,nSy)
      _RL tmpNumTile(nSx,nSy)
      _RL tmpCumSumGlo(Nr)
      _RL tmpTile(nSx,nSy)
      _RL myTempMax

      character*(MAX_LEN_FNAM) mybarfile
      character*(MAX_LEN_MBUF) msgbuf
      character*(128) fname0

      integer kgen, kg3
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer imin, imax
      integer jmin, jmax
      integer irec
      integer il
      integer kmax
      logical doglobalread
      logical ladinit
      logical exst

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = myByLo(myThid)
      jthi = myByHi(myThid)
      itlo = myBxLo(myThid)
      ithi = myBxHi(myThid)
      imin = 1
      imax = sNx
      jmin = 1
      jmax = sNy

CADJ  INIT tapelev_moc   = common, Nr

      do kgen=1,NGENCOST
        kg3 = gencost_pointer3d(kgen)

c-- detect the relevant gencost indices
        if ( (gencost_name(kgen)(1:3).EQ.'moc').AND.
     &     (using_gencost(kgen)) ) then

          write(msgbuf,'(A)') 'Inside cost_gencost_moc ...'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )

          il=ilnblnk(gencost_barfile(kgen))
          write(msgbuf,'(A,i4,A,A)') 'Cost ',kgen,
     &    ': ',gencost_barfile(kgen)(1:il)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

c=============== PART 0: initilization ===================

c-- local variables

        doglobalread = .false.
        ladinit = .false.
        dummyRL = gencost_dummy(kgen)
        nrecloc=gencost_nrec(kgen)
        il=ilnblnk(gencost_barfile(kgen))
        write(mybarfile,'(2a,i10.10)')
     &    gencost_barfile(kgen)(1:il),'.',eccoiter

c-- Initialize to zero
          DO bj = jtlo,jthi
            DO bi = itlo,ithi
              do k = 1, Nr
                tmpCumSumTile(k,bi,bj) = 0. _d 0
              enddo
              tmpNumTile(bi,bj) = 0. _d 0
            ENDDO
          ENDDO
c model mask[W,S]: already included in transp calc in ecco_phys
        nrecloc=gencost_nrec(kgen)

c=============== PART 1: main loop ===================
        do irec = 1,nrecloc

c-- Read barfile
#ifdef ALLOW_AUTODIFF
          call active_read_xyz( mybarfile, mybar, irec,
     &                       doglobalread, ladinit,
     &                       eccoiter, myThid,
     &                       dummyRL )
#else
          call READ_REC_XYZ_RL( mybarfile, mybar, irec,
     &                       1, myThid )
#endif /* ALLOW_AUTODIFF */

c-- Initialize after read

          DO bj = jtlo,jthi
            DO bi = itlo,ithi
              do k = 1, Nr
                tmpCumSumTile(k,bi,bj) = 0. _d 0
              enddo
              tmpNumTile(bi,bj) = 0. _d 0
            ENDDO
          ENDDO

c-- Temporal mask
          il = ilnblnk(gencost_mask(kgen))
          write(fname0(1:128),'(2A)')
     &      gencost_mask(kgen)(1:il),'T'
          inquire( file=fname0(1:il+1), exist=exst )

          if ( (.NOT.exst).OR.(gencost_mask(kgen).EQ.' ')
     &          ) then

           write(msgBuf,'(3A)') '**Warning: temporal msk file: ',
     &       fname0(1:il+1), ' not found, using 1/nrecloc'
           CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &         SQUEEZE_RIGHT, myThid )

           gencost_mskTemporal=nrecloc
           gencost_mskTemporal=1. _d 0 / gencost_mskTemporal
          else

           write(msgBuf,'(2A)') 'Using temporal msk from file: ',
     &          fname0(1:il+1)
           CALL PRINT_MESSAGE(msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid )

           ioUnit = 0
           CALL MDS_READVEC_LOC( fname0, cost_iprec, ioUnit, 'RL',
     &                      1, tmpVar, dummyRS, 0, 0, irec, myThid )
           gencost_mskTemporal = tmpVar(1)
          endif

c=============== PART 2: Cost Computation ===================
c-- Compute cost only if nonzero temporal mask
          if ( gencost_mskTemporal .ne. 0 ) then

          if ( myProcId .EQ. 0 ) num_gencost(1,1,kgen)=
     &      num_gencost(1,1,kgen)+gencost_mskTemporal

c=============== PART 2.1: Cumulative sum ===================
c Take cumulative sum of my bar from bottom up
c i.e. compute the streamfunction (assuming mybar = trvol)
c
          DO bj = jtlo,jthi
            DO bi = itlo,ithi
              do k = Nr, 1, -1
                do j = jmin,jmax
                  do i = imin,imax
                    tmpCumSumTile(k,bi,bj)=tmpCumSumTile(k,bi,bj) -
     &                mybar(i,j,k,bi,bj)*gencost_mskTemporal

                    tmpNumTile(bi,bj)=
     &                tmpNumTile(bi,bj)+1. _d 0
                  enddo
                enddo
               if( k .lt. Nr ) then
                tmpCumSumTile(k,bi,bj)=tmpCumSumTile(k,bi,bj) +
     &            tmpCumSumTile(k+1,bi,bj)
               endif
              enddo
            ENDDO
          ENDDO

c-- Compute global sum at each level
          do k = Nr, 1, -1
            DO bj = jtlo,jthi
              DO bi = itlo,ithi
                tmpTile(bi,bj) = tmpCumSumTile(k,bi,bj)
              ENDDO
            ENDDO
            tmpCumSumGlo(k) = 0. _d 0
            CALL GLOBAL_SUM_TILE_RL(tmpTile, tmpCumSumGlo(k), myThid )
          enddo

c=============== PART 2.2: Get max val ===================
c-- Find maximum in global cumulative sum

          myTempMax = tmpCumSumGlo(1)
          kmax = 1

          do k = 2, Nr
CADJ STORE myTempMax = tapelev_moc, key = k
            if( myTempMax < tmpCumSumGlo(k) ) then
              myTempMax = tmpCumSumGlo(k)
              kmax = k
            endif
          enddo

          WRITE(msgBuf,'(2A,I3,A,1PE21.14,A,I2)')
     &        'moc cost ',gencost_barfile(kgen)(1:il),
     &        irec,' ', myTempMax, 'kmax: ',kmax
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )

c=============== PART 2.3: Save to obj ===================
c-- Add MOC contribution to actual objective function variable
c   Note: using global values, so only want to store in
c         one processors obj function value

          if ( myProcId .EQ. 0 ) objf_gencost(1,1,kgen)=
     &      objf_gencost(1,1,kgen)+myTempMax

c============= Done with cost computation =====================

          else ! mskTemporal == 0

            WRITE(msgBuf,'(A,I3,A,I3)')
     &          'gencost_mskTemporal = 0, irec: ',irec, ' / ',
     &          nrecloc
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                          SQUEEZE_RIGHT, myThid )

          endif ! mskTemporal /=0
          enddo ! irec=1->nrecloc

c-- Print out what actually is used as cost function
          WRITE(msgBuf,'(A,1PE21.14)') 'moc fc: ',
     &          objf_gencost(1,1,kgen)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )

        endif ! gencost_name(kgen)=moc
      enddo ! kgen=1->NGENCOST

#endif /* ALLOW_GENCOST_CONTRIBUTION */

      return
      end
