C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diag_vegtile_fill.F,v 1.12 2010/01/03 00:42:45 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

CBOP
C     !ROUTINE: DIAG_VEGTILE_FILL
C     !INTERFACE:
      SUBROUTINE DIAG_VEGTILE_FILL(
     &                field,indx,chfr,ib,numpts,npeice,
     &                check, chardiag, kLev, nLevs, bi, bj, myThid )
C     !DESCRIPTION:
C***********************************************************************
C Increment the diagnostics array with a vegetation tile space field
C***********************************************************************
C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
C***********************************************************************
C   field    :: array to be mapped to grid space [ib,levs] and added to qdiag
C   indx     :: array of horizontal indices of grid points to convert to
C               tile space[numpts]
C   chfr     :: fractional area covered by the tile [ib]
C   ib       :: inner dimension of source array and number of points in
C               array a that need to be pasted
C   numpts   :: total number of points which were stripped
C   npeice   :: the current strip number to be filled
C   check    :: logical to check for undefined values
C   chardiag :: Character expression for diag to fill
C   kLev     :: Integer flag for vertical levels:
C                > 0 (any integer): which single level to increment
C                0,-1 to increment "nLevs" levels in qdiag:
C                  0 : fill-in in the same order as the input array
C                 -1 : fill-in in reverse order.
C   nLevs    :: indicates Number of levels of the input field array
C   bi       :: X-direction tile number
C   bj       :: Y-direction tile number
C   myThid   :: my thread Id number
C***********************************************************************
      CHARACTER*8 chardiag
      INTEGER kLev, nLevs, bi, bj
      INTEGER myThid
      INTEGER ib,numpts,npeice
      INTEGER indx(numpts)
      _RL field(ib,nlevs), chfr(ib)
      LOGICAL check
CEOP

C     !LOCAL VARIABLES:
C ===============
      INTEGER m, n
      INTEGER ndiagnum, ipointer
      INTEGER k, kFirst, kLast
      INTEGER kd, kd0, ksgn, kStore
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER offset, Lena
      INTEGER ivt, ij, i
      _RL undef
      INTEGER iSp, ndId, j,l
      INTEGER region2fill(0:nRegions)
      _RL     scaleFact
      _RL     gridField(sNx*sNy,nlevs), gridFrac(sNx*sNy)
#ifndef REAL4_IS_SLOW
      _RS     dummyRS(1)
#endif

#ifdef ALLOW_FIZHI
      _RL   getcon
      EXTERNAL getcon
#endif

C Run through list of active diagnostics to make sure
C we are trying to fill a valid diagnostic

      undef = UNSET_RL
#ifdef ALLOW_FIZHI
      IF ( check ) undef = getcon('UNDEF')
#endif
      ndiagnum = 0
      ipointer = 0
      DO n=1,nlists
       DO m=1,nActive(n)
        IF ( chardiag.EQ.flds(m,n) .AND. idiag(m,n).GT.0 ) THEN
         ndiagnum = jdiag(m,n)
         ipointer = idiag(m,n)
         IF ( ndiagnum.NE.0 .AND. ndiag(ipointer,1,1).GE.0 ) THEN
C--   do the filling: start here:

          IF ( (ABS(kLev).LE.1) .AND. (npeice.EQ.1) ) THEN
C Increment the counter for the diagnostic
            ndiag(ipointer,bi,bj) = ndiag(ipointer,bi,bj) + 1
          ENDIF

          offset = ib*(npeice-1)
          Lena   = MIN(ib,numpts-offset)

C-      Which part of field to add : k = 3rd index,
C         and do the loop >> do k=kFirst,kLast <<
          IF (kLev.LE.0) THEN
           kFirst = 1
           kLast  = nLevs
          ELSEIF ( nLevs.EQ.1 ) THEN
           kFirst = 1
           kLast  = 1
          ELSEIF ( kLev.LE.nLevs ) THEN
           kFirst = kLev
           kLast  = kLev
          ELSE
           STOP 'ABNORMAL END: S/R DIAGNOSTICS_FILL kLev > nLevs > 0'
          ENDIF
C-      Which part of qdiag to update: kd = 3rd index,
C         and do the loop >> do k=kFirst,kLast ; kd = kd0 + k*ksgn <<
          IF ( kLev.EQ.-1 ) THEN
           ksgn = -1
           kd0 = ipointer + nLevs
          ELSEIF ( kLev.EQ.0 ) THEN
           ksgn = 1
           kd0 = ipointer - 1
          ELSE
           ksgn = 0
           kd0 = ipointer + kLev - 1
          ENDIF

C-      Check for consistency with Nb of levels reserved in storage array
          kStore = kd0 + MAX(ksgn*kFirst,ksgn*kLast) - ipointer + 1
          IF ( kStore.GT.kdiag(ndiagnum) ) THEN
           _BEGIN_MASTER(myThid)
           WRITE(msgBuf,'(2A,I4,A)') 'DIAGNOSTICS_FILL: ',
     &      'exceed Nb of levels(=',kdiag(ndiagnum),' ) reserved '
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(2A,I6,2A)') 'DIAGNOSTICS_FILL: ',
     &      'for Diagnostics #', ndiagnum, ' : ', chardiag
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(2A,2I4,I3)') 'calling DIAGNOSTICS_FILL ',
     &      'with kLev,nLevs=', kLev,nLevs
           CALL PRINT_ERROR( msgBuf , myThid )
           WRITE(msgBuf,'(2A,I6,A)') 'DIAGNOSTICS_FILL: ',
     &      '==> trying to store up to ', kStore, ' levels'
           CALL PRINT_ERROR( msgBuf , myThid )
           STOP 'ABNORMAL END: S/R DIAGNOSTICS_FILL'
           _END_MASTER(myThid)
          ENDIF

          DO k = kFirst,kLast
           kd = kd0 + ksgn*k
           IF ( check ) THEN
            DO ivt = 1,Lena
             ij = indx(ivt+offset) - 1
             j = 1 + INT(ij/sNx)
             i = 1 + MOD(ij,sNx)
             IF ( field(ivt,k).EQ.undef ) THEN
              qdiag(i,j,kd,bi,bj) = undef
             ELSEIF ( qdiag(i,j,kd,bi,bj).NE.undef ) THEN
              qdiag(i,j,kd,bi,bj) = qdiag(i,j,kd,bi,bj)
     &                            + field(ivt,k)*chfr(ivt)
             ENDIF
            ENDDO
           ELSE
            DO ivt = 1,Lena
              ij = indx(ivt+offset) - 1
              j = 1 + INT(ij/sNx)
              i = 1 + MOD(ij,sNx)
              qdiag(i,j,kd,bi,bj) = qdiag(i,j,kd,bi,bj)
     &                            + field(ivt,k)*chfr(ivt)
            ENDDO
           ENDIF
          ENDDO

C--   do the filling: ends here.
         ENDIF
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Global/Regional Statistics :
      scaleFact = 1. _d 0

C Run through list of active statistics-diagnostics to make sure
C we are trying to compute & fill a valid diagnostic

      DO n=1,diagSt_nbLists
       DO m=1,diagSt_nbActv(n)
        IF ( chardiag.EQ.diagSt_Flds(m,n) .AND. iSdiag(m,n).GT.0 ) THEN
         iSp = iSdiag(m,n)
         IF ( qSdiag(0,0,iSp,bi,bj).GE.0. ) THEN
           ndId = jSdiag(m,n)
C-    Find list of regions to fill:
           DO j=0,nRegions
            region2fill(j) = diagSt_region(j,n)
           ENDDO
C-    if this diagnostics appears in several lists (with same freq)
C     then add regions from other lists
           DO l=1,diagSt_nbLists
            DO k=1,diagSt_nbActv(l)
             IF ( iSdiag(k,l).EQ.-iSp ) THEN
              DO j=0,nRegions
               region2fill(j) = MAX(region2fill(j),diagSt_region(j,l))
              ENDDO
             ENDIF
            ENDDO
           ENDDO

C-      Which part of field to add : k = 3rd index,
C         and do the loop >> do k=kFirst,kLast <<
           IF (kLev.LE.0) THEN
            kFirst = 1
            kLast  = nLevs
           ELSE
            kFirst = 1
            kLast  = 1
           ENDIF

C-    Fill local array with grid-space field after conversion.
           offset = ib*(npeice-1)
           Lena    = MIN(ib,numpts-offset)

           DO ij = 1,sNx*sNy
             gridFrac(ij)= 0.
           ENDDO
           DO ivt = 1,Lena
             ij = indx(ivt+offset)
             gridFrac(ij)=gridFrac(ij)+chfr(ivt)
           ENDDO

           DO k = kFirst,kLast
            DO ij = 1,sNx*sNy
             gridField(ij,k)= 0.
            ENDDO
            IF ( check ) THEN
             DO ivt = 1,Lena
              ij = indx(ivt+offset)
              IF ( field(ivt,k).EQ.undef ) THEN
               gridField(ij,k) = undef
              ELSEIF ( gridFrac(ij).GT.0. _d 0 ) THEN
               gridField(ij,k) = gridField(ij,k)
     &                         + field(ivt,k)*chfr(ivt)/gridFrac(ij)
              ENDIF
             ENDDO
            ELSE
             DO ivt = 1,Lena
              ij = indx(ivt+offset)
              IF ( gridFrac(ij).GT.0. _d 0 ) THEN
               gridField(ij,k) = gridField(ij,k)
     &                         + field(ivt,k)*chfr(ivt)/gridFrac(ij)
              ENDIF
             ENDDO
            ENDIF
           ENDDO

C-    diagnostics is valid and Active: Now do the filling
           CALL DIAGSTATS_FILL(
     I              gridField, gridFrac,
#ifndef REAL4_IS_SLOW
     I               dummyRS, dummyRS,
#endif
     I              scaleFact, 1, 0, 1,
     I              ndId, iSp, region2fill, kLev, nLevs,
     I              3, bi, bj, myThid )
         ENDIF
        ENDIF
       ENDDO
      ENDDO

      RETURN
      END
