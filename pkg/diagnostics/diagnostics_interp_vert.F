C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_interp_vert.F,v 1.5 2005/11/18 20:25:18 molod Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INTERP_VERT

C     !INTERFACE:
      SUBROUTINE  DIAGNOSTICS_INTERP_VERT(
     I     listId, md, ndId, ip, im,
     U     nlevsout,
     U     qtmp1,
     I     undef,
     I     myTime, myIter, myThid )

C     !DESCRIPTION:
C     Interpolate vertically a diagnostics field before writing to file.
C       presently implemented (for Atmospheric fields only): 
C         Interpolation (linear in p^kappa) to standard pressure levels
C                              

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
      INTEGER Nrphys
      PARAMETER (Nrphys=0)
#endif


C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     md      :: field number in the list "listId".
C     ndId    :: diagnostics  Id number (in available diagnostics list)
C     ip      :: diagnostics  pointer to storage array
C     im      :: counter-mate pointer to storage array
C     nlevsout:: number of levels
C     qtmp1   :: diagnostics field output array
C     undef   ::
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER listId, md, ndId, ip, im
      INTEGER nlevsout
      _RL     qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)
      _RL     undef
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
      INTEGER i, j, k
      INTEGER bi, bj
      _RL qtmpsrf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL qtmp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)
      _RL getcon
      EXTERNAL getcon
      integer nplevs1, nplevs2, nplevs3
      parameter(nplevs1 = 16)
      parameter(nplevs2 = 16)
      parameter(nplevs3 = 17)
      _RL plevs1(nplevs1)
      data plevs1/ 1000.0 _d 2, 925.0 _d 2, 850.0 _d 2, 700.0 _d 2,
     .              600.0 _d 2, 500.0 _d 2, 400.0 _d 2, 300.0 _d 2,
     .              250.0 _d 2, 200.0 _d 2, 150.0 _d 2, 100.0 _d 2,
     .               70.0 _d 2,  50.0 _d 2,  30.0 _d 2,  20.0 _d 2/
      _RL plevs2(nplevs2)
      data plevs2/ 1000.0 _d 2, 950.0 _d 2, 900.0 _d 2, 850.0 _d 2,
     .              800.0 _d 2, 750.0 _d 2, 700.0 _d 2, 600.0 _d 2,
     .              500.0 _d 2, 400.0 _d 2, 300.0 _d 2, 250.0 _d 2,
     .              200.0 _d 2, 150.0 _d 2, 100.0 _d 2,  50.0 _d 2/
      _RL plevs3(nplevs3)
      data plevs3/ 1000.0 _d 2, 925.0 _d 2, 850.0 _d 2, 700.0 _d 2,
     .              600.0 _d 2, 500.0 _d 2, 400.0 _d 2, 300.0 _d 2,
     .              250.0 _d 2, 200.0 _d 2, 150.0 _d 2, 100.0 _d 2,
     .               70.0 _d 2,  50.0 _d 2,  30.0 _d 2,  20.0 _d 2,
     .               10.0 _d 2/
C Use the biggest of nplevs 1-3 (or any others) for the size of qprs
      _RL qprs(sNx,sNy,nplevs3)
      _RL qinp(sNx,sNy,Nr+Nrphys)
      _RL pkz(sNx,sNy,Nr+Nrphys)
      _RL pksrf(sNx,sNy)
      _RL p
      _RL kappa
      _RL oneRL
#ifdef NONLIN_FRSURF
      INTEGER jpoint1,ipoint1
      INTEGER jpoint2,ipoint2
      logical foundp
      data foundp /.false./
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

        IF(fflags(listId)(2:2).eq.'P') then
          kappa = getcon('KAPPA')
          oneRL = 1. _d 0

c If nonlinear free surf is active, need averaged pressures
#ifdef NONLIN_FRSURF
          if(select_rStar.GT.0)then
           call diagnostics_get_pointers('RSURF   ',ipoint1,jpoint1,
     .                                                           myThid)
           call diagnostics_get_pointers('PRESSURE',ipoint2,jpoint2,
     .                                                           myThid)
C if fizhi is being  used, may need to get physics grid pressures
#ifdef ALLOW_FIZHI
           if(gdiag(ndId)(10:10) .EQ. 'L')then
           call diagnostics_get_pointers('FIZPRES ',ipoint2,jpoint2,
     .                                                           myThid)
           endif
#endif
           if( jpoint1.ne.0 .and. jpoint2.ne.0) then
            foundp = .true.
           else
            foundp = .false.
           endif

           if(.not. foundp) then
            WRITE(msgBuf,'(4A)') 'DIAGNOSTICS_INTERP_VERT: ',
     .    ' Have asked for pressure interpolation but have not ',
     .    ' Activated surface and 3D pressure diagnostic, ',
     .    ' RSURF and PRESSURE'
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_INTERP_VERT'
           endif

           DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
             call getdiag(oneRL,undef,qtmpsrf(1-OLx,1-OLy,bi,bj),
     .                       jpoint1,0,ipoint1,0,bi,bj,myThid)
            ENDDO
           ENDDO
           DO bj = myByLo(myThid), myByHi(myThid)
            DO bi = myBxLo(myThid), myBxHi(myThid)
             DO k = 1,nlevels(listId)
              call getdiag(levs(k,listId),undef,
     .          qtmp2(1-OLx,1-OLy,k,bi,bj),jpoint2,0,ipoint2,0,
     .          bi,bj,myThid)
             ENDDO
            ENDDO
           ENDDO
          endif
#else 
C If nonlinear free surf is off, get pressures from rC and rF arrays
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              qtmpsrf(i,j,bi,bj) = rF(1)
             ENDDO
            ENDDO
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
              DO k = 1,nlevels(listId)
               qtmp2(i,j,k,bi,bj) = rC(k)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
          ENDDO
#endif
C Load p to the kappa into a temporary array
          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1,sNy
             DO i = 1,sNx
              pksrf(i,j) = qtmpsrf(i,j,bi,bj) ** kappa
              DO k = 1,nlevels(listId)
               if(gdiag(ndId)(10:10).eq.'R') then
                if(hFacC(i,j,nlevels(listId)-k+1,bi,bj).ne.0.) then
                 qinp(i,j,k) =  qtmp1(i,j,nlevels(listId)-k+1,bi,bj)
                else
                 qinp(i,j,k) =  undef
                endif
                pkz(i,j,k) = qtmp2(i,j,nlevels(listId)-k+1,bi,bj)**kappa
               elseif(gdiag(ndId)(10:10).eq.'L') then
                qinp(i,j,k) =  qtmp1(i,j,k,bi,bj)
                pkz(i,j,k) = qtmp2(i,j,k,bi,bj)**kappa
               endif
              ENDDO
             ENDDO
            ENDDO

            if(fflags(listId)(3:3).eq.'1') then
             nlevsout = nplevs1
             DO k = 1,nplevs1
              p = plevs1(k)
              call prestopres(qprs(1,1,k),qinp,pkz,pksrf,0.,p,sNx,sNy,
     .                                     nlevels(listId),myThid )
             ENDDO
            elseif(fflags(listId)(3:3).eq.'2')then
             nlevsout = nplevs2
             DO k = 1,nplevs2
              p = plevs2(k)
              call prestopres(qprs(1,1,k),qinp,pkz,pksrf,0.,p,sNx,sNy,
     .                                     nlevels(listId),myThid )
             ENDDO
            elseif(fflags(listId)(3:3).eq.'3')then
             nlevsout = nplevs3
             DO k = 1,nplevs3
              p = plevs3(k)
              call prestopres(qprs(1,1,k),qinp,pkz,pksrf,0.,p,sNx,sNy,
     .                                     nlevels(listId),myThid )
             ENDDO
            endif

            DO j = 1,sNy
             DO i = 1,sNx
              DO k = 1,nlevsout
               qtmp1(i,j,k,bi,bj) =  qprs(i,j,k)
               if(qtmp1(i,j,k,bi,bj).eq.undef) qtmp1(i,j,k,bi,bj) = 0.
              ENDDO
             ENDDO
            ENDDO

           ENDDO
          ENDDO

        ENDIF

      RETURN
      END
