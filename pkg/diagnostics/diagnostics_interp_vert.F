C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_interp_vert.F,v 1.12 2011/06/12 19:16:09 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_INTERP_VERT

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_INTERP_VERT(
     I                        listId, md, ndId, ip, im, lm,
     U                        qtmp1,
     O                        qtmp2,
     I                        undefRL,
     I                        myTime, myIter, myThid )

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

      INTEGER NrMax
      PARAMETER( NrMax = numLevels )


C     !INPUT PARAMETERS:
C     listId  :: Diagnostics list number being written
C     md      :: field number in the list "listId".
C     ndId    :: diagnostics  Id number (in available diagnostics list)
C     ip      :: diagnostics  pointer to storage array
C     im      :: counter-mate pointer to storage array
C     lm      :: index in the averageCycle
C     qtmp1   :: diagnostics field output array
C     qtmp2   :: temp working array (same size as output array)
C     undefRL ::
C     myTime  :: current time of simulation (s)
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER listId, md, ndId, ip, im, lm
      _RL     qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     qtmp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,NrMax,nSx,nSy)
      _RL     undefRL
      _RL     myTime
      INTEGER myIter, myThid
CEOP

C     !FUNCTIONS:
#ifdef ALLOW_FIZHI
      _RL   getcon
      EXTERNAL getcon
#endif

C     !LOCAL VARIABLES:
C     i,j,k :: loop indices
      INTEGER i, j, k
      INTEGER bi, bj
      _RL   qtmpsrf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER kLev
      _RL   qprs (sNx,sNy)
      _RL   qinp (sNx,sNy,NrMax)
      _RL   pkz  (sNx,sNy,NrMax)
      _RL   pksrf(sNx,sNy)
      _RL   pk, pkTop
      _RL   kappa
      INTEGER jpoint1, ipoint1
      INTEGER jpoint2, ipoint2
      LOGICAL pInc
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (fflags(listId)(2:2).EQ.'P') THEN
        pkTop = 0. _d 0
        kappa = atm_kappa
#ifdef ALLOW_FIZHI
        IF ( useFIZHI ) kappa = getcon('KAPPA')
#endif

C--   If nonlinear free surf is active, need averaged pressures
        IF (select_rStar.GT.0) THEN
          CALL DIAGNOSTICS_GET_POINTERS( 'RSURF   ', listId,
     &                                   jpoint1, ipoint1, myThid )
C-    IF fizhi is being  used, may need to get physics grid pressures
          IF ( useFIZHI .AND.
     &          gdiag(ndId)(10:10) .EQ. 'L') THEN
           CALL DIAGNOSTICS_GET_POINTERS('FIZPRES ', listId,
     &                                   jpoint2, ipoint2, myThid )
          ELSE
           CALL DIAGNOSTICS_GET_POINTERS('RCENTER ', listId,
     &                                   jpoint2, ipoint2, myThid )
          ENDIF
          IF ( ipoint1.EQ.0 .OR. ipoint2.EQ.0 ) THEN
            WRITE(msgBuf,'(2A,I6,2A)') 'DIAGNOSTICS_INTERP_VERT: ',
     &      'fails to interpolate diag.(#', ndId,'): ',flds(md,listId)
            CALL PRINT_ERROR( msgBuf , myThid )
            STOP 'ABNORMAL END: S/R DIAGNOSTICS_INTERP_VERT'
          ENDIF
C-    averageCycle: move pointer
          ipoint1 = ipoint1 + kdiag(jpoint1)*(lm-1)
          ipoint2 = ipoint2 + kdiag(jpoint2)*(lm-1)

          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
             CALL DIAGNOSTICS_GET_DIAG( 1, undefRL,
     O                     qtmpsrf(1-OLx,1-OLy,bi,bj),
     I                     jpoint1,0,ipoint1,0, bi,bj,myThid )
c            WRITE(0,*) 'rSurf:',bi,bj,qtmpsrf(15,15,bi,bj)
             CALL DIAGNOSTICS_GET_DIAG( 0, undefRL,
     O                     qtmp2(1-OLx,1-OLy,1,bi,bj),
     I                     jpoint2,0,ipoint2,0, bi,bj,myThid )
           ENDDO
          ENDDO

        ELSE
C-    If nonlinear free surf is off, get pressures from rC and rF arrays

          DO bj = myByLo(myThid), myByHi(myThid)
           DO bi = myBxLo(myThid), myBxHi(myThid)
            DO j = 1-OLy,sNy+OLy
             DO i = 1-OLx,sNx+OLx
               qtmpsrf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
             ENDDO
            ENDDO
            DO k = 1,kdiag(ndId)
             DO j = 1-OLy,sNy+OLy
              DO i = 1-OLx,sNx+OLx
               qtmp2(i,j,k,bi,bj) = rC(k)
              ENDDO
             ENDDO
            ENDDO
           ENDDO
          ENDDO

C-    end if nonlinear/linear free-surf
        ENDIF

C--   start loops on tile indices bi,bj:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
C-    Load p to the kappa into a temporary array
           DO j = 1,sNy
            DO i = 1,sNx
              pksrf(i,j)    = qtmpsrf(i,j,bi,bj)**kappa
            ENDDO
           ENDDO
           IF ( useFIZHI.AND.gdiag(ndId)(10:10).EQ.'L') THEN
            pInc = .TRUE.
            DO k = 1,kdiag(ndId)
             DO j = 1,sNy
              DO i = 1,sNx
                qinp(i,j,k) = qtmp1(i,j,k,bi,bj)
                pkz(i,j,k)  = qtmp2(i,j,k,bi,bj)**kappa
              ENDDO
             ENDDO
            ENDDO
           ELSE
            DO k = 1,kdiag(ndId)
             pInc = .TRUE.
             kLev = kdiag(ndId)-k+1
c            pInc = .FALSE.
c            kLev = k
             DO j = 1,sNy
              DO i = 1,sNx
                IF (maskC(i,j,kLev,bi,bj).NE.0.) THEN
                 qinp(i,j,k)= qtmp1(i,j,kLev,bi,bj)
                ELSE
                 qinp(i,j,k)= undefRL
                ENDIF
                pkz(i,j,k)  = qtmp2(i,j,kLev,bi,bj)**kappa
              ENDDO
             ENDDO
            ENDDO
           ENDIF

C-    Interpolate, level per level, and put interpolated field in qprs:
           DO k = 1,nlevels(listId)
             pk = levs(k,listId)**kappa
             CALL DIAGNOSTICS_INTERP_P2P(
     O                        qprs,
     I                        qinp,pkz,pksrf,pkTop,pk,
     I                        undefRL,pInc,sNx*sNy,kdiag(ndId),myThid )
C-    Transfert qprs to qtmp1:
             DO j = 1,sNy
              DO i = 1,sNx
               IF (qprs(i,j).EQ.undefRL) THEN
                 qtmp1(i,j,k,bi,bj) = 0.
               ELSE
                 qtmp1(i,j,k,bi,bj) =  qprs(i,j)
               ENDIF
              ENDDO
             ENDDO
           ENDDO

C-   end bi,bj loops
         ENDDO
        ENDDO

      ENDIF

      RETURN
      END
