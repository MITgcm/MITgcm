#include "PACKAGES_CONFIG.h"
#include "CPP_EEOPTIONS.h"
#undef DBUG_EXCH_VEC

C--   Contents
C--   o EXCH2_RECV_GET_VEC_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C !ROUTINE: EXCH2_RECV_GET_VEC_RL

C !INTERFACE:
      SUBROUTINE EXCH2_RECV_GET_VEC_RL(
     U                        array,
     I                        theHandle,
     I                        myd1, myThid )
C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH2_RECV_GET_VEC_RL
C     | o "Receive" edges for RL array.
C     *==========================================================*
C     | Routine that invokes actual message passing receive
C     | of data to update buffer
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_SIZE.h"
#include "W2_EXCH2_TOPOLOGY.h"
#endif
#include "FLT_SIZE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     arrayE        :: buffer array to collect Eastern Neighbour values
C     arrayW        :: buffer array to collect Western Neighbour values
C     myd1          :: size
C     myThid        :: my Thread Id. number
      INTEGER myd1
      _RL array(myd1, nSx, nSy, 4)
#ifdef ALLOW_EXCH2
      INTEGER theHandle(2,W2_maxNeighbours,nSx,nSy)
#else
      INTEGER theHandle
#endif
      INTEGER myThid
CEOP

#ifdef ALLOW_EXCH2
#ifdef ALLOW_USE_MPI
C     !LOCAL VARIABLES:
C     bi, bj        :: tile indices
C     theProc       :: Variables used in message building
C     theTag        :: Variables used in message building
C     theType       :: Variables used in message building
C     theSize       :: Variables used in message building
      INTEGER bi, bj
      INTEGER ioUnit
      INTEGER thisTile, nN, tgT, oNb, dir
      INTEGER theProc, theTag, theType, theSize
      INTEGER wHandle
      INTEGER mpiStatus(MPI_STATUS_SIZE,4), mpiRc
      INTEGER imax, imax2
      PARAMETER(imax=9)
      PARAMETER(imax2=imax*max_npart_exch)
      INTEGER pi(2), pj(2), oi, oj, ip, itb, jtb, isb, jsb
      _RL itc,jtc, itmp, jtmp

C--   Under a "put" scenario we
C--     i. set completetion signal for buffer we put into.
C--    ii. wait for completetion signal indicating data has been put in
C--        our buffer.
C--   Under a messaging mode we "receive" the message.
C--   Under a "get" scenario <= not implemented, we
C--     i. Check that the data is ready.
C--    ii. Read the data.
C--   iii. Set data read flag + memory sync.

      ioUnit = errorMessageUnit

      _BEGIN_MASTER(myThid)

      DO bj=1,nSy
       DO bi=1,nSx
         thisTile = W2_myTileList(bi,bj)

C- loop over neighboring tiles
         DO nN=1,exch2_nNeighbours(thisTile)

          tgT = exch2_neighbourId(nN, thisTile )
          oNb = exch2_opposingSend(nN, thisTile )
          dir = exch2_neighbourDir(nN,thisTile)

#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I6)') 'RECV,0 :',myProcId,bi,bj
#endif
          IF ( W2_myCommFlag(nN,bi,bj) .EQ. 'M' ) THEN
           theProc = W2_tileProc(tgT) - 1
           theTag  = (tgT-1)*W2_maxNeighbours + oNb
           theSize = myd1
           theType = _MPI_TYPE_RL
#ifdef DBUG_EXCH_VEC
           write(ioUnit,'(A,5I5,I8)') 'qq2xW: ',myProcId,bi,bj,
     &           theProc,theTag,theSize
#endif
           CALL MPI_Recv( array(1,bi,bj,dir), theSize, theType,
     &                    theProc, theTag, MPI_COMM_MODEL,
     &                    mpiStatus, mpiRc )

C- apply exch2_pij to rotate or shift indices
           itb = exch2_tBasex(tgT)
           jtb = exch2_tBasey(tgT)
           isb = exch2_tBasex(thisTile)
           jsb = exch2_tBasey(thisTile)
           pi(1)=exch2_pij(1,nN,thisTile)
           pi(2)=exch2_pij(2,nN,thisTile)
           pj(1)=exch2_pij(3,nN,thisTile)
           pj(2)=exch2_pij(4,nN,thisTile)
           oi  = exch2_oi(nN,thisTile)
           oj  = exch2_oj(nN,thisTile)

#ifdef DBUG_EXCH_VEC
           DO ip=1,max_npart_exch
            IF (array(imax*(ip-1)+1,bi,bj,dir).NE.0.) THEN
             itc=array(imax*(ip-1)+3,bi,bj,dir)+itb
             jtc=array(imax*(ip-1)+4,bi,bj,dir)+jtb
             itmp = pi(1)*itc+pi(2)*jtc+oi-isb
             jtmp = pj(1)*itc+pj(2)*jtc+oj-jsb          
             write(ioUnit,'(A,3I6)') 'LOC,1 :',thisTile,tgT,dir
             write(ioUnit,'(A,8F10.3)') 'LOC,2 :',
     &       array(imax*(ip-1)+1,bi,bj,dir),
     &       array(imax*(ip-1)+2,bi,bj,dir),
     &       array(imax*(ip-1)+3,bi,bj,dir),
     &       array(imax*(ip-1)+4,bi,bj,dir),
     &       itc,jtc,itmp,jtmp
            ENDIF
           ENDDO
#endif

           DO ip=1,max_npart_exch
            IF (array(imax*(ip-1)+1,bi,bj,dir).NE.0.) THEN
             itc=array(imax*(ip-1)+3,bi,bj,dir)+itb
             jtc=array(imax*(ip-1)+4,bi,bj,dir)+jtb
             array(imax*(ip-1)+3,bi,bj,dir) = pi(1)*itc+pi(2)*jtc+oi-isb
             array(imax*(ip-1)+4,bi,bj,dir) = pj(1)*itc+pj(2)*jtc+oj-jsb
            ENDIF
           ENDDO

          ENDIF
#ifdef DBUG_EXCH_VEC
          write(ioUnit,'(A,5I6)') 'RECV,1 :',myProcId,bi,bj
#endif
C-       nN
         ENDDO
C-     bj,bi
       ENDDO
      ENDDO
#ifdef DBUG_EXCH_VEC
       write(ioUnit,'(A,5I6,I12)') 'RECV:',myProcId
#endif

C--   Clear message handles/locks
      DO bj=1,nSy
       DO bi=1,nSx
         thisTile = W2_myTileList(bi,bj)
         DO nN=1,exch2_nNeighbours(thisTile)
c          tgT = exch2_neighbourId(nN, thisTile )

C-    Note: In a between process tile-tile data transport using
C           MPI the sender needs to clear an Isend wait handle here.
C           In a within process tile-tile data transport using true
C           shared address space/or direct transfer through commonly
C           addressable memory blocks the receiver needs to assert
C           that he has consumed the buffer the sender filled here.
           IF ( W2_myCommFlag(nN,bi,bj) .EQ. 'M' ) THEN
            wHandle = theHandle(1,nN,bi,bj)
            CALL MPI_Wait( wHandle, mpiStatus, mpiRc )
           ENDIF

         ENDDO
       ENDDO
      ENDDO

      _END_MASTER(myThid)

C--   need to sync threads after master has received data
      _BARRIER

#endif /* ALLOW_USE_MPI */
#endif /* ALLOW_EXCH2 */

      RETURN
      END
