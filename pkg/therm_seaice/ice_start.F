#include "CPP_OPTIONS.h"

C     !ROUTINE: ICE_START
C     !INTERFACE:
      SUBROUTINE ICE_START(i,j, bi, bj, myThid, 
     &                     esurp,fresh,fsalt,Tf, compact )
C     *==========================================================*
C     | S/R  ICE_START                                              
C     | o Check water temperature and start ice layer if
C     | appropriate.                                            
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
cswdice -- add ---
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#include "ICE_DIAGS.h"
#include "BULKF_ICE_CONSTANTS.h"
#endif
cswdice -- end add ---

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj
      INTEGER myThid
      _RL esurp ! energy available for freezing
      _RL fresh ! fresh water flux
      _RL fsalt ! salt water flux
      _RL Tf    ! freezing temperature
      _RL compact ! fraction of grid area covered in ice

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL qi0av, hi0new

#ifdef ALLOW_THERM_SEAICE

C     start ice
            compact=0.d0
            qi0av=-rhoi*Lfresh 
            hi0new=-esurp*deltaTtracer/qi0av
c redistribute ice over portion of cell if too thin
            if (hi0new.lt.hicemin) then
             compact=hi0new/hicemin 
             iceHeight(i,j,bi,bj)=hicemin
            else
             compact=1.d0
             iceHeight(i,j,bi,bj)=hi0new
            endif
c           theta(i,j,1,bi,bj)=Tf
            Tsrf(i,j,bi,bj)=Tf    
            Tice1(i,j,bi,bj)=Tf
            Tice2(i,j,bi,bj)=Tf
cQQ         Qice1(i,j,bi,bj)=qi0av
cQQ         Qice2(i,j,bi,bj)=qi0av
            Qice1(i,j,bi,bj) = -cpwater*Tmlt1 + 
     &              cpice *(Tmlt1-Tice1(i,j,bi,bj)) +
     $              Lfresh*(1.-Tmlt1/Tice1(i,j,bi,bj))
            Qice2(i,j,bi,bj) = -cpice *Tice2(i,j,bi,bj) + Lfresh
            fresh=-(rhoi*iceheight(i,j,bi,bj))/deltaTtracer
cQQ         fsalt=-(rhoi*iceheight(i,j,bi,bj)*saltice )
cQQ  &             *0.001/deltaTtracer
            fsalt=-(rhoi*iceheight(i,j,bi,bj)*saltice )
     &             /35/deltaTtracer
            iceMask(i,j,bi,bj)=compact
cBB
cBB         print*,'**** ice start **** i,j', i,j
cBB         print*,'qi1, qi2',Qice1(i,j,bi,bj), Qice2(i,j,bi,bj), Qi0av
cBB         print*,'hi, fresh',hi0new, fresh
cBB         print*,'*******************'
cBB         print*,'             '
#endif 

      RETURN
      END
