#include "CPP_OPTIONS.h"

C     !ROUTINE: ICE_EXTRASTART
C     !INTERFACE:
      SUBROUTINE ICE_EXTRASTART(i,j, bi, bj, myThid, qleft,
     &                     esurp,fresh,fsalt,Tf,Ta, compact )
C     *==========================================================*
C     | S/R  ICE_EXTRASTART                                              
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
      _RL Ta    ! air temperature
      _RL compact ! fraction of grid area covered in ice
      _RL qleft

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL  qi0av, hi0new,compact2, hice_old
      _RL  qicen (2)  ! ice enthalpy (J m-3)
      _RL  hlyr          ! individual ice layer thickness (m)
      _RL  hnew(2)    ! new ice layer thickness (m)
      _RL  hi            ! ice height
      _RL  hs            ! snow height

#ifdef ALLOW_THERM_SEAICE

        hice_old=iceHeight(i,j,bi,bj)
        hi=iceHeight(i,j,bi,bj)
        hs=snowHeight(i,j,bi,bj)
        qicen(1)=Qice1(i,j,bi,bj)
        qicen(2)=Qice2(i,j,bi,bj)
        hnew(1)=0.d0
        hnew(2)=0.d0
C  start ice
        qi0av=-rhoi*Lfresh 
        hi0new=-esurp*deltaTtracer/qi0av
c redistribute ice over portion of cell if too thin
        if (hi0new.lt.hicemin) then
         compact2=(1.d0-compact)*hi0new/hicemin 
         hi0new=hicemin
        else
         compact2=(1.d0-compact)
        endif
        if ((compact+compact2).gt.1.d0) then
          print*,'QQQQQQQQQ ice mask > 1, ice_extrastart'
          stop
        endif
        hi=(compact2*hi0new+compact*hi)/
     &                       (compact+compact2)  
        hs=(compact2*0.d0+compact*hs)/
     &                       (compact+compact2) 
        hlyr=hi/2
        hnew(2)=hi-hice_old
        call new_layers_winton(hs,hi,hlyr,hnew,qicen)
        fresh=(compact2*(-(rhoi*hi0new)/deltaTtracer)+
     &         compact*fresh)/(compact+compact2)
        fsalt=(compact2*(-(rhoi*hi0new*saltice )
     &         /35.d0/deltaTtracer) + compact*fsalt)/
     &                 (compact+compact2)
        qleft=(-compact2*esurp+compact*qleft)/(compact+compact2)
        iceMask(i,j,bi,bj)=compact+compact2
        iceHeight(i,j,bi,bj)=hi
        snowHeight(i,j,bi,bj)=hs
        Qice1(i,j,bi,bj)=qicen(1)
        Qice2(i,j,bi,bj)=qicen(2)
cBB
cBB         print*,'**** ice start **** i,j', i,j
cBB         print*,'qi1, qi2',Qice1(i,j,bi,bj), Qice2(i,j,bi,bj), Qi0av
cBB         print*,'hi, fresh',hi0new, fresh
cBB         print*,'*******************'
cBB         print*,'             '
#endif 

      RETURN
      END
