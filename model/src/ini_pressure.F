C $Header: 
C $Name:  

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PRESSURE
C     !INTERFACE:
      subroutine ini_pressure( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PRESSURE
C     | o initialise the pressure field consistently with 
C     |   temperature and salinity
C     |   - needs to be called after ini_theta, ini_salt, and 
C     |     ini_psurf
C     |   - does not include surface pressure loading, because
C     |     that is only available until after 
C     |     call packages_init_variables
C     *==========================================================*
C     \ev

C     !USES:
      implicit none
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_PRESSURE
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
      INTEGER ncount, iMin, iMax, jMin, jMax, npiter
      _RL phiHyd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL pold(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL rmspp, rmsppold 

      CHARACTER*(MAX_LEN_MBUF) msgBuf

      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy
      
      write(msgBuf,'(a)')
     &     'Start initial hydrostatic pressure computation'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &     SQUEEZE_RIGHT , 1)
C     initialise pressure to water column thickness times a constant
      do bj = myByLo(myThid), myByHi(myThid)
       do bi = myBxLo(myThid), myBxHi(myThid)
        do k = 1,Nr
         do j=1-OLy,sNy+OLy
          do i=1-OLx,sNx+OLx
           phiHyd(i,j,k) = 0. _d 0
          end do
         end do
         call store_pressure(  bi, bj, k, phiHyd, myThid )
        end do
       end do
      end do

      if ( buoyancyRelation .eq. 'OCEANIC' ) then

       rmspp    = 1. _d 0
       rmsppold = 0. _d 0
       npiter = 0

C     iterate pressure p = integral of (g*rho(p)*dz)
       do npiter = 1, 15
        rmsppold = rmspp
        rmspp    = 0. _d 0
        ncount   = 0
        do bj = myByLo(myThid), myByHi(myThid)
         do bi = myBxLo(myThid), myBxHi(myThid)
          do k = 1, Nr
C     for each level save old pressure and compute new pressure 
           do j=jMin,jMax
            do i=iMin,iMax
             pold(i,j,k) = pressure(i,j,k,bi,bj)
            end do
           end do
           call calc_phi_hyd(
     &          bi, bj, iMin, iMax, jMin, jMax, k,
     &          theta, salt, phiHyd, myThid)
           call store_pressure(  bi, bj, k, phiHyd, myThid )
          end do
C     compute convergence criterion
          do k = 1, Nr
           do j=jMin,jMax
            do i=iMin,iMax
             rmspp = rmspp 
     &            + (pressure(i,j,k,bi,bj)-pold(i,j,k))**2
             ncount = ncount + int(maskC(i,j,k,bi,bj))
            end do
           end do
          end do
         end do
        end do
Cml        WRITE(msgBuf,'(I10.10)') npiter
Cml        CALL WRITE_FLD_XYZ_RL( 'POLD.',msgBuf,pold,npiter,myThid)
Cml        CALL WRITE_FLD_XYZ_RL( 'PINI.',msgBuf,pressure,npiter,myThid)
        if ( ncount .eq. 0 ) then
           rmspp = 0. _d 0
        else
           rmspp = sqrt(rmspp/ncount)
        end if
        write(msgBuf,'(a,i2,a,e20.13)')
     &       'Iteration ', npiter, ', RMS-difference = ', rmspp
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &       SQUEEZE_RIGHT , 1)

       end do
C     print some diagnostics	
       if ( rmspp .ne. 0. ) then
        if ( rmspp .eq. rmsppold ) then
         write(msgBuf,'(a)')
     &      'Initial hydrostatic pressure did not converge perfectly,' 
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &        SQUEEZE_RIGHT , 1)
         write(msgBuf,'(a)')
     &      'but the RMS-difference is constant, indicating that the'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &        SQUEEZE_RIGHT , 1)
         write(msgBuf,'(a)')
     &      'algorithm converged within machine precision.'    
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &        SQUEEZE_RIGHT , 1)
        else
         write(msgBuf,'(a,i2,a)')
     &       'Initial hydrostatic pressure did not converge after ', 
     &          npiter-1, ' steps'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PRESSURE'
        end if
       end if
       write(msgBuf,'(a)')
     &     'Initial hydrostatic pressure converged.' 
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &      SQUEEZE_RIGHT , 1)

      else
C     print a message and do nothing
       write(msgBuf,'(a,a)')
     &        'Pressure is predetermined for buoyancyRelation ',
     &        buoyancyRelation(1:11)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &      SQUEEZE_RIGHT , 1)

      end if

      write(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &     SQUEEZE_RIGHT , 1)

      return
      end
