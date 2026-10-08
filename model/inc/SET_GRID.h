!

!BOP
! !ROUTINE: SET_GRID.h
! !INTERFACE:
! #include SET_GRID.h

! !DESCRIPTION:
! Header file holding some 1-D arrays which are used to set-up the model grid.
!==========================================================================
!  IMPORTANT : This header file can only be included after the options
!          file PACKAGES_CONFIG.h and the header file W2_EXCH2_SIZE.h
!==========================================================================

!EOP

!    grid_maxNx :: Maximum length of delX vector
!    grid_maxNy :: Maximum length of delY vector
      INTEGER :: grid_maxNx, grid_maxNy
#ifdef ALLOW_EXCH2
      PARAMETER( grid_maxNx = W2_maxXStackNx )
      PARAMETER( grid_maxNy = W2_maxYStackNy )
#else  /* ALLOW_EXCH2 */
      PARAMETER( grid_maxNx = Nx )
      PARAMETER( grid_maxNy = Ny )
#endif /* ALLOW_EXCH2 */

!--   COMMON /SET_GRID_R/ "Real" valued parameters used to set-up the model grid.
! delX      :: Separation between cell faces (m) or (deg), depending on type
! delY         type of horizontal grid choice (cartesian/spherical-polar ...)
      COMMON /SET_GRID_R/                                                         &
     &      delX, delY

      _RL delX(grid_maxNx)
      _RL delY(grid_maxNy)

!EH3 ;;; Local Variables: ***
!EH3 ;;; mode:fortran ***
!EH3 ;;; End: ***
