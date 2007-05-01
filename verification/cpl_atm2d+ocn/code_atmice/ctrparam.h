
! ==================================================
!
! ctrparam.h
! ----------
!
!  Purpose:	A header file contains cpp control 
!			parameters for the model
!
!  -------------------------------------------------
!	
!  Usage:	1. (un)comment #define line; or 
!		2. #define/#undef x [number]
!		  to set given cpp parameters x
!		  to be true or false  
!
!  -------------------------------------------------
!
!  Author:	Chien Wang
!
!  Revision:
!	Date	By		Brief Description
!	----	--		-----------------
!	052400	Chien Wang	created
!	121800	Chien Wang	add NYR_CHEM
!	062304	Chien Wang	rev for igsm2
!
! ==================================================

!
! === number of latitudinal grid points
!
#define N_LAT 46

!
! === number of vertical pressure layers
!
#define N_LEV 11

#define N_LON0 72

! === number of vertical  layers in duffusive ocean model
!
! --- Current options include 12 and 11
!
#define N_LEVOCEAN 11

!
! === defines 3D ocean
!
#define OCEAN_3D 1

!
! === coupling ocean co2 model
!
!
!#define CPL_OCEANCO2 1

! === coupling CLM
!
!#define CLM 1
!
!
#define NYR_CHEM 124

