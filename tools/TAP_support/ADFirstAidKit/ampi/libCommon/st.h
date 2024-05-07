/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_LIBCOMMON_ST_H_
#define _AMPI_LIBCOMMON_ST_H_

/**
 * \file 
 * common AD implementation portion of AMPI routines from ampi/userIF/st.h
 */ 

#include <mpi.h>

#if defined(__cplusplus)
extern "C" {
#endif

#include "ampi/userIF/request.h"

/** 
 * forward sweep variant of \ref AMPI_Wait_ST 
 */
int FW_AMPI_Wait_ST(AMPI_Request *request, 
		    void *buf,
		    MPI_Status *status);

/** 
 * backward sweep variant of \ref AMPI_Wait_ST 
 */
int BW_AMPI_Wait_ST(AMPI_Request *request, 
		    void *buf,
		    MPI_Status *status);

#if defined(__cplusplus)
}
#endif

#endif
