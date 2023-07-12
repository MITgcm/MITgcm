/*
##########################################################
# This file is part of the AdjoinableMPI library         #
# released under the MIT License.                        #
# The full COPYRIGHT notice can be found in the top      #
# level directory of the AdjoinableMPI distribution.     #
########################################################## 
*/
#ifndef _AMPI_ST_H_
#define _AMPI_ST_H_

/**
 * \file 
 * \ingroup UserInterfaceHeaders
 * ST = "source tansformation" specific versions of routines that exist because generic memory mapping is not yet implemented in any source transformation tool   
 */ 

#include "ampi/userIF/request.h"

/** \ingroup UserInterfaceDeclarations
 * @{
 */

/**
 * variant of \ref AMPI_Wait with an extra parameter 
 * \param request follows the semantics of  <tt>MPI_Wait</tt>
 * \param buf is the buffer that was passed to the corresponding \ref AMPI_Isend or \ref AMPI_Irecv call; 
 * see \ref nonblocking for a discussion of the necessity of the buf parameter.
 * \param status follows the semantics of  <tt>MPI_Wait</tt>
 */
int AMPI_Wait_ST(AMPI_Request *request, 
		 void *buf,
		 MPI_Status *status);

/** @} */

#endif
