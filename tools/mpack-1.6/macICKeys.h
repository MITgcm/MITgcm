/* (C) Copyright 1995 by Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of Carnegie
 * Mellon University not be used in advertising or publicity
 * pertaining to distribution of the software without specific,
 * written prior permission.  Carnegie Mellon University makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * CARNEGIE MELLON UNIVERSITY DISCLAIMS ALL WARRANTIES WITH REGARD TO
 * THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY BE LIABLE
 * FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
 * OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */
/*
	The canonical Internet Config interface is defined in Pascal.  These headers have
	not been thoroughly tested.  If there is a conflict between these headers and the
	Pascal interfaces, the Pascal should take precedence.
*/

/* ///////////////////////////////////////////////////////////////////////////////// */

#ifndef __ICKEYS__
#define __ICKEYS__

#ifndef __TYPES__
#include <Types.h>
#endif

#ifndef __ALIASES__
#include <Aliases.h>
#endif

/* ///////////////////////////////////////////////////////////////////////////////// */

#define kICRealName "\pRealName"	/* PString */
#define kICEmail "\pEmail"	/* PString -- user@host.domain */
#define kICMailAccount "\pMailAccount"	/* PString -- user@host.domain */
#define kICMailPassword "\pMailPassword"	/* PString -- scrambled */
#define kICNewsAuthUsername "\pNewsAuthUsername"	/* PString -- host.domain */
#define kICNewsAuthPassword "\pNewsAuthPassword"	/* PString -- scrambled */
#define kICArchiePreferred "\pArchiePreferred"	/* PString -- formatted */
#define kICArchieAll "\pArchieAll"	/* STR# -- formatted */
#define kICUMichPreferred "\pUMichPreferred"	/* PString -- formatted */
#define kICUMichAll "\pUMichAll"	/* STR# -- formatted */
#define kICInfoMacPreferred "\pInfoMacPreferred"	/* PString -- formatted */
#define kICInfoMacAll "\pInfoMacAll"	/* STR# -- formatted */
#define kICPhHost "\pPhHost"	/* PString -- host.domain */
#define kICWhoisHost "\pWhoisHost"	/* PString -- host.domain */
#define kICFingerHost "\pFingerHost"	/* PString -- host.domain */
#define kICFTPHost "\pFTPHost"	/* PString -- host.domain */
#define kICTelnetHost "\pTelnetHost"	/* PString -- host.domain */
#define kICSMTPHost "\pSMTPHost"	/* PString -- host.domain */
#define kICNNTPHost "\pNNTPHost"	/* PString -- host.domain */
#define kICGopherHost "\pGopherHost"	/* PString -- host.domain */
#define kICLDAPServer "\pLDAPServer"	/* PString -- host.domain */
#define kICLDAPSearchbase "\pLDAPSearchbase"	/* PString -- string LDAP thing */
#define kICWWWHomePage "\pWWWHomePage"	/* PString -- URL */
#define kICWAISGateway "\pWAISGateway"	/* PString -- no idea */
#define kICScreenFont "\pScreenFont"	/* ICFontRecord */
#define kICPrinterFont "\pPrinterFont"	/* ICFontRecord */
#define kICTextCreator "\pTextCreator"	/* ICAppSpec */
#define kICBinaryTypeCreator "\pBinaryTypeCreator"	/* ICFileInfo */
#define kICDownloadFolder "\pDownloadFolder"	/* ICFileSpec */
#define kICSignature "\pSignature"	/* TEXT */
#define kICOrganization "\pOrganization"	/* PString */
#define kICPlan "\pPlan"	/* TEXT */
#define kICQuotingString "\pQuotingString"	/* PString */
#define kICMailHeaders "\pMailHeaders"	/* TEXT */
#define kICNewsHeaders "\pNewsHeaders"	/* TEXT */
#define kICMapping "\pMapping"	/* ICMapEntries */
#define kICCharacterSet "\pCharacterSet"	/* ICCharTable */
#define kICHelper "\pHelper¥"	/* ICAppSpec */
#define kICServices "\pServices"	/* ICServices */

#if defined(powerc) || defined (__powerc)
#pragma options align=mac68k
#endif

struct ICFontRecord {
	short size;
	Style face;
	char pad;
	Str255 font;
};
typedef struct ICFontRecord ICFontRecord, *ICFontRecordPtr, **ICFontRecordHandle;

struct ICCharTable {
	unsigned char net_to_mac[256];
	unsigned char mac_to_net[256];
};
typedef struct ICCharTable ICCharTable, *ICCharTablePtr, **ICCharTableHandle;

struct ICAppSpec {
	OSType fCreator;
	Str63 name;
};
typedef struct ICAppSpec ICAppSpec, *ICAppSpecPtr, **ICAppSpecHandle;

struct ICFileInfo {
	OSType fType;
	OSType fCreator;
	Str63 name;
};
typedef struct ICFileInfo ICFileInfo, *ICFileInfoPtr, **ICFileInfoHandle;

struct ICFileSpec {
	Str31 vol_name;
	long vol_creation_date;
	FSSpec fss;
	AliasRecord alias;
	/* plus extra data, aliasSize 0 means no alias manager present when
			ICFileSpecification was created */
};
typedef struct ICFileSpec ICFileSpec, *ICFileSpecPtr, **ICFileSpecHandle;

enum {
	ICfile_spec_header_size = sizeof(ICFileSpec) - sizeof(AliasRecord)
};

struct ICMapEntry {
	short total_length;
	short fixed_length;
	short version;
	OSType file_type;
	OSType file_creator;
	OSType post_creator;
	long flags;
	/* variable part starts here */
	Str255 extension;
	Str255 creator_app_name;
	Str255 post_app_name;
	Str255 MIME_type;
	Str255 entry_name;
};
typedef struct ICMapEntry ICMapEntry, *ICMapEntryPtr, **ICMapEntryHandle;

enum {
	ICmap_binary_bit = 0,						/* file should be transfered in binary as opposed to text mode */
	ICmap_binary_mask = 0x00000001,
	ICmap_resource_fork_bit = 1,		/* the resource fork of the file is significant */
	ICmap_resource_fork_mask = 0x00000002,
	ICmap_data_fork_bit = 2,				/* the data fork of the file is significant */
	ICmap_data_fork_mask = 0x00000004,

	ICmap_post_bit = 3,							/* post process using post fields */
	ICmap_post_mask = 0x00000008,

	ICmap_not_incoming_bit = 4,			/* ignore this mapping for incoming files */
	ICmap_not_incoming_mask = 0x00000010,
	ICmap_not_outgoing_bit = 5,			/* ignore this mapping for outgoing files */
	ICmap_not_outgoing_mask = 0x00000020,

	ICmap_fixed_length = 22					/* number in fixed_length field */
};

struct ICServiceEntry {
	Str255 name;
	short port;
	short flags;
};
typedef struct ICServiceEntry ICServiceEntry, *ICServiceEntryPtr, **ICServiceEntryHandle;

struct ICServices {
	short count;
	ICServiceEntry services[1];
};
typedef struct ICServices ICServices, *ICServicesPtr, **ICServicesHandle;

enum {
	ICservices_tcp_bit = 0,
	ICservices_tcp_mask = 0x00000001,
	ICservices_udp_bit = 1,
	ICservices_udp_mask = 0x00000002
	/* both bits can be set, which means the service is both TCP and UDP, eg daytime */
};

#if defined(powerc) || defined(__powerc)
#pragma options align=reset
#endif

#endif
