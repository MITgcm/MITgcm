AUTOMAKE_OPTIONS=foreign
ACLOCAL=aclocal -I $(srcdir)/cmulocal

bin_PROGRAMS=mpack munpack

mpack_SOURCES=unixpk.c encode.c codes.c magic.c unixos.c xmalloc.c \
	md5c.c
mpack_LDADD=@LIBOBJS@ @LIB_SOCKET@

munpack_SOURCES=unixunpk.c decode.c uudecode.c codes.c unixos.c \
	part.c xmalloc.c md5c.c

munpack_LDADD=@LIBOBJS@ @LIB_SOCKET@

noinst_HEADERS=common.h md5.h part.h version.h xmalloc.h
man_MANS=mpack.1 munpack.1

BUILT_SOURCES=mpack.1 munpack.1

mpack.1: unixpk.man
	cp $? $@
munpack.1: unixunpk.man
	cp $? $@

EXTRA_DIST=README.mac   macICKeys.h   macmpack.c  macndlog.c  macntcp.c \
 macproj.hqx README.unix  macICTypes.h  macmpack.h  macnfile.c  macnte.c \
 macrsrc.hqx macICAPI.h   macbhex.c     macnapp.c   macninit.c  macos.c \
 mkreadme.pl macICGlue.v  macfile.c     macnapp.h   macnsmtp.c  macpcstr.c \
 unixpk.man unixunpk.man cmulocal Changes

