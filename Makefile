#
# Makefile for PIC Programmer
# Copyright 1994-2002 by Brian C. Lane <bcl@brianlane.com>
#
# Because of the need for root access to the ioports this program must be
# compiled and installed by root. You could allow users to run it, but
# that probably isn't the best idea.
#
# The /usr/include/ncurses include path is needed so that the curses.h
# include will find the right files for ncurses
#
ifeq ($(strip $(CC)),)
  CC = gcc
else
  CC:=${CC}
endif

VERSION = 2.3.0
INCLUDES = -I/usr/include/ncurses
CFLAGS	=  -O2 -Wall -pipe -DVERSION=\"$(VERSION)\" -g
LIBS	= -lncurses

PICOBJS	= picprg.o lowlvl.o pichex.o

all:		picprg

picprg:		$(PICOBJS)
		$(CC) $(PICOBJS) -o picprg $(LIBS) -DVERSION=\"$(VERSION)\"

clean:
		rm -f *~ *.o core picprg *.asc
		rm -fr picprg-$(VERSION)

install:	all
		install -b -o root -g root -m u=rwxs picprg /usr/local/bin

# Build the archive of everything
archive:	picprg sign
		cd .. && tar cvzf picprg-$(VERSION).tar.gz picprg-$(VERSION)/* --exclude *.o

# Sign the binaries using gpg (www.gnupg.org)
# My key is available from www.brianlane.com
sign:
		gpg -ba picprg


# Build the source distribution
source:		archive

dist:		archive

# Build RedHat binary and source RPMs
rpm:	dist
	su -c "cp ../picprg-$(VERSION).tar.gz /usr/src/redhat/SOURCES"
	rm -f picprg-$(VERSION)-1.spec
	ln -s picprg.spec picprg-$(VERSION)-1.spec
	su -c "rpm -ba -vv picprg-$(VERSION)-1.spec"

