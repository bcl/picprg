Summary: PICprg, a programmer for Microchip PIC microprocessors
Name: picprg
Version: 2.3.0
Release: 1
Copyright: GPL
Group: Applications/System
Source: http://www.brianlane.com/linux/picprg-2.3.0.tar.gz
URL: http://www.brianlane.com/picprg.php
Packager: bcl@brianlane.com
Prefix: /usr/local
Buildroot: /var/tmp/%{name}-buildroot

%description
PICprg is a command line or ncurses based program for programming Microchip PIC
microprocessors using a parallel port programmer like the ones designed by 
David Tait. It has a very user friendly parallel port setup menu that allows
parallel port pins to be assigned to programmer functions, polarity is pin
selectable, as well as a test mode to turn the pin on and off.

%prep
%setup

%build
make

%install
install -d -m 4700 $RPM_BUILD_ROOT/usr/local/bin/
install -s -m 4700 picprg $RPM_BUILD_ROOT/usr/local/bin/

%files
%doc COPYRIGHT COPYING FAQ README ChangeLog
/usr/local/bin/picprg
