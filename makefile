#globals
default: build
freshen: clean build
clean:
	rm -rf dist apexctl

#vars
SHELL := /bin/bash

#lists
hs_files = src/Main.hs
config_files = \
	config/90-apexctl.rules \
	config/90-apex.hwdb \
	config/Xmodmap
binary = dist/build/apexctl/apexctl
binary_install_dir = /usr/local/sbin

#build defs
$(binary): $(hs_files)
	cabal configure
	cabal build
apexctl: $(binary)
	cp $(binary) $@

#checks
check-build:
	[ -f apexctl ]
	[ -f $(binary) ]
check-root:
	[[ `whoami` = "root" ]]
check-installed:
	[ -f $(binary_install_dir)/apexctl ]
	[ -f $(binary_install_dir)/apexctl-resethub ]
	[ -f /etc/udev/hwdb.d/90-apex.hwdb ]
	[ -f /etc/udev/rules.d/90-apexctl.rules ]
	[ -f /etc/X11/Xmodmap.bak ]
	echo -en "ApexCtl is fully installed\n"

#basic commands
build: $(binary) apexctl

enable: check-root
	./apexctl
	xmodmap config/Xmodmap

#global installation
install: check-build check-root
	#make dirs
	mkdir -p /etc/udev/hwdb.d
	mkdir -p /etc/udev/rules.d
	#install binary
	install -m 755 apexctl $(binary_install_dir)/apexctl
	#install scripts
	install -m 755 apexctl-resethub $(binary_install_dir)/apexctl-resethub
	#install udev rules
	install config/90-apex.hwdb /etc/udev/hwdb.d/
	install config/90-apexctl.rules /etc/udev/rules.d/
	#install Xmodmap globally
	[[ -f /etc/X11/Xmodmap ]] && \
		cp /etc/X11/Xmodmap /etc/X11/Xmodmap.bak || :
	cat config/Xmodmap >> /etc/X11/Xmodmap
	#reload udev rules
	udevadm hwdb --update
	udevadm control --reload

uninstall: check-root
	#remove binary, scripts, and udev rules
	rm -f \
		$(binary_install_dir)/apexctl \
		$(binary_install_dir)/apexctl-resethub \
		/etc/udev/hwdb.d/90-apex.hwdb \
		/etc/udev/rules.d/90-apexctl.rules
	#unapply Xmodmap using backup made during install
	[[ -f /etc/X11/Xmodmap.bak ]] && \
		cp /etc/X11/Xmodmap /etc/X11/Xmodmap.bak2 && \
		mv /etc/X11/Xmodmap.bak /etc/X11/Xmodmap || :
	#reload udev rules
	udevadm hwdb --update
	udevadm control --reload

reinstall: check-build check-root \
	uninstall install

#local installation
local-install: check-build
	#make dirs
	mkdir -p ~/.local/bin
	#install binary
	install apexctl ~/.local/bin/apexctl
	chmod +x ~/.local/bin/apexctl
	#install scripts
	install apexctl-resethub ~/.local/bin/apexctl-resethub
	chmod +x ~/.local/bin/apexctl-resethub
	#install Xmodmap locally
	install config/Xmodmap ~/.Xmodmap

local-uninstall:
	#remove binary, scripts, and Xmodmap
	rm -f ~/.local/bin/apexctl ~/.Xmodmap

local-reinstall: check-build \
	local-uninstall local-install
