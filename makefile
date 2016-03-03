# globals
default: build
freshen: clean build
clean:
	rm -rf dist apexctl

# vars
SHELL := /bin/bash
INSTALL_ROOT ?= /asdf

# lists
hs_files = src/Main.hs
config_files = \
	config/90-apexctl.rules \
	config/90-apex.hwdb \
	config/Xmodmap
binary = dist/build/apexctl/apexctl
binary_install_dir = /usr/local/bin

# build defs
$(binary): $(hs_files)
	cabal configure
	cabal build
apexctl: $(binary)
	cp $(binary) $@

# checks
check-build:
	[ -f apexctl ]
	[ -f $(binary) ]
check-root:
	[[ `whoami` = "root" ]]
check-installed:
	[ -f $(INSTALL_ROOT)$(binary_install_dir)/apexctl ]
	[ -f $(INSTALL_ROOT)/etc/udev/hwdb.d/90-apex.hwdb ]
	[ -f $(INSTALL_ROOT)/etc/udev/rules.d/90-apexctl.rules ]
	[ -f $(INSTALL_ROOT)/etc/X11/Xmodmap.bak ]
	echo -en "ApexCtl is fully installed\n"

# basic commands
build: $(binary) apexctl

enable: check-root
	# enable keys
	./apexctl
	# reload udev rules
	udevadm hwdb --update
	udevadm control --reload
	# apply xmodmap
	xmodmap config/Xmodmap

# global installation
install: check-build
	# make dirs
	mkdir -p $(INSTALL_ROOT)/etc/udev/hwdb.d
	mkdir -p $(INSTALL_ROOT)/etc/udev/rules.d
	# install binary
	install -m 755 apexctl $(INSTALL_ROOT)$(binary_install_dir)/apexctl
	# install udev rules
	install config/90-apex.hwdb $(INSTALL_ROOT)/etc/udev/hwdb.d/
	install config/90-apexctl.rules $(INSTALL_ROOT)/etc/udev/rules.d/
	# install Xmodmap globally
	[[ -f $(INSTALL_ROOT)/etc/X11/Xmodmap ]] && \
		cp $(INSTALL_ROOT)/etc/X11/Xmodmap $(INSTALL_ROOT)/etc/X11/Xmodmap.bak || :
	cat config/Xmodmap >> $(INSTALL_ROOT)/etc/X11/Xmodmap

uninstall:
	# remove binary, scripts, and udev rules
	rm -f \
		$(INSTALL_ROOT)$(binary_install_dir)/apexctl \
		$(INSTALL_ROOT)/etc/udev/hwdb.d/90-apex.hwdb \
		$(INSTALL_ROOT)/etc/udev/rules.d/90-apexctl.rules
	# unapply Xmodmap using backup made during install
	[[ -f $(INSTALL_ROOT)/etc/X11/Xmodmap.bak ]] && \
		cp $(INSTALL_ROOT)/etc/X11/Xmodmap $(INSTALL_ROOT)/etc/X11/Xmodmap.bak2 && \
		mv $(INSTALL_ROOT)/etc/X11/Xmodmap.bak $(INSTALL_ROOT)/etc/X11/Xmodmap || :
	# reload udev rules
	udevadm hwdb --update
	udevadm control --reload

reinstall: \
	check-build \
	uninstall install

# local installation
local-install: check-build
	# make dirs
	mkdir -p ~/.local/bin
	# install binary
	install -m 755 apexctl ~/.local/bin/apexctl
	# install Xmodmap locally
	[[ -f ~/.Xmodmap ]] && \
		cp ~/.Xmodmap ~/.Xmodmap.bak || :
	cat config/Xmodmap >> /etc/X11/Xmodmap
	install config/Xmodmap ~/.Xmodmap

local-uninstall:
	# remove binary, scripts, and Xmodmap
	rm -f ~/.local/bin/apexctl ~/.Xmodmap

local-reinstall: check-build \
	local-uninstall local-install

test:
	echo $(INSTALL_ROOT)
