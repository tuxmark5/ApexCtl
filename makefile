#globals
default: build
freshen: clean build
clean:
	rm -rf dist apexctl

#lists
hs_files = src/Main.hs
config_files = \
	config/90-apexctl.rules \
	config/90-apex.hwdb \
	config/Xmodmap
binary = dist/build/apexctl/apexctl

#build defs
$(binary): $(hs_files)
	cabal configure
	cabal build
apexctl: $(binary)
	cp $(binary) $@

#checks
check-build:
	[ -f apexctl ]
check-root:
	[[ `whoami` = "root" ]]
check-installed:
	[ -f /usr/local/sbin/apexctl ]
	[ -f /usr/local/sbin/apexctl-reset ]
	[ -f /etc/udev/hwdb.d/90-apex.hwdb ]
	[ -f /etc/udev/rules.d/90-apexctl.rules ]
	[ -f /etc/X11/Xmodmap.bak ]

#basic commands
build: $(binary) apexctl

enable: check-root
	./apexctl
	xmodmap config/Xmodmap

disable: check-root
	./apex-reset

#global installation
install: check-build check-root enable
	#make dirs
	mkdir -p /etc/udev/hwdb.d
	mkdir -p /etc/udev/rules.d
	#install binary
	install -m 755 apexctl /usr/local/sbin/apexctl
	install -m 755 apexctl-reset /usr/local/sbin/apexctl-reset
	#install udev rules
	install config/90-apex.hwdb /etc/udev/hwdb.d/
	install config/90-apexctl.rules /etc/udev/rules.d/
	#install Xmodmap globally
	cp /etc/X11/Xmodmap /etc/X11/Xmodmap.bak
	cat config/Xmodmap >> /etc/X11/Xmodmap
	#install reload udev rules
	udevadm hwdb --update
	udevadm control --reload

uninstall: check-root disable
	#remove binary and udev rules
	rm -f \
		/usr/local/sbin/apexctl \
		/usr/local/sbin/apexctl-reset \
		/etc/udev/hwdb.d/90-apex.hwdb \
		/etc/udev/rules.d/90-apexctl.rules
	#unapply Xmodmap using backup made during install
	[[ -f /etc/X11/Xmodmap.bak ]] && \
		cp /etc/X11/Xmodmap /etc/X11/Xmodmap.bak2 && \
		mv /etc/X11/Xmodmap.bak /etc/X11/Xmodmap
	#install reload udev rules
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
	#install Xmodmap locally
	install config/Xmodmap ~/.Xmodmap

local-uninstall:
	#remove binary and Xmodmap
	rm -f ~/.local/bin/apexctl ~/.Xmodmap

local-reinstall: check-build \
	local-uninstall local-install
