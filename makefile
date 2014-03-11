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

#commands
check-root:
	[[ `whoami` = "root" ]]

build: $(binary)
	cp $(binary) apexctl

enable: check-root
	xmodmap config/Xmodmap
	./apexctl

disable: check-root
	./reset-usb

install: check-root
	mkdir -p /etc/udev/hwdb.d
	mkdir -p /etc/udev/rules.d
	install -m 755 apexctl /usr/local/bin
	install config/90-apex.hwdb /etc/udev/hwdb.d
	install config/90-apexctl.rules /etc/udev/rules.d
	cp /etc/X11/Xmodmap /etc/X11/Xmodmap.bak
	cat config/Xmodmap >> /etc/X11/Xmodmap
	udevadm hwdb --update
	udevadm control --reload

uninstall: check-root
	rm /usr/local/apexctl \
		/etc/udev/hwdb.d/90-apex.hwdb \
		/etc/udev/rules.d/90-apexctl.rules
	[[ -f /etc/X11/Xmodmap.bak ]] && \
		cp /etc/X11/Xmodmap /etc/X11/Xmodmap.bak2 && \
		mv /etc/X11/Xmodmap.bak /etc/X11/Xmodmap
	udevadm hwdb --update
	udevadm control --reload
