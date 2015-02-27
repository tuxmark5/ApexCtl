# ApexCtl #

### Steelseries Apex and Apex [Raw] support for Linux ###

=========================================================

### Contributors ###
tuxmark5 : [github.com/tuxmark5](http://github.com/tuxmark5)  
Zimmux : [github.com/Zimmux](http://github.com/Zimmux)  
kiwistrongis : [github.com/kiwistrongis](http://github.com/kiwistrongis)  

## Dependencies ##
#### Debian: ####
```bash
sudo aptitude install ghc libusb-1.0-0-dev cabal-install git pkg-config
cabal update
cabal install usb-1.2.0.1 cmdargs
```
#### Fedora: ####
```bash
sudo yum -y install ghc libusb libusb-devel cabal-install git pkgconfig
cabal update
cabal install usb-1.2.0.1 cmdargs
```
#### Arch: ####
```bash
sudo pacman -Syu ghc libusb libusb cabal-install pkg-config
cabal update
cabal install usb-1.2.0.1 cmdargs
```
#### Other: ####
Install GHC, libusb 1.0.0 headers, cabal. Then:
```bash
cabal update
cabal install usb-1.2.0.1 cmdargs
```

## Installation ##

#### Global Install ####
```bash
make && sudo make install
```

#### Local User Install ####
You will have to run ~/.local/bin/apexctl manually (as root) to enable the extra keys after every boot.
```bash
make && make local-install
```

## Usage ##

#### Help ####
```bash
apexctl --help
```

#### Enable Macro Keys ####
```bash
sudo apexctl
```

#### Apex :: Set Colour Zones ####
```bash
sudo apexctl colors \
	-s RRGGBB:A \
	-n RRGGBB:A \
	-e RRGGBB:A \
	-w RRGGBB:A \
	-l RRGGBB:A
```
```bash
sudo apexctl colors \
	--south=RRGGBB:A \
	--north=RRGGBB:A \
	--east=RRGGBB:A \
	--west=RRGGBB:A \
	--logo=RRGGBB:A
```
Where RR, GG, and BB are in the domain of [00,ff], and A is in the domain of [1,8].

Example:
```bash
sudo apexctl colors \
	-s bb33bb:6 \
	-n ee1111:6 \
	-e 33aa33:6 \
	-w 88ee88:6 \
	-l aa7777:6
```

#### Apex [Raw] :: Set Brightness ####
```bash
sudo apexctl br (1..8)
```
Example:
```bash
sudo apexctl br 6
```

## Notes ##
Some distros ( Fedora 19, for example ) do not have ```/usr/local/sbin``` in their secure_path. This means you cannot just run ```sudo apexctl```, you will have to run ```sudo -E apexctl``` or ```sudo /usr/local/sbin/apexctl```. To fix this, there are two options.

Find the line that sets secure_path in ```/etc/sudoers``` and change it to the following ( or anything that includes ```/usr/local/sbin``` ):
```
Defaults    secure_path = /sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin
```

Alternatively, before installation, change this line in the makefile:
```
binary_install_dir = /usr/local/sbin
```
to:
```
binary_install_dir = /usr/sbin
```
