# ApexCtl #

===============================================================

## Steelseries Apex and Apex [Raw] support for Linux ##

### Contributors ###
tuxmark5 : [github.com/tuxmark5](http://github.com/tuxmark5)  
Zimmux : [github.com/Zimmux](http://github.com/Zimmux)  
kiwistrongis : [github.com/kiwistrongis](http://github.com/kiwistrongis)  

## Dependencies ##
Debian:
```bash
sudo aptitude install ghc libusb-1.0-0-dev cabal-install git pkg-config
cabal update
cabal install usb cmdargs
```
Fedora:
```bash
sudo yum -y install ghc libusb libusb-devel cabal-install git pkgconfig
cabal update
cabal install usb cmdargs
```
Other:  
Install GHC, libusb 1.0.0 headers, cabal. Then:
```bash
cabal update
cabal install usb cmdargs
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

#### Notes ####
Some distros ( fedora 19, for example ) do not have /usr/local/sbin in their secure_path. This means you cannot just run ```sudo apexctl```, you will have to run ```sudo -E apexctl``` or ```sudo /usr/local/sbin/apexctl```. To fix this, there are two options.

Find the line that sets secure_path in /etc/sudoers and change it to the following ( or anything that includes /usr/local/sbin ):
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