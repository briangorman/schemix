obj-m	:= schemix.o

include /lib/modules/$(shell uname -r)/build/Rules.make
