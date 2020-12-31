#
# Makefile
#

CP= cp -f
SRC= ../src

test: elo
	cd scanner && $(MAKE)
	cd sem && $(MAKE)

elo:
	cd $(SRC) && $(MAKE) pack
	$(CP) $(SRC)/elo.cmi $(SRC)/elo.cmx $(SRC)/elo.o ./scanner/
	$(CP) $(SRC)/elo.cmi $(SRC)/elo.cmx $(SRC)/elo.o ./sem/

clean:
	cd scanner && $(MAKE) $@
	cd sem     && $(MAKE) $@
