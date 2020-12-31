#
# Makefile
#

BIN= ./bin/

RM= rm -f
MKDIR= mkdir -p

main:
	$(MKDIR) $(BIN)
	cd src && $(MAKE)

test:
	cd tests && $(MAKE)

clean:
	$(RM) -r $(BIN)
	$(RM) *.bc *.ll
	cd src && $(MAKE) $@
	cd tests && $(MAKE) $@
