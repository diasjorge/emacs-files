# How to release a new ergoemacs-keybindings distribution?
#
# Just run "make update-version VERSION=x.x.x"
# And then "make package"

CURRENT_VERSION = $(shell cat ergoemacs-mode.el \
			| grep ";; Version:" \
			| sed -e "s/^;; Version: *\([0-9.]*\)/\1/")

PACKAGE_NAME = ergoemacs-keybindings-$(CURRENT_VERSION)

# Default rule. Shows the how to use this Makefile
all:
	@echo Usage:
	@echo "  make package"
	@echo "    Creates a .zip file to distribute the current ergoemacs-keybindings version."
	@echo
	@echo "  make package-tarball"
	@echo "    Creates a .tar.gz file to distribute the current ergoemacs-keybindings version."
	@echo
	@echo "  make show-version"
	@echo "    Shows the current version ergoemacs-keybindings package. The version is extracted"
	@echo "    from ergoemacs-mode.el file."
	@echo
	@echo "  make update-version VERSION=x.x.x"
	@echo "    Changes the ergoemacs-keybindings version from ergoemacs-mode.el file"
	@echo

# Makes a .zip with the ergoemacs-keybindings package.
package:
	mkdir $(PACKAGE_NAME)
	cp *.el *.txt $(PACKAGE_NAME)
	-rm $(PACKAGE_NAME).zip
	zip -9 $(PACKAGE_NAME).zip $(PACKAGE_NAME)/*.*
	rm -fr $(PACKAGE_NAME)

# Makes a .tar.gz file with the ergoemacs-keybindings package.
package-tarball:
	mkdir $(PACKAGE_NAME)
	cp *.el *.txt $(PACKAGE_NAME)
	-rm $(PACKAGE_NAME).tar.gz
	tar czf $(PACKAGE_NAME).tar.gz $(PACKAGE_NAME)/*.*
	rm -fr $(PACKAGE_NAME)

# Displays the current version
show-version:
	@echo $(CURRENT_VERSION)

# Changes the version of the current ergoemacs-mode.el
update-version:
	cat ergoemacs-mode.el \
		| sed -e "s/^;; Version:[ 0-9.]*/;; Version: $(VERSION)/" \
		      -e "s/ergoemacs-mode-version \"[ 0-9.]*\"/ergoemacs-mode-version \"$(VERSION)\"/" \
		> ergoemacs-mode.tmp
	mv ergoemacs-mode.tmp ergoemacs-mode.el
