R	:= R --no-save --no-restore
RSCRIPT	:= Rscript
DELETE	:= rm -fR
VERSION := $(shell Rscript ./tools/get-version)
TARGZ   := mlrMBO_$(VERSION).tar.gz

.SILENT:
.PHONEY: clean roxygenize package install test check

usage:
	echo "Available targets:"
	echo " clean         - Clean everything up"
	echo " roxygenize    - roxygenize in-place"
	echo " package       - build source package"
	echo " install       - install the package"
	echo " test          - run unit tests"
	echo " check         - run R CMD check on the package"
	echo " html          - build static html documentation"


clean:
	echo "\nCleaning up ...\n"
	${DELETE} src/*.o src/*.so *.tar.gz
	${DELETE} html
	${DELETE} *.Rcheck
	${DELETE} .RData .Rhistory

roxygenize: clean
	echo "\nRoxygenizing package ...\n"
	${RSCRIPT} ./tools/roxygenize

package: roxygenize
	echo "\nBuilding package file $(TARGZ)\n"
	${R} CMD build . 
 
install: package
	echo "\nInstalling package $(TARGZ)\n"
	${R} CMD INSTALL $(TARGZ) 

test: install
	echo "\nTesting package $(TARGZ)\n"
	${RSCRIPT} ./test_all.R

check: package
	echo "\nRunning R CMD check ...\n"
	${R} CMD check $(TARGZ)

check-rev-dep: package
	echo "\nRunning reverse dependency checks for CRAN ...\n"
	${RSCRIPT} ./tools/check-rev-dep

html: install
	echo "\nGenerating html docs...\n"
	${DELETE} html
	mkdir html
	${RSCRIPT} ./tools/generate-html-docs
  
