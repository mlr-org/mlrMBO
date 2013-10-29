R	:= R --no-save --no-restore
RSCRIPT	:= Rscript
DELETE	:= rm -fR
VERSION := $(shell Rscript ./tools/get-version)
TARGZ   := mlrMBO_$(VERSION).tar.gz

.SILENT:
.PHONEY: clean roxygenize package install test check

usage:
	printf "Available targets:"
	printf ""
	printf " clean         - Clean everything up"
	printf " roxygenize    - roxygenize in-place"
	printf " package       - build source package"
	printf " install       - install the package"
	printf " test          - run unit tests"
	printf " check         - run R CMD check on the package"
	printf " html          - build static html documentation"


clean:
	printf "\nCleaning up ...\n"
	${DELETE} src/*.o src/*.so *.tar.gz
	${DELETE} html
	${DELETE} .RData .Rhistory

roxygenize: clean
	printf "\nRoxygenizing package ...\n"
	${RSCRIPT} ./tools/roxygenize

package: roxygenize
	printf "\nBuilding package file $(TARGZ)\n"
	${R} CMD build . 
 
install: package
	printf "\nInstalling package $(TARGZ)\n"
	${R} CMD INSTALL $(TARGZ) 

test: install
	printf "\nTesting package $(TARGZ)\n"
	${RSCRIPT} ./test_all.R

check: package
	printf "\nRunning R CMD check ...\n"
	${R} CMD check $(TARGZ)

check-rev-dep: package
	printf "\nRunning reverse dependency checks for CRAN ...\n"
	${RSCRIPT} ./tools/check-rev-dep

html: install
	printf "\nGenerating html docs...\n"
	${DELETE} html
	mkdir html
	${RSCRIPT} ./tools/generate-html-docs
  
