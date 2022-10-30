R=R
# -> you can do    R=R-devel  make ....
PACKAGE=iidda

## turn off use of shell because of R CMD Check
## warning that it is not portable
#VERSION := $(shell sed -n '/^Version: /s///p' ./DESCRIPTION)

#testversion:
#	echo "${VERSION}"

#TARBALL := $(PACKAGE)_$(VERSION).tar.gz
#ZIPFILE := =$(PACKAGE)_$(VERSION).zip

#build-package: $(TARBALL)
#$(TARBALL): ./NAMESPACE
#	$(R) CMD build .
#	mv $@ ..

#install: $(TARBALL)
#	export NOT_CRAN=true; $(R) CMD INSTALL --preclean ../$<
#	@touch $@


#	Rscript -e "remotes::install_github('canmod-eidm/iidda-tools', subdir = 'R/iidda')"
