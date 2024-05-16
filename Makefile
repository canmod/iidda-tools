R/%/doc-update: R/%/R/*.R
	echo "suppressWarnings(roxygen2::roxygenize(\"$(dir $@)\",roclets = c(\"collate\", \"rd\", \"namespace\")))" | R --slave
	touch $@


pkg-build:: ../macpan2_$(VERSION).tar.gz
../macpan2_$(VERSION).tar.gz: .Rbuildignore DESCRIPTION man/*.Rd R/*.R src/*.cpp tests/testthat/test-*.R tests/testthat.R inst/starter_models/**/tmb.R doc-update
	cd .. && R CMD build --no-build-vignettes macpan2
	touch pkg-build


pkg-check: ../macpan2_$(VERSION).tar.gz pkg-build
	cd .. && R CMD check macpan2_$(VERSION).tar.gz
	touch pkg-check


pkg-install: ../macpan2_$(VERSION).tar.gz pkg-build
	cd .. && R CMD INSTALL --no-multiarch --install-tests macpan2_$(VERSION).tar.gz



iidda-r-pkg :
	cd R/iidda && make build-package && make install

check-update:
	git pull

start-api:
	@cd python && make runapp

stop-api:
	@cd python && make stopapp

update-start-api:
	make check-update && make start-api
