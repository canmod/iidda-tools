iidda-r-pkg :
	cd R/iidda && make build-package && make install

check-update:
	git pull

start-api:
	@cd python && make runapp

update-start-api:
	make check-update && make start-api
