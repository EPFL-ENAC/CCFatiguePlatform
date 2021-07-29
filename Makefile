install: install_backend install_frontend

build: install build_frontend

install_backend:
	$(MAKE) -C backend install

install_frontend:
	$(MAKE) -C frontend install

build_frontend:
	$(MAKE) -C frontend build
