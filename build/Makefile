NIX_FLAGS=--cores 0 -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/19.03.tar.gz

build:
	nix-build ${NIX_FLAGS} default.nix

notebook:
	nix-shell ${NIX_FLAGS} -A env notebook.nix --command "jupyter lab --notebook-dir=../examples/"

api-server:
	nix-build ${NIX_FLAGS} api-server-docker.nix -o api-server.tar.gz
	docker load -i api-server.tar.gz

notebook-docker:
	nix-build ${NIX_FLAGS} notebook-docker.nix -o notebook-docker.tar.gz
	docker load -i notebook-docker.tar.gz

swagger:
	nix-shell ${NIX_FLAGS} default.nix --command "atidot-anonymizer swagger"

shell:
	nix-shell ${NIX_FLAGS} default.nix