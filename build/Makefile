NIX_FLAGS=--cores 0 --max-jobs 4 --option build-use-sandbox false

build:
	nix-build ${NIX_FLAGS} default.nix

notebook:
	nix-shell ${NIX_FLAGS} -A env notebook.nix --command "jupyter lab --notebook-dir=../examples/"

docker:
	nix-build ${NIX_FLAGS} docker.nix -o docker.tar.gz
