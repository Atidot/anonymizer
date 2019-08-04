{ nixpkgs ? import <nixpkgs> {}
}:

with nixpkgs;
let
  secrets-dir = "/var/.secrets";
  atidot-anonymizer = import ./default.nix {};
in
nixpkgs.dockerTools.buildImage {
  name = "anonymizer-api";
  tag = "latest";
  fromImage = dockerTools.pullImage {
    imageName = "ubuntu";
    sha256 = "105lm1rwnawg9hx7jmxci146x21s90dw8lchq5rlbb2rsh84dk83";
    imageDigest = "sha256:f961d3d101e66017fc6f0a63ecc0ff15d3e7b53b6a0ac500cd1619ded4771bd6";
  };
  contents = [ nixpkgs.bash
               nixpkgs.nss
               nixpkgs.cacert
               nixpkgs.coreutils
               atidot-anonymizer
             ];

  runAsRoot = ''
    #!${pkgs.stdenv.shell}
    ${nixpkgs.dockerTools.shadowSetup}
  '';

  config = {
        Entrypoint = [ "bash" "-c" ];
        Cmd =  [(pkgs.lib.concatStringsSep " " [
            "atidot-anonymizer" "api"
            "--sslKey" ''''${SSL_KEY:=${secrets-dir}/ssl-key.key}''
            "--sslCrt" ''''${SSL_CERT:=${secrets-dir}/ssl-cert.crt}''
            "--key"    ''''${HASH_KEY:=${secrets-dir}/hashing-key.key}''
            "--port"   ''''${PORT:=443}''
          ] )] ;
      };
}
