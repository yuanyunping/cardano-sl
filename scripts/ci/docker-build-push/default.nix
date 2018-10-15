# This script will load nix-built docker images of cardano-sl wallet
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".

let
  localLib = import ../../../lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, iohkPkgs ? import ../../.. { inherit config system; }
, pkgs ? iohkPkgs.pkgs
, hostPkgs ? import <nixpkgs> { inherit config system; }
}:

with hostPkgs;
with hostPkgs.lib;

let
  images = attrValues (localLib.forEnvironments ({ environment, ...}:
    { name = "wallet-${environment}";
      image = iohkPkgs.dockerImages.${environment}.wallet; }));

in
  writeScript "docker-build-push" (''
    #!${stdenv.shell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

    repo=cardano-sl
    username="$(docker info | sed '/Username:/!d;s/.* //')"

  '' + concatMapStringsSep "\n" ({ name, image }: ''
    echo "Loading ${image}"
    tagged="$username/$repo:${iohkPkgs.cardano-sl-node-static.version}-${name}"
    docker load -i "${image}"
    docker tag "${image.imageName}:${image.imageTag}" "$tagged"
    docker push "$tagged"
  '') images)
