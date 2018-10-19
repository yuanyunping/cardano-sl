{ writeScriptBin, dockerTools
, glibcLocales, iana-etc, bashInteractive

, cardano-sl-cluster-static, demoCluster, version

# Used to generate the docker image names
, repoName ? "cardano-sl"
}:

with import ../lib.nix;

let
  stateDir = "/state";
  demoCluster' = demoCluster.override {
    inherit stateDir;
  };

  startScript = writeScriptBin "start-demo-cluster" ''
    #!/bin/sh
    set -euo pipefail

    export LOCALE_ARCHIVE="${glibcLocales}/lib/locale/locale-archive"

    if [ ! -d ${stateDir} ]; then
      echo '${stateDir} volume not mounted'
      echo 'You need to create one with `docker volume create`,'
      echo 'and pass the correct -v flag to `docker run`'
      exit 1
    fi

    exec ${demoCluster'} "$@"
  '';

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-cluster-env";
    contents = [ iana-etc bashInteractive ];
  };

in

  dockerTools.buildImage {
    name = repoName;
    tag = "${version}-demo-cluster";
    fromImage = baseImage;
    contents = [ startScript ];
    config = {
      Cmd = [ "start-demo-cluster" ];
      ExposedPorts = {
        "3000/tcp" = {}; # protocol
        "8090/tcp" = {}; # wallet
        "8091/tcp" = {}; # wallet doc
        "8100/tcp" = {}; # explorer api
        "8000/tcp" = {}; # ekg
      };
    };
  }
