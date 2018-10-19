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
    extraEnv = {
      # DEMO_LISTEN = "0.0.0.0:3000";
      # DEMO_ADDRESS = "127.0.0.1:3000";
      DEMO_WALLET_ADDRESS = "0.0.0.0:8090";
      DEMO_WALLET_DOC_ADDRESS = "0.0.0.0:8091";
      DEMO_STATE_DIR = stateDir;
    };
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

  tcpPorts = ports: listToAttrs (map (port: nameValuePair "${toString port}/tcp" {}) ports);
  portRange = start: count: range start (start + count - 1);

in

  dockerTools.buildImage {
    name = repoName;
    tag = "${version}-demo-cluster";
    fromImage = baseImage;
    contents = [ startScript ];
    config = {
      Cmd = [ "start-demo-cluster" ];
      Env = [
        "DEMO_NO_CLIENT_AUTH=True"
      ];
      ExposedPorts = tcpPorts ([
        8090  # wallet
        8091  # wallet doc
        8100  # explorer api
        8000  # ekg
        ]
        # core node protocol port
        ++ portRange 3000 demoCluster'.numCoreNodes
        # relay node protocol port
        ++ portRange 3100 demoCluster'.numRelayNodes
      );
    };
  }
