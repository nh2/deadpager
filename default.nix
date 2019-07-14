{
  pkgs ? (import <nixpkgs> {})
}:
with pkgs.haskell.lib;
let
  # Pin nixpkgs version for Haskell build (which has the GHC we use in stack.yaml).
  pkgs-19_09 = import (fetchTarball https://github.com/nh2/nixpkgs/archive/a2d7e9b875e8ba7fd15b989cf2d80be4e183dc72.tar.gz) {};
  stack2nix_haskellPackages = import ./stack2nix-output.nix { pkgs = pkgs-19_09; };

  bootstrap-css = builtins.fetchurl {
    url = https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css;
    sha256 = "0dldiln2s3z8iqc5ccjid2i5gh9527naas064bwly8x9lrfrxcb0";
  };
  jquery-js = builtins.fetchurl {
    url = https://code.jquery.com/jquery-3.3.1.slim.min.js;
    sha256 = "1cgv6agm53y854kpvslcd1xwjf63m350c7y8gywhxlwh5fdnpryx";
  };
  popper-js = builtins.fetchurl {
    url = https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js;
    sha256 = "1hhi8jw0gv2f1ir59ydzvgrspj6vb121wf36ddl4mdm93xza1wv6";
  };
  bootstrap-js = builtins.fetchurl {
    url = https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js;
    sha256 = "0yspqrh258iqzvkacih0a1rnnngxhalvnmpczvsc2ff589wahd0a";
  };

  deadpager-server = overrideCabal stack2nix_haskellPackages.deadpager-server (old: {
    preConfigure = ''
      ${old.preConfigure or ""}

      mkdir -p static/
      cp "${bootstrap-css}" "static/bootstrap.min.css"
      cp "${jquery-js}" "static/jquery.min.js"
      cp "${popper-js}" "static/popper.min.js"
      cp "${bootstrap-js}" "static/bootstrap.min.js"
    '';

    enableLibraryProfiling = false;
    enableExecutableProfiling = false;
  });
in
  deadpager-server
