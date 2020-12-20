let
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-unstable-2020-12-20";
    url =
      "https://github.com/nixos/nixpkgs/archive/134a1ad3751f22eba9a1a6ae41fde34d98d168b8.tar.gz";
    sha256 = "1inja49kq421yb32hwhm36cckiikms86lsjsp4sx0cxyksqx2wdn";
  };
in (import nixpkgs) { }
