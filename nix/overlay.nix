final: prev:
with prev; {
  ocamlPackages = final.ocaml-ng.ocamlPackages_5_2;

  ocaml-ng =
    ocaml-ng
    // (with ocaml-ng; {
      ocamlPackages_5_2 = ocamlPackages_5_2.overrideScope (
        _: prev:
          with prev; {
            grace = buildDunePackage rec {
              pname = "grace";
              version = "0.2.0";

              minimalOCamlVersion = "4.14";

              src = fetchurl {
                url = "https://github.com/johnyob/grace/releases/download/${version}/grace-${version}.tbz";
                hash = "sha256-gh31SILJJT6safR7zzpx/9xhx3/a5CWHwyqtpbVs/q4=";
              };
              propagatedBuildInputs = [core ppx_jane fmt dedent iter core_unix uutf ppx_optcomp];
            };
          }
      );
    });
}
