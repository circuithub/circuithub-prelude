{ mkDerivation, aeson, aeson-pretty, base, bifunctors, bytestring
, classy-prelude, comparable-key, containers, either, errors
, hashable, mono-traversable, safe, semigroups, split, stdenv, text
, time, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "circuithub-prelude";
  version = "0.0.28";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty base bifunctors bytestring classy-prelude
    comparable-key containers either errors hashable mono-traversable
    safe semigroups split text time unordered-containers utf8-string
    vector
  ];
  homepage = "https://github.com/circuithub/circuithub-prelude";
  description = "CircuitHub's Prelude that builds on top of the ClassyPrelude";
  license = stdenv.lib.licenses.mit;
}
