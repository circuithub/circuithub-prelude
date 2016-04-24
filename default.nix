{ mkDerivation, aeson, aeson-pretty, base, bifunctors, bytestring
, classy-prelude, comparable-key, containers, either, errors
, hashable, mono-traversable, safe, semigroups, split, stdenv, text
, time, unordered-containers, utf8-string, vector

, lib, dist ? null
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

  # We will output to $out as normal, but also to $dist
  outputs = [ "out" "dist" ];

  # Before we can build, we restore the previous dist/ directory
  preConfigure = ''
    # Before we do anything, capture the MD5 sums of all source files.
    # We will compare against this in subsequent builds.
    mkdir -p $dist
    find . -type f | xargs md5sum | sort > $dist/MD5SUMS
  '' + (lib.optionalString (dist != null) ''
    # Restore the old dist/ directory, with its MD5SUMS
    mkdir -p dist
    cp -r ${dist}/* dist/   # */
    chmod +w -R dist/
    touch dist/MD5SUMS

    # Touch any files whose MD5SUM has changed since the last build
    join $dist/MD5SUMS dist/MD5SUMS -v 1 | cut -d' ' -f 2 | while read filename; do
      echo "$filename" has changed
      touch "$filename" || true
    done

    # Touch all dist/ files to be 2 hours in the past.
    # Note that source code will be last modified in 1970 *by default*
    # but changed to the current time by the loop above.
    mkdir -p dist/build
    find dist/build -print | while read filename; do
        touch -d "$(date -R -r "$filename") - 2 hours" "$filename"
    done
  '');

  # We need to store dist/ in the $dist output.
  preInstall = "cp -R dist/build $dist/";
}
