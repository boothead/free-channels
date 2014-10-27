{ cabal, free, mtl, transformers }:

cabal.mkDerivation (self: {
  pname = "free-channels";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ free mtl transformers ];
  meta = {
    homepage = "https://github.com/boothead/free-channels";
    description = "Channels in Free Monads";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
