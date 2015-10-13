## autofix-ghc

Yet another warning auto-fixer for GHC compilations with the following properties:

* Build system independent - can work with anything that emits GHC to stdout.
* Does not try to compile or fully parse the HS file. It doesn't depend on
  any Haskell parsing library (see [End note](#end-note) about this).
* Can show a unidiff of proposed changes (-d).
* Can fix in place (-f).
* It's very fast.

Also, it is very limited. Currently only fixes a single type of warning, but
an annoying one about redundant imports:

```
src/Main.hs:34:1: Warning:
    The import of ‘forM_, forM’ from module ‘Control.Monad’ is redundant
```

## Quick HowTo

If you fully trust the thing:

```
cabal clean
cabal build 2> warnings.txt
cat warnings.txt | autofix-ghc --fix
```

## A more elaborate HowTo

Support you saved the above warning to `warnings.txt` by redirecting
cabal output somehow.


```shell
$ cat warnings.txt | autofix-ghc --fix

autofix-ghc can fix: src/Main.hs
```
You can see a diff:

```shell
$ cat warnings.txt | autofix-ghc --diff

autofix-ghc can fix: src/Lib/Git.hs
diff -urN src/Main.hs src/Main.hs
--- src/Main.hs
+++ src/Main.hs
@@ -31,7 +31,7 @@
   ) where
 
 ------------------------------------------------------------------------------------
-import           Control.Monad             (forM_, forM, when)
+import           Control.Monad             (when)
 import           Control.Lens.Operators    ((&))
 import           Control.Monad.IO.Class    (liftIO)
 import           Control.Monad.Catch       (MonadMask)
```

And you can fix it:

```shell
$ cat warnings.txt | autofix-ghc --fix

autofix-ghc fixed: src/Main.hs
```

## End note

I wrote this in a day for teaching myself `attoparsec`. If you find this
useful and want to improve it, pull requests are welcomed.
