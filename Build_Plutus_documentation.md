### Building Plutus documentation
(Reference: [community docs](https://docs.plutus-community.com/docs/setup/buildDocumentation.html))

In order to build the Plutus documentation in a local web server with js enabled, we just need to follow the next simple steps.

1. Go to your plutus folder
```bash
_@_:some/path$ cd /path/to/plutus/plutus
```
2. Enter nix shell
```bash
_@_:path/to/plutus/plutus$ nix-shell
```
3. (Optional) If you have not before, build plutus haddock
```bash
_@_:path/to/plutus/plutus$ nix-build -A plutus-playground.haddock
```
4. Once it loads, go up one level and create a new directory
```bash
_@_:path/to/plutus/plutus$ cd ..
_@_:path/to/plutus$ mkdir haddock-web
```
5. Then `cd` into `haddock-web` and create and compile a main.hs file, as described in the reference
6. Finally, from this same directory, launch the server
```bash
_@_:haddock-web$ ./plutus-haddock -s /path/to/plutus/plutus/result
```
7. You will see something like
```bash
running plutus documentation in http://localhost:8081/plutus-haddock/index.html
```
so go to the browser and enjoy. By pressing `s` in the browser you will open a search bar.
