SWAT: Simple Web-Assembly Transpiler

### Goals:
- very portable (`no_std`), kernel friendly
- small number of dependencies
- simple to maintain

### Non-goals:
- event-driven / streaming parser approach
- multi-pass optimisations

### Todo:
- [x] WASM Parsing (Vector/SIMD instructions not yet supported)
- [ ] Aarch64 transpilation
- [ ] x86_64 transpilation
- [ ] rv64 transpilation
