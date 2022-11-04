# clong

A wrapper for libclang and a generator that can turn c header files into clojure apis.

Currently, there is only a generator for clojure jna, but support for other ffi libs is likely.

## Usage

### Setup

Parsing header files requires libclang to be available on the `jna.library.path`. You can set the `jna.library.path` with by adding the following to an alias:
```
:jvm-opts ["-Djna.library.path=/my/lib/path"]
```

Note: api wrapper generation does not require libclang.

#### Obtaining libclang

It's harder than it should be to acquire libclang. If you're on linux, there might be a package. I couldn't find one on for mac osx, but it was easy to build locally from https://github.com/llvm/llvm-project. I built the project with the following configuration:

```
mkdir build
cd build
cmake -G 'Unix Makefiles' -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=`pwd`/out -DLLVM_ENABLE_PROJECTS="clang" ../llvm
make
make install
```

### Parsing

`com.phronemophobic.clong.clang/parse` takes a header filename and the command line args to pass to clang. The most common command line args are `"-I/my/header/path"`. If you built clang locally, then you may also need to add system paths. You can see the system paths that you may need to by running `clang -### empty-file.h` from the command line.

`parse` returns a CXCursor. Further processing can be done via the raw api in `com.phronemophobic.clong.clang.jna.raw`. For basic usage, just use:
```clojure
(->> (parse "myheader.h" [])
     get-children
     (map cursor-info))
```

### Generating APIs

Below is how clong can be used to generate a wrapper for libz ([full example](https://github.com/phronmophobic/clong/tree/master/examples/libz)):

```clojure
(def ^:no-doc libz
  (com.sun.jna.NativeLibrary/getInstance "z"))

(def api (clong/easy-api "/opt/local/include/zlib.h"))

(gen/def-api libz api)

(zlibVersion) ;; "1.2.11"

(def source (.getBytes "clong!")) 

(def dest (byte-array 255))
(def dest-size* (doto (LongByReference.)
                  (.setValue (alength dest))))

(compress  dest dest-size* source (alength source)) ;; 0

(.getValue dest-size*) ;; 14

(def dest2 (byte-array (alength source)))
(def dest2-size* (doto (LongByReference.)
                   (.setValue (alength dest2))))
(uncompress dest2 dest2-size* dest (.getValue dest-size*)) ;; 0

(String. dest2) ;; "clong!"
```

The basic idea is to generate a description of the api with `easy-api` and generate the required structs, functions, and enums with `gen-api`.

## Future Work

- Improve documentation.
- Add support for #define values.
- Add support for other ffi libraries besides jna.
- Improve support for references. Currently, there is support for structs by reference, but not for references to structs by reference.
- Implement clojure data interfaces over structs.

## License

Copyright © 2022 Adrian

Distributed under the Eclipse Public License version 1.0.
