# clong

A wrapper for libclang and a generator that can turn c header files into clojure apis.

Currently, there is only a generator for jna, but support for other ffi libs is likely.

## Rationale

Writing wrappers for c libraries is tedious and error prone. The goal of clong is to streamline the process of creating wrappers for c libraries. It is a non-goal to make the process 100% automatic. However, it should be possible to do 80-90% of the work and provide tools that are useful for building higher level APIs.

## Usage

### Setup

Parsing header files requires libclang to be available on the `jna.library.path`. You can set the `jna.library.path` by adding the following to an alias:
```
:jvm-opts ["-Djna.library.path=/my/lib/path"]
```

Note: libclang is only required for parsing. If an api specification has been pre-generated, then the api can be generated without libclang as a dependency.

#### Obtaining libclang

It's harder than it should be to acquire libclang. If you're on linux, there might be a package (I haven't tested, but the internet seems to indicate `apt install build-essentials libclang-dev clang` should work). I couldn't find a package for mac osx, but it was easy to build locally from https://github.com/llvm/llvm-project. I built the project with the following configuration:

```
mkdir build
cd build
cmake -G 'Unix Makefiles' -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=`pwd`/out -DLLVM_ENABLE_PROJECTS="clang" ../llvm
make
make install
```

### Parsing

`com.phronemophobic.clong.clang/parse` takes a header filename and the command line args to pass to clang. The most common command line args are `"-I/my/header/path"`. If you built clang locally, then you may also need to add system paths. You can see the system paths that you may by running `clang -### empty-file.h` from the command line.

`parse` returns a CXCursor. Further processing can be done via the raw api in `com.phronemophobic.clong.clang.jna.raw`. For basic usage, just use:
```clojure
(->> (parse "myheader.h" [])
     get-children
     (map cursor-info))
```

### Generating APIs

Below is how clong can be used to generate a wrapper for libz ([full example](https://github.com/phronmophobic/clong/tree/master/examples/libz)):

```clojure
(def libz
  (com.sun.jna.NativeLibrary/getInstance "z"))

(def api (clong/easy-api "/opt/local/include/zlib.h"))

(gen/def-api libz api)

(zlibVersion) ;; "1.2.11"

(def source "clong!")

(def dest (byte-array 255))
(def dest-size* (doto (LongByReference.)
                  (.setValue (alength dest))))

(compress  dest dest-size* source (count source)) ;; 0

(.getValue dest-size*) ;; 14

(def dest2 (byte-array (count source)))
(def dest2-size* (doto (LongByReference.)
                   (.setValue (alength dest2))))
(uncompress dest2 dest2-size* dest (.getValue dest-size*)) ;; 0

(String. dest2) ;; "clong!"
```

The basic idea is to generate a description of the api with `easy-api` and generate the required structs, functions, and enums with `def-api`.

### Examples

Examples can be found in the [examples directory](https://github.com/phronmophobic/clong/tree/main/examples).

- [libz](https://github.com/phronmophobic/clong/tree/main/examples/libz)
- [freetype](https://github.com/phronmophobic/clong/tree/main/examples/freetype)

For a more complicated example, clong's [clang interface](https://github.com/phronmophobic/clong/blob/main/src/com/phronemophobic/clong/clang/jna/raw.clj) is [generated](https://github.com/phronmophobic/clong/blob/main/src/com/phronemophobic/clong/clang.clj#L546) by clong itself.

Additionally, clong was successfully able to generate a complete wrapper for the libav* libraries, ([source](https://github.com/phronmophobic/clj-media/blob/main/src/com/phronemophobic/clj_media/audio.clj#L138)).

### Tips

- Structs will not have any fields reported if _any_ of the struct's fields has an undetermined size. If fields for a struct are missing, you may need to include more headers or make sure the system's standard paths are included (try `clang -### empty-file.h` to see what system paths might be missing).
- Clong doesn't offer any special support for automatic memory management of ffi data.
- You can include additional headers using the `-include` argument when parsing
- Parsing headers can be slow and you probably don't want to ship headers and libclang with your wrapper library. Generate the wrapper ahead of time and save the result as a edn resource that can be loaded. In the future, support for AOT may be added to better support this.
- JNA has issues with writing to StructureByReference fields. In some cases, it's useful to override structure fields to be raw pointers.

## Future Work

- Improve documentation.
- Add support for #define values.
- Add support for other ffi libraries besides jna.
- Improve support for references. Currently, there is support for structs by reference, but not for references to structs by reference.
- Implement clojure data interfaces over structs.
- Support AOT of wrappers.
## License

Copyright © 2022 Adrian

Distributed under the Eclipse Public License version 1.0.
