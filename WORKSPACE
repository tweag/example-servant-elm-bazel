workspace(name = "example-servant-elm")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "build_bazel_rules_nodejs",
    sha256 = "dcc55f810142b6cf46a44d0180a5a7fb923c04a5061e2e8d8eb05ccccc60864b",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/5.8.0/rules_nodejs-5.8.0.tar.gz"],
)

gazelle_cabal_version = "34a41345973cdffa29bf4d5d349439e86fc30ed3"

http_archive(
    name = "io_tweag_gazelle_cabal",
    sha256 = "953f18f0df03082a543f8e73e69c9a75ed48ab09bd455438bfc2c48983927c47",
    strip_prefix = "gazelle_cabal-%s" % gazelle_cabal_version,
    url = "https://github.com/tweag/gazelle_cabal/archive/%s.zip" % gazelle_cabal_version,
)

##########################
# bazel_skylib preamble
##########################

http_archive(
    name = "bazel_skylib",
    sha256 = "c6966ec828da198c5d9adbaa94c05e3a1c7f21bd012a0b29ba8ddbccb2c93b0d",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.1.1/bazel-skylib-1.1.1.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.1.1/bazel-skylib-1.1.1.tar.gz",
    ],
)

load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")

bazel_skylib_workspace()

##########################
# rules_nixpkgs preamble
##########################

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "dc31f6d7e028354500c839a0fe8e2db377518ee7f916542f9cca2b1f13c751f6",
    strip_prefix = "rules_nixpkgs-6563f78c9e82be839343c0759ed4b9d1e3588b82",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/6563f78c9e82be839343c0759ed4b9d1e3588b82.tar.gz"],
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")
rules_nixpkgs_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
    "nixpkgs_nodejs_configure_platforms",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_python_configure(repository = "@nixpkgs")

##########################
# rules_haskell preamble
##########################
http_archive(
    name = "rules_cc",
    sha256 = "4dccbfd22c0def164c8f47458bd50e0c7148f3d92002cdb459c2a96a68498241",
    urls = ["https://github.com/bazelbuild/rules_cc/releases/download/0.0.1/rules_cc-0.0.1.tar.gz"],
)

http_archive(
    name = "rules_haskell",
    sha256 = "2a07b55c30e526c07138c717b0343a07649e27008a873f2508ffab3074f3d4f3",
    strip_prefix = "rules_haskell-0.16",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.16.tar.gz",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

rules_haskell_dependencies()

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc8107",
    compiler_flags = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "8.10.7",
)

######################################
# gazelle_cabal & haskell deps
######################################

load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")

gazelle_cabal_dependencies()

stack_snapshot(
    name = "stackage",
    components = {
        "hspec-discover": [
            "lib",
            "exe:hspec-discover",
        ],
    },
    extra_deps = {"zlib": ["@zlib.dev//:zlib"]},
    packages = [
        "aeson",  #keep
        "base",
        "containers",
        "elm-bridge",
        "hspec",
        "hspec-discover",
        "http-client",
        "http-types",
        "path",  #keep
        "path-io",  #keep
        "servant",
        "servant-client",
        "servant-elm",
        "servant-server",
        "text",
        "transformers",
        "wai",
        "wai-app-static",
        "warp",
        "wai-cors", # keep
    ],
    snapshot = "lts-18.12",
)

###############
# rules_go preamble
###############

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "69de5c704a05ff37862f7e0f5534d4f479418afc21806c887db544a316f3cb6b",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
    ],
)

# TODO: go executable (toolchain) can taken either directly from nix buildInputs or from
# nixpkgs_go_configure
#load(
#    "@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl",
#    "nixpkgs_go_configure",
#)
# nixpkgs_go_configure(repository = "@nixpkgs")

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains(version = "host")

####################
# gazelle preamble
####################

http_archive(
    name = "bazel_gazelle",
    sha256 = "62ca106be173579c0a167deb23358fdfe71ffa1e4cfdddf5582af26520f1c66f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

gazelle_dependencies()

####################
# zlib
####################

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
filegroup(
   name = "include",
   srcs = glob(["include/*.h"]),
   visibility = ["//visibility:public"],
)
cc_library(
   name = "zlib",
   srcs = ["@nixpkgs_zlib//:lib"],
   hdrs = [":include"],
   strip_include_prefix = "include",
   visibility = ["//visibility:public"],
   # This rule only bundles headers and a library and doesn't compile or link by itself.
   # We set linkstatic = 1 to quiet to quiet the following warning:
   #
   #   in linkstatic attribute of cc_library rule @zlib.dev//:zlib:
   #   setting 'linkstatic=1' is recommended if there are no object files.
   #
   linkstatic = 1,
)
""",
    repository = "@nixpkgs",
)

####################
# elm
####################

# rules_haskell dependency:
http_archive(
    name = "io_bazel_rules_webtesting",
    sha256 = "3141fb50ac504d0ecf1a159b86e232ebd88547c6c5958c4b0e6b319aaa0871aa",
    strip_prefix = "rules_webtesting-1460fa2b9a4307765cdf4989019a92ed6e65e51f",
    urls = [
        "https://github.com/bazelbuild/rules_webtesting/archive/1460fa2b9a4307765cdf4989019a92ed6e65e51f.tar.gz",
    ],
)

load("@io_bazel_rules_webtesting//web:repositories.bzl", "web_test_repositories")

web_test_repositories()
###############################################################

load("@build_bazel_rules_nodejs//:repositories.bzl", "build_bazel_rules_nodejs_dependencies")

build_bazel_rules_nodejs_dependencies()

nixpkgs_nodejs_configure_platforms(
    repository = "@nixpkgs",
    name = "nixpkgs_nodejs"
)

load("@build_bazel_rules_nodejs//:index.bzl", "npm_install")
npm_install(
    name = "npm",
    node_repository = "nixpkgs_nodejs",
    exports_directories_only = True,
    package_json = "//:package.json",
    package_lock_json = "//:package-lock.json",
    args = ["--ignore-scripts"],
)

rules_elm_version = "944cd84f4e0dd101e9d632ca6fb2ceee15e8509a"

http_archive(
    name = "com_github_edschouten_rules_elm",
    sha256 = "8a917d768c0aad6f73357e0e4ac00050a8ca1d8949aa0c367361d840892bf681",
    strip_prefix = "rules_elm-%s" % rules_elm_version,
    urls = [ "https://github.com/kczulko/rules_elm/archive/%s.tar.gz" % rules_elm_version ],
)

nixpkgs_package(
    name = "nodejs",
    attribute_path = "nodejs",
    repository = "@nixpkgs",
)

load("@com_github_edschouten_rules_elm//elm:deps.bzl", "elm_register_toolchains")

elm_register_toolchains()

http_archive(
    name = "aherrmann_example_servant_elm",
    sha256 = "40d300848cb1094f290ce5a05836054bbd488a61b3264fca50a69c8603aa13cd",
    strip_prefix = "example-servant-elm-f567cebb02f5bd18197b6203a65d3d3248fc8d36",
    urls = [
        "https://github.com/aherrmann/example-servant-elm/archive/f567cebb02f5bd18197b6203a65d3d3248fc8d36.tar.gz",
    ],
)

load("@aherrmann_example_servant_elm//:elm_repositories.bzl", "elm_repositories")
elm_repositories()

