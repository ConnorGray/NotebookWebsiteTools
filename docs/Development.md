# Development

This document contains information useful to anyone who wants to contribute to
the development of NotebookWebsiteTools. People who only wish to use
NotebookWebsiteTools to author a website do not need to read this document.

### Command quick reference


#### Build and install the `NotebookWebsiteTools` paclet

```shell
$ cargo make install
```

#### Build the main `NotebookWebsiteTools` paclet archive and install manually

```shell
$ cargo make paclet
```

The resulting build files and final .paclet archive file will be located in the
`build` subdirectory of the repository.

Use [`wolfram-cli`](https://github.com/ConnorGray/wolfram-cli) to install the
built paclet:

```shell
$ wolfram-cli paclet install build/ConnorGray__NotebookWebsiteTools-X.X.X.paclet
```

#### Build the `Examples` notebook website

Using the CLI:

```shell
$ wolfram-cli notebook-website build ./Examples
```

#### Run the test suite

Using the CLI:

```shell
$ wolfram-cli paclet test ./paclets/NotebookWebsiteTools Tests
```



## Setup

Developing NotebookWebsiteTools requires that the following software be
installed:

* The Rust programming language, specifically the `cargo` build tool.

* The Wolfram programming language.

  The Community Edition of [Wolfram Engine](https://www.wolfram.com/engine/) is
  sufficient to execute builds of Notebook Websites, but does not provide the
  Wolfram notebook GUI interface necessary to author website notebooks (for that,
  Mathematica is required).

* The [`cargo-make`](https://crates.io/crates/cargo-make) utility, used to run
  development command-line tasks. `cargo-make` can be installed using `cargo`:

  ```shell
  $ cargo install cargo-make
  ```

## Updating Feature Overview Examples

*These instructions are currently only executable by me (Connor Gray), as they*
*require access to my Wolfram Cloud account.*

**Note:** Before following these steps, ensure that `Examples/build/` is
up-to-date by following the paclet build and Examples build instructions above.

Delete the existing feature-overview.html page:

```wolfram
DeleteDirectory[
    CloudObject["Examples/NotebookWebsiteTools"],
    DeleteContents -> True
]
```

Publish the updated feature-overview.html page:

```wolfram
CopyDirectory[
    "~/Dev/Github/ConnorGray/NotebookWebsiteTools/Examples/build",
    CloudObject["Examples/NotebookWebsiteTools", Permissions -> "Public"]
]
```

Update the rasterized view of [feature-overview.nb](./images/feature-overview.nb.png):

```wolfram
Export[
    "~/Dev/GitHub/ConnorGray/NotebookWebsiteTools/docs/images/feature-overview.nb.png",
    Rasterize[NotebookOpen[
        "~/Dev/GitHub/ConnorGray/NotebookWebsiteTools/Examples/Content/feature-overview.nb"
    ]]
]
```
