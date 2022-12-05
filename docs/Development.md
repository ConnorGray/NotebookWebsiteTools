# Development

This document contains information useful to anyone who wants to contribute to
the development of NotebookWebsiteTools. People who only wish to use
NotebookWebsiteTools to author a website do not need to read this document.

### Command quick reference

**Build the main `NotebookWebsiteTools` paclet archive:**

```shell
$ cargo make paclet
```

The resulting build files and final .paclet archive file will be located in the
`build` subdirectory of the repository.

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

## Updating Kitchen Sink Examples

*These instructions are currently only executable by me (Connor Gray), as they*
*require access to my Wolfram Cloud account.*

Delete the existing kitchen-sink.html page:

```wolfram
DeleteFile[CloudObject["Examples/NotebookWebsiteTools/kitchen-sink.html"]]
```

Publish the updated kitchen-sink.html page:

```wolfram
CopyFile[
    "~/Dev/GitHub/ConnorGray/NotebookWebsiteTools/Examples/build/kitchen-sink.html",
    CloudObject["Examples/NotebookWebsiteTools/kitchen-sink.html", Permissions -> "Public"]
]
CopyFile[
    "~/Dev/GitHub/ConnorGray/NotebookWebsiteTools/Examples/build/web_assets/notebook-website-default.css",
    CloudObject["Examples/NotebookWebsiteTools/web_assets/notebook-website-default.css", Permissions -> "Public"]
]
```

Update the rasterized view of [kitchen-sink.nb](./images/kitchen-sink.nb.png):

```wolfram
Export[
    "~/Dev/GitHub/ConnorGray/NotebookWebsiteTools/docs/images/kitchen-sink.nb.png",
    Rasterize[NotebookOpen[
        "~/Dev/GitHub/ConnorGray/NotebookWebsiteTools/Examples/Content/kitchen-sink.nb"
    ]]
]
```
