# NotebookWebsiteTools

A static site generator based on Wolfram Notebooks.

## Examples

[feature-overview.nb](./Examples/Content/feature-overview.nb) ([**image**](./docs/images/feature-overview.nb.png.md))
is a Wolfram Notebook that contains an overview of features supported by NotebookWebsiteTools.

[**feature-overview.html**](https://www.wolframcloud.com/obj/connorg/Examples/NotebookWebsiteTools/feature-overview.html)
is the HTML rendered version of feature-overview.nb.

### Building the Examples website

The [./Examples](./Examples/) subdirectory of this repository is a website in the
layout expected by `NotebookWebsiteBuild[..]`.

To build the Examples website, first load NotebookWebsiteTools:

```wolfram
Needs["ConnorGray`NotebookWebsiteTools`"]
```

Then use `NotebookWebsiteBuild` to build:

```wolfram
NotebookWebsiteBuild["/path/to/NotebookWebsiteTools/Examples"]
```

The built website assets will be placed in the `Examples/build` directory.

### Command-Line Interface

A command-line interface to NotebookWebsiteTools functionality is available via
the unofficial [`wolfram-cli`](https://github.com/ConnorGray/wolfram-cli) tool.

If `wolfram-cli` is available, and the `NotebookWebsiteTools` paclet is
installed, then a notebook website can be built using:

```shell
$ wolfram-cli notebook-website build [WEBSITE_DIR]
```

Include documents with the `Draft` status flag:

```shell
$ wolfram-cli notebook-website build [WEBSITE_DIR] --drafts
```

## Contributing

See [**Development.md**](./docs/Development.md) for instructions on how to
develop NotebookWebsiteTools.

## Credits

* [rust-logo-blk.svg](./paclets/NotebookWebsiteTools/Assets/web_assets/rust-logo-blk.svg)
  and [rust-logo-32x32-blk.png](./paclets/NotebookWebsiteTools/Assets/Icons/rust-logo-32x32-blk.png)
  were sourced unchanged from the rust-lang/rust-artwork GitHub repository, and are licensed
  under the [Creative Commons Attribution license (CC-BY)](https://creativecommons.org/licenses/by/4.0/).
