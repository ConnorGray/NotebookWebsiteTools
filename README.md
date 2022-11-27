# NotebookWebsiteTools

A static site generator based on Wolfram Notebooks.

## Examples

[kitchen-sink.nb](./Examples/Content/kitchen-sink.nb) ([**image**](./docs/images/kitchen-sink.nb.png.md))
is a Wolfram Notebook that contains an overview of features supported by NotebookWebsiteTools.

[**kitchen-sink.html**](https://www.wolframcloud.com/obj/connorg/Examples/NotebookWebsiteTools/kitchen-sink.html)
is the HTML rendered version of kitchen-sink.nb.

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

### Contributing

See [**Development.md**](./docs/Development.md) for instructions on how to
develop NotebookWebsiteTools.
