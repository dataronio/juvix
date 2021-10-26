# Documentation for the Juvix Language.


This manual has been prepared using ReStructured Text and the [Sphinx Documentation Generator](https://www.sphinx-doc.org) for future inclusion on [Read The Docs](https://readthedocs.org).

## Dependencies

To build the manual, the following dependencies must be met. We assume that you have standard build automation tools already installed i.e. `make`.

### Sphinx-Doc

Python should be installed by default on most systems.
Sphinx can be installed either through your hosts package manager or using pip/easy_install.
Recommended way is to use virtual environment for building documentation.

The ReadTheDocs theme can be installed in virtual environment using pip as follows:

```sh
python3 -m venv juvixdocs_venv
source juvixdocs_venv/bin/activate
pip install --upgrade pip
pip install sphinx_rtd_theme
pip install myst_parser
pip install sphinx_proof
pip install sphinxcontrib-tikz
```

### LaTeX

LaTeX can be installed either using your systems package manager or directly from TeXLive.


## Build Instructions

```sh
cd docs
make html
make latexpdf
```

### Layout

- The [language reference](./reference) section holds the official language reference.
  Please note that the language reference is still a work in progress.

- The [contribute](./CONTRIBUTING.md) file holds all information needed to contribute
  to Juvix.

- For Documentation about the codebase itself see the [code layout docs](./Code) folder.
  + There are three files for each section of the codebase.

#### Informal Discussions

These folders cover various discussions and more informal plans for the
Juvix programming language.

- The [architecture](./Architecture) folder holds various non-obvious
  architecture choices.

- The [frontend](./Frontend) folder contains various ideas and plans
  for the front end syntax of the Juvix programming language.

#### Miscellaneous

- The [Bohm](./Bohm) holds an EBNF file and the corresponding railroad diagram
  for an intermediate layer for interaction nets in the Juvix Interpreter.

