# Configuration file for the Sphinx documentation builder.
#
# Juvix Manual documentation build configuration file
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
import sphinx_rtd_theme
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

# General information about the project.
project = 'Juvix'
copyright = '2021, Heliax'
author = 'Heliax Dev Team'

# The short X.Y version.
version = '0.0'

# The full version, including alpha/beta/rc tags
release = '0.0'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    'sphinx.ext.todo',
#    'sphinx.ext.pngmath', # imgmath is not supported on readthedocs.
    'sphinx.ext.ifconfig',
    "sphinx_rtd_theme",
    "myst_parser",
    "sphinx_proof",
    "sphinxcontrib.tikz",
    "sphinxcontrib.bibtex"
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The master toctree document.
master_doc = 'index'

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
# # Read The Docs Themes specific settings
html_theme = 'sphinx_rtd_theme'
html_theme_options = {
    'display_version': True,
    'prev_next_buttons_location': 'bottom'
}




# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
# html_static_path = ['_static']
html_static_path = []
# Output file base name for HTML help builder.
htmlhelp_basename = 'JuvixManualdoc'

# -- Options for LaTeX output ---------------------------------------------

latex_title_page = r'''
\begin{titlepage}
    \vspace*{\fill}
    \begin{center}
        \vspace{1cm}
        {\huge\sffamily\bfseries \makeatletter\@title\makeatother\par}
        \vspace{1cm}
        {\Large Version \version\par}
    \end{center}
    \vspace*{\fill}
\end{titlepage}
'''

latex_elements = {
# The paper size ('letterpaper' or 'a4paper').
'papersize': 'a4paper',

'fontpkg': '',
'inputenc': '',
'utf8extra': '',
'releasename': 'Version',

# The font size ('10pt', '11pt' or '12pt').
'pointsize': '10pt',

# Additional stuff for the LaTeX preamble.
'preamble': r'''
\usepackage{lmodern}
\usepackage[T1]{fontenc}
\usepackage[utf8x]{inputenc}
\usepackage{titlesec}
\usepackage{tikz}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{bussproofs}
\usepackage{amsmath}
\usepackage{cmll}
\usepackage{microtype}
\usepackage{float}
\floatstyle{boxed}
\restylefloat{figure}
\usepackage{subcaption}
\usepackage{tikz}
\usetikzlibrary{arrows,automata}
\newtheorem{theorem}{Theorem}
\usepackage[many]{tcolorbox}
\tcbuselibrary{theorems}

%
\usepackage{fancyhdr}
\fancypagestyle{plain}{%
  \renewcommand{\headrulewidth}{0pt}%
  \fancyhf{}%
  \fancyfoot[C]{\textsf{\thepage}}
}
\pagestyle{fancy}
\fancyhf{}
\fancyhead[LE,RO]{\textsf{\bfseries{v\version}}}
\fancyhead[LO,RE]{\textsf{\bfseries\leftmark}}
%\fancyhead[LO]{\textsf{\bfseries{\leftmark}}}
%\fancyhead[RO]{\textsf{\bfseries{v\version}}}
%\fancyhead[RE]{\textsf{\bfseries{\leftmark}}}
%\fancyhead[LE]{\textsf{\bfseries{v\version}}}
\fancyfoot[C]{\textsf{\thepage}}
\renewcommand{\footrulewidth}{0pt}
\renewcommand{\headrulewidth}{0pt}
%
\usepackage[font={small,it}]{caption}
\titleformat{\section}
  {\normalfont\sffamily\Large\bfseries\color{black}}
  {\thesection}{1em}{}
\titleformat{\subsection}
  {\sffamily\large\bfseries\color{black}}
  {\thesubsection}{1em}{}
\titleformat{\subsubsection}
  {\sffamily\normalsize\bfseries\color{black}}
  {\thesubsubsection}{1em}{}
\titleformat{\paragraph}{\normalfont\normalsize\slshape}{\theparagraph}{1em}{}
\setlength{\parskip}{1em}
%
\hypersetup{colorlinks = false}
\definecolor{VerbatimBorderColor}{rgb}{1,1,1}
\DeclareUnicodeCharacter{"2237}{}


% general color defintions *****************************************************
\definecolor{ceruleanblue}{rgb}{0.16, 0.32, 0.75}
\definecolor{mediumpersianblue}{rgb}{0.0, 0.4, 0.65}
\definecolor{palered-violet}{rgb}{0.86, 0.44, 0.58}
\definecolor{bondiblue}{rgb}{0.0, 0.58, 0.71}
\definecolor{antiquefuchsia}{rgb}{0.57, 0.36, 0.51}
\definecolor{mypink3}{cmyk}{0, 0.7808, 0.4429, 0.1412}
\definecolor{blush}{rgb}{0.87, 0.36, 0.51}
\definecolor{candypink}{rgb}{0.89, 0.44, 0.48}

\definecolor{lavenderpurple}{rgb}{0.59, 0.48, 0.71}
\definecolor{cerise}{rgb}{0.87, 0.19, 0.39}
\definecolor{brickred}{rgb}{0.8, 0.25, 0.33}
\definecolor{cadetblue}{rgb}{0.37, 0.62, 0.63}
\definecolor{brilliantrose}{rgb}{1.0, 0.33, 0.64}

% Theorem Boxes and other special colors ***************************************

% currently defined to mediumpersianblue
% used for all definitions
\definecolor{theorems}{rgb}{0.0, 0.4, 0.65}

% currently defined to be antiquefuchsia
% used for all boxes that are the first subox inside a proof
\definecolor{inner-box-1}{rgb}{0.57, 0.36, 0.51}

% currently defined to be blush
% used for all boxes that are the second subox inside a proof
\definecolor{inner-box-2}{rgb}{0.87, 0.36, 0.51}

% currenlty defined to be brickred
% used for all defined words
\definecolor{definitions}{rgb}{0.8, 0.25, 0.33}


\newtcbtheorem[number within=section]{Lemma}{Lemma}{colframe=theorems, breakable}{Lm}
\newtcbtheorem[number within=section]{Theorem}{Theorem}{colframe=theorems, breakable}{Th}
\newtcbtheorem[number within=section]{Definition}{Definition}{colframe=theorems, breakable}{De}
''',

'maketitle': latex_title_page,
'tableofcontents': "\\tableofcontents"
# Latex figure (float) alignment
#'figure_align': 'htbp',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
# TODO: Include other folders too
latex_documents = [
   ('getting-started/index',  'juvix-documentation.tex',  u'The Juvix Documentation',    u'The Juvix Community', 'howto'),
]


latex_show_pagerefs = True
latex_show_url = 'footnote'

# The name of an image file (relative to this directory) to place at the top of
# the title page.
# latex_logo = 'img/juvix-logo.png'

# For "manual" documents, if this is true, then toplevel headings are parts,
# not chapters.
#latex_use_parts = True

# If true, show page references after internal links.
#latex_show_pagerefs = False

# If true, show URL addresses after external links.
#latex_show_urls = False

# Documents to append as an appendix to all manuals.
#latex_appendices = []

# If false, no module index is generated.
#latex_domain_indices = True

bibtex_bibfiles = ['refs.bib']

# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'juvixmanual', u'Juvix Manual Documentation',
     [author], 1)
]

# If true, show URL addresses after external links.
#man_show_urls = False


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  (master_doc, 'JuvixManual', u'Juvix Manual Documentation',
   author, 'JuvixManual', 'One line description of project.',
   'Miscellaneous'),
]

# Documents to append as an appendix to all manuals.
#texinfo_appendices = []

# If false, no module index is generated.
#texinfo_domain_indices = True

# How to display URL addresses: 'footnote', 'no', or 'inline'.
#texinfo_show_urls = 'footnote'

# If true, do not generate a @detailmenu in the "Top" node's menu.
#texinfo_no_detailmenu = False


# -- Options for Epub output ----------------------------------------------

# Bibliographic Dublin Core info.
epub_title = project
epub_author = author
epub_publisher = author
epub_copyright = copyright

# The basename for the epub file. It defaults to the project name.
#epub_basename = project

# The HTML theme for the epub output. Since the default themes are not optimized
# for small screen space, using the same theme for HTML and epub output is
# usually not wise. This defaults to 'epub', a theme designed to save visual
# space.
#epub_theme = 'epub'

# The language of the text. It defaults to the language option
# or 'en' if the language is not set.
#epub_language = ''

# The scheme of the identifier. Typical schemes are ISBN or URL.
#epub_scheme = ''

# The unique identifier of the text. This can be a ISBN number
# or the project homepage.
#epub_identifier = ''

# A unique identification for the text.
#epub_uid = ''

# A tuple containing the cover image and cover page html template filenames.
#epub_cover = ()

# A sequence of (type, uri, title) tuples for the guide element of content.opf.
#epub_guide = ()

# HTML files that should be inserted before the pages created by sphinx.
# The format is a list of tuples containing the path and title.
#epub_pre_files = []

# HTML files shat should be inserted after the pages created by sphinx.
# The format is a list of tuples containing the path and title.
#epub_post_files = []

# A list of files that should not be packed into the epub file.
epub_exclude_files = ['search.html']

# The depth of the table of contents in toc.ncx.
#epub_tocdepth = 3

# Allow duplicate toc entries.
#epub_tocdup = True

# Choose between 'default' and 'includehidden'.
#epub_tocscope = 'default'

# Fix unsupported image types using the Pillow.
#epub_fix_images = False

# Scale large images.
#epub_max_image_width = 0

# How to display URL addresses: 'footnote', 'no', or 'inline'.
#epub_show_urls = 'inline'

# If false, no index is generated.
#epub_use_index = True

myst_enable_extensions = [
    "amsmath",
    "colon_fence",
    "deflist",
    "dollarmath",
    "html_admonition",
    "html_image",
    # "linkify",
    "replacements",
    "smartquotes",
    "substitution",
    "tasklist",
]