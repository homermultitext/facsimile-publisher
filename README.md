# Facsimile editions


Use a quick-and-dirty script to read in a Homer Muiltitext publication in CEX format, and assemble facsimile editions from it.

It's painfully slow, but the static output is usable.  This generates the fascimiles published at <https://homermultitext.github.io/facsimiles/> and <http://homermultitext.org/facsimiles/>.

To build:

1.  Open an sbt console and `:load scripts/facs.sc`
2.  Follow the instructions to build a markdown directory you can serve directly from jekyll.


## Requirements

The script requires a large quantity of RAM.  This is set to 4 Gb in the included `.sbtopts` file.
