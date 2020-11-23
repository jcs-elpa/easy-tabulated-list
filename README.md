[![Build Status](https://travis-ci.com/jcs-elpa/easy-tabulated-list.svg?branch=master)](https://travis-ci.com/jcs-elpa/easy-tabulated-list)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# easy-tabulated-list
> Simplify usage for `tabulated-list'.

This package managed and clean up code while you are implementing your tool
with `tabulated-list`.

The `easy-tabulated-list` package is a tiny library that are target to design
to simplify the usage of `tabulated-list`. There are multiple variables that
you would need to customize in order to create a `tabulated-list` buffer; yet
this package would help and make the code a little bit cleaner.

## Usage

Set all needed variables within a function call.

```el
(easy-tabulated-list-make your-format
                          (your-entires)
                          :sort-key (cons "Type" nil)
                          :padding 1)
```

Create entries with a better readability. And make you focus on building the
data you want to display in the table.

```el
(easy-tabulated-list-form-entries (list '("1" "2" "3")
                                        '("4" "5" "6")
                                        '("7" "8")))
```

## Advance Usage

With these functionality, you now able to create buffer built on top of
`tabulated-list`. You will no longer need to worry about the basic implementation
from `tabulated-list`. This brings you the benefits to just focus on building the
data table itself.

* See example [here](./examples/test-table.el).

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
