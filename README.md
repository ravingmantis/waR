# waR: A battle-hardened R workspace

A workspace directory to make it easier to develop R packages.
Check this project out (e.g. in ``/srv/devel``),
then any R packages in subdirectories within (e.g. ``/srv/devel/my_package``).

The waR ``.Rprofile`` will be loaded whenever you run R from within the waR directory.

The ``.Rprofile`` does nothing for non-interactive sessions.

## R startup

Several features can be enabled by passing extra arguments to ``R --args ...``:

```sh
# Install local package(s) on startup
R --args package_directory another_package

# Install package & Run a script on startup (via. psource(), so working directory is package_directory/)
R --args package_directory package_directory/test/test-foo.R

# Install package, run last command
R --args package_directory last

# Set environment variable, run script
R --args PARROTS=yes package_directory/test/test-foo.R

# Run arbitary R command
R --args 'print("hello")' 'print("world")'
```

## R shutdown

History is saved to ``.Rhistory`` on shutdown.

In addition ``psource()`` will save history before sourcing a script, in case it crashes your R session.

## R Errors

Stack traces are added to any errors.

## hijack()

``hijack()`` extracts state from within a long-running process & saves it to the globalenv for inspection.

Insert a call to ``hijack()`` deep within a package, run a script then inspect the state at the R prompt.
Examples:

```r
# Save every local variable to the global environment
hijack()

# Save foo, bar, baz to the global environment
hijack(foo, bar, baz)

# Save a reference to the local environment to moo
hijack(moo = environment())
# ... later, do moo$foo

# Save a computed value to baa
hijack(baa = foo + bar + baz)
```

NB: ``hijack()`` will not be available outside interactive sessions.
This is intentional, so you don't accidentally release code containing ``hijack()`` commands.

## strl()

Similar to hijack, ``strl()`` wraps ``str()``, but puts all arguments in a list first.

```r
# Equivalent to str(list( moo = moo, oink = oink, baa = 4 + 5 ))
strl(moo, oink, baa = 4 + 5)
```

## psource()

``psource()`` is an imProved ``source()``. In addition to ``source()``'s capabilities, it will:

* Go into the package directory the script is from before running (it walks upwards until it finds a ``.git`` or ``DESCRIPTION``).
* Set (and restore) environment variables with the ``environ`` option
* Save command history in case the R session dies
* Automatically processes .Rmd files with ``knitr::purl()``

```r
# Run a test script as if it was inside a package
psource('package/tests/test-this.R')

# Run a vignette
psource('package/vignettes/my_vignette.Rmd', echo = TRUE)
```

## print.formula()

Add a print method to formula, printing the contents of the environment as well as the code.
