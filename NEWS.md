# svBase 1.2.0

-   Addition of "tidy" 't' functions and "speedy" 's' functions. The tidy functions are mostly renamed versions of {dplyr} or {tidyr} functions, while the speedy functions have a similar interface but are from {collapse} or {data.table} for a big part of them. Arguments are homogenized and sometimes reduced to restrict to features that are compatibles in the two groups.

-   The function `n()` is not working in `ssumarise()`. Use `fn(var)` instead, and you can also use it in `tsummarise()`.

-   Addition of "fast" functions from {collapse} + `fnobs_all()` and `fnobs_na`.

-   The `%xf%` operators where `x`can be `-`, `/`, etc. for a more readable alternative to `TRA=` in the fast functions.

-   More robust strict `is_dt_()` functions for grouped data.

# svBase 1.1.1

-   In `dtf()`, creating a column with name `.rowname` (or the value in `getOption("SciViews.dtx.rownames")`) will transform it into the row names of the resulting data.frame object.

# svBase 1.1.0

-   When `group_by()` is used, `collect()` creates a **grouped_df**. This special object is now recognized and converted by `as_dtx()` and co into **ungrouped** data frame.

-   `collect_dtx()`/`dtf()`/`dtt()`/`dtbl()` added to force the right data frame class (with `collect_dtbl()` giving the same result as `collect()`, except that grouped data are also ungrouped).

# svBase 1.0.0

-   The most important functions are added to manage data frames (being data.frame, data.table, or tibble tbl_df objects) in a more coherent way, with the possibility to define one's preferred object.

-   The `dtx()`, `as_dtx()` and `default_dtx()` functions can be used in packages to automatically convert data frames into the preferred object without forcing it to the user.

-   The alternate assignment operators `%<-%` and `%->%` collect result from {dtplyr} (or {dbplyr}) and convert it into the preferred data frame object. Also, the multiple or deconstructing assignment implemented in {zeallot} is also usable here in addition to the conversion into the preferred data frame.

-   Row names were not printed when a data.frame is using {pillar}. It is now a column named and with class \<rownames\>.

-   `as_matrix()` applied to a tbl_df object did not honor `.rownames`. Corrected.

# svBase 0.9.0

-   This is the first version of the package with main functions to manage data.frame/data.table/tibble objects.
