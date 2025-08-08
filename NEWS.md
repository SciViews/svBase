# svBase 1.6.0

-   First implementation of the SciViews functions (ending with an underscore `_` and with standard evaluation of their arguments, except in formulas and with implicit data-dot).

-   Functions `group_by_()`, `ungroup_()` (and the group companion functions), `summarise_()`, `select_()`, `mutate_()`, `transmute_()`, `rename_()`, `rename_with_()`, `arrange_()`, `pull_()`, `full_join_()`, `inner_join_()`, `left_join_()`, `right_join_()`, `semi_join_()`, `anti_join_()`, `bind_rows()`, `bind_cols()`, `slice_()`, `slice_head()` are done. A `filter_()` version that cannot use grouped data for now is done. A first version of `reframe_()` that does not accept data frame returns or `across()` is implemented.

# svBase 1.5.0

-   There are now functions to create data.trame objects and the default output from `dtx()` or `as_dtx()` or `collect_dtx()` is now a data.trame, instead of a data.table.

# svBase 1.4.1

-   The alternate assign operators `%<-%` and `%->%` implement differently the call to {zeallot} functions because they are completely refactored.

# svBase 1.4.0

-   `scount()`, `sadd_count()`, `tally()` and `add_tally()` are now based on `collapse::fcount()` with some changes to better match the arguments and behavior of the corresponding{dplyr} functions. However, much work was needed to align these speedy functions to the behavior of their tidy counterparts, in part because the tidy functions sort of the frequencies columns and the `collapse::fcount()` function that is used sorts on the categories labels... and it does not sort on it by default.

# svBase 1.3.0

-   The alternate assignments `%<-%` and `%->%` do not evaluate `collect()` in a `try()` construct, silently swallowing any error any more. A `collect.default()` method is defined to make sure to return intact any object that cannot be collected without error.

-    Default `print()` methods are restored for **data.frame** and **data.table** objects.

-    Since {dplyr} functions now return the same object as it is receiving (tibble -> tibble, but data.frame -> data.frame and data.table -> data.table), the use of `lazy_dt()` is mandatory to benefit of {dtplyr} translation into {data.table} statements and its speed performance, examples are adapted accordingly.

-   Bug correction in `sunite()`, the argument `col=` was setting the name of the column always to "col".

# svBase 1.2.2

-   `is_dtx()` and similar functions now also detect **spec_tbl_df** objects from {readr} and **groupedData** of {datasets} are true data frames, even with `strict = TRUE`. Consequently, `default_dtx()` converts also these objects.

-   `as_dtx()` and similar functions get labels and units from the corresponding attributes of **groupedData** objects and apply them to each vector in the resulting data frame.

# svBase 1.2.1

-   The tidy functions are reworked to use the original function name, not the one with a "t" prefix.

-   The functions that list family functions now start with `list_`, like `list_tidy_functions()`, `list_speedy_functions()` or `list_fstat_functions()`.

-   `dtx_rows()` and other similar functions are added to avoid using `dtx(tribble(...))`.

# svBase 1.2.0

-   Addition of "tidy" 't' functions and "speedy" 's' functions. The tidy functions are mostly renamed versions of {dplyr} or {tidyr} functions, while the speedy functions have a similar interface but are from {collapse} or {data.table} for a big part of them. Arguments are homogenized and sometimes reduced to restrict to features that are compatibles in the two groups.

-   The function `n()` is not working in `ssummarise()`. Use `fn(var)` instead, and you can also use it in `tsummarise()`.

-   Addition of "fast" functions from {collapse} + `fn()` and `fna()`.

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
