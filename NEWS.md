# svBase 1.0.0

-   The most important functions are added to manage data frames (being data.frame, data.table, or tibble tbl_df objects) in a more coherent way, with the possibility to define one's preferred object.

-   The `dtx()`, `as_dtx()` and `default_dtx()` functions can be used in packages to automatically convert data frames into the preferred object without forcing it to the user.

-   The alternate assignment operators `%<-%` and `%->%` collect result from {dtplyr} (or {dbplyr}) and convert it into the preferred data frame object. Also, the multiple or deconstructing assignment implemented in {zeallot} is also usable here in addition to the conversion into the preferred data frame.

-   Row names were not printed when a data.frame is using {pillar}. It is now a column named and with class \<rownames\>.

-   `as_matrix()` applied to a tbl_df object did not honor `.rownames`. Corrected.

# svBase 0.9.0

-   This is the first version of the package with main functions to manage data.frame/data.table/tibble objects.
