# svBase To Do list

-   Rework the `sXXX()` speedy functions that simply reuse their {dplyr} or {tidyr} counterparts.

-   Implement {tidyr} verbs that are not done yet: `nest_join()`, `chop()`, `unchop()`, `nest()`, `unnest()`, `unnest_longer()`, `unnest_wider()`, `hoist()`, `pack()`, `unpack()`.

-   Check package dependencies and make sure that the speedy and fast functions can be used in a package with minimal dependencies ({data.table} and {collapse} are OK, but {dplyr} or {tidyr} are not).

-   Should `rownames()` and `rownames<-()` get and assign to `.rownames` for data/table and tbl_df objects? Should we do something similar for keys in data.table or tbl_df?

-   Display more information in the `print()` methods of data.frame, data.table and tbl_df: help page, comment, labels units, ... See: vignette("extending", package = "pillar"). Also indicate key for data.table object.
