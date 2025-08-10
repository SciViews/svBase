# svBase To Do list

-   Look at {tidytable}

-   `filter()` by group.

-   `.before=` and `.after=` for `mutate()`.

-   `pivot_longer()` with multiple columns.

-   `fquantile()` and `frange()` operate on vectors only, but it would be useful to get a function that returns only one quantile in the fast stat functions. Also `fcumsum()`, `flag()`, ... => make a category for these additional functions.

-   `collect_dtx()`: do not use collect, but `as_tibble()` and the like that automatically ungroup (currently, we collect and ungroup then). Should we???

-   Deprecate the `sXXX()` speedy functions in favor of the `XXX_()` functions..

- Make the `XXX_()` functions aware of dbplyr, dtplyr, tsibble, DuckDB and polars (and sf?).

-   Implement {dplyr} verbs: `auto_copy()`, `case_match()`, `case_when()`, `coalesce()`, `cross_join()`, `c_across()`, `nest_join()`, `relocate()`, `rowwise()`, `slice_sample()`, `slice_max()`, `slice_min()`. Also `across()`, `all_vars()`, `any_vars()`, `between()`, `consecutive_id()` == `data.table::rleid()`, `cumall()`, `cumany()`, `cummean()`, `cume_dist()`, `dense_rank()`, `first()`, `group_cols()`, `group_map()`, `group_modify()`, `group_walk()`, `group_trim()`, `ident()`, `if_any()`, `if_all()`, `if_else()`, `intersect()`, `lag()`, `last()`, `lead()`, `min_rank()`, `n_distinct()`, `na_if()`, `near()`, `nth()`, `ntile()`, `order_by()`, `percent_rank()`, `pick()`, `recode()`, `recode_factor()`, `row_number()`, `rows_append()`, `rows_delete()`, `rows_insert()`, `rows_patch()`, `rows_update()`, `rows_upsert()`, `setdiff()`, `setequal()`, `sql()`, `symdiff()`, `union()`, `union_all()`, `vars()`. ?context: `n()`, `cur_group()`, `cur_group_id()`, `cur_group_rows()`, `cur_column()`. ?scoped `_if`, `_at`, `_all` (superseded by `across()` and `pick()`). ?select (`everything()`, `last_col()`, `group_cols()`, `starts_with()`, `ends_with()`, `contains()` `matches()`, `num_range()`, `all_of()`, `any_of()`, `where()`). No? `compute()`, `collect()` and `collapse()` + `copy_to()`, `explain()`, `show_query()`, `glimpse()` == `pillar::glimpse()`, `tbl()`, `is.tbl()`? See also ?dplyr_extending, including `dplyr_reconstruct()`, `dplyr_row_slice()` and `dplyr_col_modify()`.

-   Implement {tidyr} verbs that are not done yet: `nest_join()`, `chop()`, `unchop()`, `nest()`, `unnest()`, `unnest_longer()`, `unnest_wider()`, `hoist()`, `pack()`, `unpack()`.

-   Check package dependencies and make sure that the speedy and fast functions can be used in a package with minimal dependencies ({data.table} and {collapse} are OK, but {dplyr} or {tidyr} are not).

-   Should `rownames()` and `rownames<-()` get and assign to `.rownames` for data/table and tbl_df objects? Should we do something similar for keys in data.table or tbl_df?

-   Display more information in the `print()` methods of data.frame, data.table and tbl_df: help page, comment, labels units, ... See: vignette("extending", package = "pillar"). Also indicate key for data.table object.
