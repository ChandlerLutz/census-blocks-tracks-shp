Census block, block group, and tract shape files
---

- Download Links: [Blocks](https://www.dropbox.com/scl/fo/6mqquh4ni7hxvklolxv7y/AMgeSpOiLewBcVdDyTQBVBc?rlkey=2y2r97v2qp281fk10e84mtso7&dl=0), [Block Groups](https://www.dropbox.com/scl/fo/5nukzr1gco94urtuawld9/AJpSupbf72DZthunRXaPpfI?rlkey=x8d59zfmigwnvjwwoyd43jtrf&dl=0), [Tracts](https://www.dropbox.com/scl/fo/brzfffe5u64h57ohbun1v/APV6SyitrQscIlnETXzLY3Q?rlkey=6gnd740f3ws47r3ec8eqq75od&dl=0)
- Years: 2000, 2010, 2020
- File format: - R `rds` file with:
  - `GEOID` -- geographic identifier
  - `hh20*` -- the numbers of households by geography
  - `pop20*` -- the population by geography
  - `geometry` -- an `sfc_MULTIPOLYGON`
	- Load the R `sf` package before using
- Notes: 
  - Each rds file holds a `data.table` (load with `library(data.table)`, so that they can be easily combined with `data.table::rbindlist()`
  
