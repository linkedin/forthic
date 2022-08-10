# isoweek_module

The isoweek module allows you to work with ISO Week numbers and perform true CY and FY
quarter calculations.

See https://en.wikipedia.org/wiki/ISO_week_date for more info.

## Example
```
["isoweek"] USE-MODULES

# Compute start/end of a quarter containing a specified date
2022-08-09 isoweek.QUARTER-START          # 2022-07-04
2022-08-09 isoweek.QUARTER-END            # 2022-10-02

# Computes fiscal quarter for a company with a FY offset by 2 quarters
2022-08-09 2 isoweek.QUARTER/YEAR         # [1, 2023]  i.e., FY23Q1
2022-06-19 2 isoweek.QUARTER/YEAR         # [4, 2022]  i.e., FY22Q4
```

## Reference

### WEEK-NUM
`(date -- num)`

Returns the ISO week number for the specified date.


### QUARTER-START
`(date -- date)`

Returns the start date for the quarter containing the specified `date`

### QUARTER-END
`(date -- date)`

Returns the end date for the quarter containing the specified `date`

### QUARTER/YEAR
`(date qtr_offset -- [qtr year])`

Given a `date` and a `qtr_offset` (being the quarter offset from the calendar year), this returns
an array whose first element is the quarter for the specified date and whose second element is the associated year.

This is used to compute fiscal quarters from dates where the fiscal calendar is offset from the calendar year.