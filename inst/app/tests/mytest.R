app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

# Input 'data_set_table_rows_current' was set, but doesn't have an input binding.
# Input 'data_set_table_rows_all' was set, but doesn't have an input binding.
app$snapshot()
app$setInputs(load_ctmm_data = "click")
# Input 'individuals_rows_current' was set, but doesn't have an input binding.
# Input 'individuals_rows_all' was set, but doesn't have an input binding.
app$snapshot()
