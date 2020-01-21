# example check for tests
example_check <- new_check(
    "Example check for testing IO functions", "INFO",
    tab_hasColumn, column = "A")

# example grour of checks for tests
example_group <- add_check(
    new_group(),
    example_check)

# filenames to use to store and upload checks and groups
check_file_name <- "./test_save_check.dqchk"
group_file_name <- "./test_save_check.dqgrp"

# helper to clean up mess afterwards
remove_file <- function(file_name) {
    if (file.exists(file_name)) {
        file.remove(file_name)
    }
}
# save_check
test_that("save_check creates file of with requested name", {
    remove_file(check_file_name)
    save_check(example_check, check_file_name)
    expect_equal(file.exists(check_file_name), TRUE)
    remove_file(check_file_name)
})

test_that("save_check overwrites existing file", {
    remove_file(check_file_name)
    # to test it we will first place wrong content (group)
    # and then correct (check)
    save_check(example_group, check_file_name)
    save_check(example_check, check_file_name)
    a <- load_check(check_file_name)
    expect_equal(a, example_check)
    remove_file(check_file_name)
})

# load_check
test_that("load_check loads exactly the same what was stored", {
    remove_file(check_file_name)
    save_check(example_check, check_file_name)
    a <- load_check(check_file_name)
    expect_equal(a, example_check)
    remove_file(check_file_name)
})

# save_group
test_that("save_group creates file of with requested name", {
    remove_file(group_file_name)
    save_group(example_group, group_file_name)
    expect_equal(file.exists(group_file_name), TRUE)
    remove_file(group_file_name)
})

test_that("save_group overwrites existing file", {
    remove_file(group_file_name)
    # to test it we will first place wrong content (group)
    # and then correct (check)
    save_group(example_check, group_file_name)
    save_group(example_group, group_file_name)
    a <- load_group(group_file_name)
    expect_equal(a, example_group)
    remove_file(group_file_name)
})

# load_group
test_that("load_group loads exactly the same what was stored", {
    remove_file(group_file_name)
    save_group(example_group, group_file_name)
    a <- load_group(group_file_name)
    expect_equal(a, example_group)
    remove_file(group_file_name)
})
