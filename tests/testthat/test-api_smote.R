test_that(".gsmote_sample generates points within expected bounds", {
    # zero-radius edge case: center == surface_point returns center unchanged
    center <- c(1, 2, 3)
    expect_equal(.gsmote_sample(center, center), center)

    # generic case: generated point must lie within `radius` of center
    surface_point <- c(4, 6, 3)
    radius <- sqrt(sum((center - surface_point)^2))
    set.seed(42)
    new_point <- .gsmote_sample(center, surface_point)
    expect_true(sqrt(sum((new_point - center)^2)) <= radius + 1e-8)
})
