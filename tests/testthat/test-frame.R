test_that("Test frame class and methods", {
  # Frame class
  f <- frame(1, 2, 3, 4)
  expect_named(f, c("left", "top", "width", "height", "unit"))
  expect_false(is_frame("no frame"))
  expect_false(is_frame(mtcars))
  expect_false(is_frame(1))
  expect_false(is_frame(TRUE))
  expect_no_error(assert_frame(f))
  expect_error(assert_frame(TRUE))

  f1 <- frame(0, 0, 1, 1)
  f2 <- frame(0, 0, 2, 2)
  expect_identical(frame_scale(f1, factor = 2), f2)

  expect_identical(frame_ratio(f1), 1)
  expect_identical(frame_ratio(f1), frame_ratio(f2))

  path <- test_image("flag_de")
  f_img <- frame_from_image(path)
  expect_identical(f_img, frame(0, 0, 320, 192))

  f1 <- frame(0, 0, 1, 1)
  f2 <- frame(0, 0, 2, 2)
  f1_new <- frame_scale_to_target(f1, f2)
  expect_identical(f1_new, f2)

  f1 <- frame(3, 4, 1, 1)
  f2 <- frame(0, 0, 2, 2)
  f1_new <- frame_scale_to_target(f1, f2)
  expect_identical(f1_new, frame(3, 4, 2, 2)) # left, top does not change in f1

  f1 <- frame(10, 10, 1, 1)
  f2 <- frame(0, 0, 2, 2)
  expect_identical(frame_fit_to_target(f1, f2), f2)

  f1 <- frame(1, 1, 1, .5)
  f2 <- frame(0, 0, 2, 1)
  expect_identical(frame_fit_to_target(f1, f2), f2)

  f1 <- frame(0, 0, 2, 1)
  f2 <- frame(0, 0, 1, 1)
  f1_new <- frame_fit_to_target(f1, f2)
  # frames_draw(f1, f2)
  # frames_draw(f1_new, f2)
  expect_identical(f1_new, frame(0, .25, 1, .5))

  f1 <- frame(0, 0, 2, 1)
  f2 <- frame(2, 2, 1, 1)
  f1_new <- frame_fit_to_target(f1, f2, fit = "outside")
  expect_identical(f1_new, frame(1.5, 2, 2, 1))
})
