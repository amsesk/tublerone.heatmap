test_that("color assignments behave", {
            df = data.frame(A = c("hey", "yo", "good day"), B = c("see", "ya", "later"))
            expect_equal(assign_colors(df), 
                        list(
                             "A" = c( "hey" = "#855C75",
                                     "you" = "#D9AF6B",
                                     "good day" = "#AF6458"),
                             "B" = c("see" = "#736F4C",
                                     "ya" = "#526A83",
                                     "later" = "#625377")
                             )
                        )
})
