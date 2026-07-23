test_that("calc_hdds counts food groups correctly", {
  fake_data <- data.frame(
    hhid = rep(1:3, each = 5),
    produit = c(1,2,3,7,8, 1,1,1,1,1, 4,5,6,9,10),
    consomme = 1
  )
  correspondance <- data.frame(
    produit = 1:10,
    groupe = c(rep("A",3), rep("B",3), rep("C",3), "D")
  )
  hdds <- calc_hdds(fake_data, col_menage = "hhid", col_produit = "produit",
                    correspondance = correspondance, col_consomme = "consomme")
  expect_equal(hdds$hdds, c(2, 1, 3))
})