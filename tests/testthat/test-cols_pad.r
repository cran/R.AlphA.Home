test_that("cols_pad adds the correct number of columns", {
  # Créer un dataframe simple pour les tests
  test_df <- data.frame(a = 1:5, b = letters[1:5])
  
  # Test avec le nombre par défaut de colonnes (100)
  result_default <- cols_pad(test_df)
  expect_equal(ncol(result_default), 100)
  
  # Test avec un nombre spécifique de colonnes
  result_custom <- cols_pad(test_df, nCols = 10)
  expect_equal(ncol(result_custom), 10)
  
  # Vérifier que les colonnes originales sont préservées
  expect_equal(test_df$a, result_default$a)
  expect_equal(test_df$b, result_default$b)
  expect_equal(test_df$a, result_custom$a)
  expect_equal(test_df$b, result_custom$b)
})

test_that("cols_pad uses the correct prefix for new columns", {
  test_df <- data.frame(a = 1:3, b = letters[1:3])
  
  # Test avec le préfixe par défaut
  result_default_prefix <- cols_pad(test_df, nCols = 5)
  expect_true(all(grepl("^x_", names(result_default_prefix)[3:5])))
  
  # Test avec un préfixe personnalisé
  result_custom_prefix <- cols_pad(test_df, nCols = 5, colPrefix = "test_")
  expect_true(all(grepl("^test_", names(result_custom_prefix)[3:5])))
})

test_that("cols_pad returns the original dataframe when nCols equals ncol(data)", {
  test_df <- data.frame(a = 1:3, b = letters[1:3])
  result <- cols_pad(test_df, nCols = 2)
  expect_equal(ncol(result), 2)
  expect_equal(result, test_df)
})

test_that("cols_pad throws an error when nCols is less than ncol(data)", {
  test_df <- data.frame(a = 1:3, b = letters[1:3], c = 4:6)
  expect_error(cols_pad(test_df, nCols = 2), "data already has 3 cols, >2")
})

test_that("cols_pad handles edge cases correctly", {
  # Test avec un dataframe vide
  empty_df <- data.frame()
  result_empty <- cols_pad(empty_df, nCols = 5)
  expect_equal(ncol(result_empty), 5)
  
  # Test avec un très grand nombre de colonnes
  test_df <- data.frame(a = 1:3)
  result_large <- cols_pad(test_df, nCols = 200)
  expect_equal(ncol(result_large), 200)
})
