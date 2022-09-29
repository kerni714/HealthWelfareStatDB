

test_that("Check version equal to v1", {
  versions <- return_meta(type="api_version")

  expect_equal(toString(versions[1]), "v1")
  expect_equal(toString(versions[2]), "version 1") #We use to string to compare the values because if not the classesare the ones that are compare
})



test_that("Check language is equal to [en]glish and swedish(svenska)",{

  languages <- return_meta(type="lang")

  expect_match(languages[1,1], "sv") #you can use expect_match to compare strings instead of using toString with expect equal
  expect_match(languages[2,2], "engelska")
  expect_equal(dim.data.frame(languages), c(2,2))
  })



test_that("Check topics for english and swedish",{
  topicsen <- return_meta(type="topic", lang="en")
  topicssv <- return_meta(type="topic", lang="sv")

  expect_equal(dim.data.frame(topicsen), c(2,2)) #test length in english
  expect_equal(dim.data.frame(topicssv), c(13,2)) #test length in swedish
  expect_equal((names(topicsen)), c("namn","text"))
})



test_that("Check variables, Select inpatient topic (diagnoserislutenvard)",{
  vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
  varsndf <- vars[,1:2]

  expect_equal(as.list(varsndf[,1]), list("region", "alder", "kon","matt", "ar", "diagnos"))
})



test_that("Check variable categories, alder and regions",{
  var_cats_alder <- return_meta(type="var_cat", lang="en",
                                topic="diagnoserislutenvard", var="alder")

  var_cats_region <- return_meta(type="var_cat", lang="en",
                                topic="diagnoserislutenvard", var="region")


  expect_equal(dim.data.frame(var_cats_alder), c(18,2))
  expect_equal(var_cats_alder[18,1], 18)
  expect_equal(dim.data.frame(var_cats_region), c(22,3))
  expect_equal(var_cats_region[1,1], 0)
})



test_that("Check data",{
  vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
  var_list <- vars[,1]
  values_list <- c("0",
                   "10,11",
                   "1,2,3",
                   "1",
                   "2017,2018,2019,2020,2021",
                   "C50,J13")
  df_input_vars <- as.data.frame(cbind(var_list,values_list))
  data <- return_data(lang="en",topic="diagnoserislutenvard", df_input_vars,addText=TRUE)

  expect_equal(dim.data.frame(data), c(57,11))
})

test_that("return_data() discovers erroneous value input, number", {
  vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
  var_list <- vars[,1]
  values_list <- c("0",
                   "10,11",
                   "1,2,3",
                   "1",
                   "2017,2018,2019,2020,2021",
                   "C50,J13")
  df_input_vars <- as.data.frame(cbind(var_list,values_list))
  data <- return_data(lang="en",topic="diagnoserislutenvard", df_input_vars,addText=TRUE)

  expect_error(return_data(lang="en",topic="diagnoserislutenvard", df_input_vars))
})

test_that("return_data() discovers erroneous value input, character", {
  vars<- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
  var_list <- vars[,1]
  values_list <- c("0",
                   "10",
                   "1",
                   "1",
                   "2012,2013",
                   "V99")
  df_input_vars <- as.data.frame(cbind(var_list,values_list))
  #data_obj <- return_data(lang="en",topic="diagnoserislutenvard", df_input_vars)

  expect_error(return_data(lang="en",topic="diagnoserislutenvard", df_input_vars))
})
