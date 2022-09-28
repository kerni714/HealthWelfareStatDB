versions_obj <- return_meta(type="api_version")
versions<- contentToDataframe_meta(versions_obj)

test_that("Check version equal to v1", {
  expect_equal(toString(versions[1]), "v1")
  expect_equal(toString(versions[2]), "version 1") #We use to string to compare the values because if not the classesare the ones that are compare
})

languages_obj <- return_meta(type="lang")
languages <- contentToDataframe_meta(languages_obj)

test_that("Check language is equal to [en]glish and swedish(svenska)",{
  expect_match(languages[1,1], "sv") #you can use expect_match to compare strings instead of using toString with expect equal
  expect_match(languages[2,2], "engelska")
  expect_equal(dim.data.frame(languages), c(2,2))
  })

topics_objen <- return_meta(type="topic", lang="en")
topicsen <- contentToDataframe_meta(topics_objen)
topics_objsv <- return_meta(type="topic", lang="sv")
topicssv <- contentToDataframe_meta(topics_objsv)

test_that("Check topics for english and swedish",{
  expect_equal(dim.data.frame(topicsen), c(2,2)) #test length in english
  expect_equal(dim.data.frame(topicssv), c(13,2)) #test length in swedish
  expect_equal((names(topicsen)), c("namn","text"))
})



