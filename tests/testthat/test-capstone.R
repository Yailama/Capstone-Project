library(testthat)
library(capstone.project)

###########CREATE DATAFRAME WITH SAME STRUCTURE (VARIABLES  AND ITS CLASSES) AS INITIAL#####
LONGITUDE<-c("   35.500", "   58.200", "   35.800", "   25.400", "   35.300")
LATITUDE<-c("  31.100", "  38.000", "  35.683", "  36.400", "  31.500")
COUNTRY<-c("JORDAN", "TURKMENISTAN", "SYRIA", "GREECE", "ISRAEL")
YEAR<-c(2000, 2001, 2001, 2002, 2002)
MONTH<-c(5, 5, 6, 6, 6)
DAY<-c(7, 17, 4, 6, 7)
DEATHS<-c(NA,  "       3", "     103", "       2", NA )
EQ_MAG_ML<-c(NA, 6.2, 5.2, 7.1, NA)

test_df<-data.frame(LONGITUDE, LATITUDE, COUNTRY, YEAR, MONTH, DAY, DEATHS, EQ_MAG_ML,
                    stringsAsFactors=FALSE)

###########################################################################################
############################RUN  TESTS#####################################################

expect_that(eq_location_clean("MEXICO"), is_a("character"))
expect_match(eq_location_clean("MEXICO"), "Mexico:")


test_that("Cleaning base", {
  test_cleaned<-eq_clean_data(test_df)
  expect_that(test_cleaned, is_a("data.frame"))
  expect_that(test_cleaned$DATE, is_a("POSIXct"))
  expect_that(test_cleaned$LONGITUDE, is_a("numeric"))
  expect_that(test_cleaned$LATITUDE, is_a("numeric"))
})



test_that("geom_timeline test",class(geom_timeline())[3]=="ggproto")
test_that("geom_timeline_label test",class(geom_timeline_label())[3]=="ggproto")


test_that("Appropriate creation of labels",{
  test_popuped<-eq_create_label(test_df, "COUNTRY", "EQ_MAG_ML", "DEATHS" )
  expect_that(test_popuped, is_a("character"))
  expect_match(test_popuped[!is.na(test_df$DEATHS)], ".+Deaths.+")
  expect_failure(expect_match(test_popuped[is.na(test_df$DEATHS)], ".+Deaths.+"))
})

test_that("Leaflet HTML widget", {
  test_df<-eq_clean_data(test_df)
  test_df$popup<-eq_create_label(test_df,  "COUNTRY", "EQ_MAG_ML", "DEATHS" )
  p<-eq_map(test_df, "LONGITUDE", "LATITUDE", "popup")
  expect_s3_class(p, "leaflet")
  expect_s3_class(p, "htmlwidget")

})

