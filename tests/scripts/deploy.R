install.packages("remotes")

remotes::install_github("Helseatlas/kart")
remotes::install_github("Helseatlas/data")
remotes::install_github("Helseatlas/shinymap", ref = Sys.getenv("TRAVIS_BRANCH"))

# All atlas data and maps
all_data <- list(
  "fodsel"   = list("data_no" = data::fodsel,
                    "data_en" = data::fodsel_en,
                    "year" = "2015–2017",
                    "title_no" = "Fødselshjelp",
                    "title_en" = "Obstetrics",
                    "map" = kart::fodsel),
  "gyn"      = list("data_no" = data::gyn,
                    "data_en" = data::gyn_en,
                    "year" = "2015–2017",
                    "title_no" = "Gynekologi",
                    "title_en" = "Gynaecology",
                    "map" = kart::gyn),
  "ortopedi" = list("data_no" = data::ortopedi,
                    "data_en" = data::ortopedi,
                    "year" = "2012–2016",
                    "title_no" = "Ortopedi",
                    "title_en" = "Orthopaedic",
                    "map" = kart::gyn),
  "dagkir2"  = list("data_no" = data::dagkir2,
                    "data_en" = data::dagkir2_en,
                    "year" = "2013–2017",
                    "title_no" = "Dagkirurgi",
                    "title_en" = "Day surgey",
                    "map" = kart::dagkir2),
  "kols"     = list("data_no" = data::kols,
                    "data_en" = data::kols_en,
                    "year" = "2013–2015",
                    "title_no" = "Kols",
                    "title_en" = "COPD",
                    "map" = kart::kols),
  "eldre"    = list("data_no" = data::eldre,
                    "data_en" = data::eldre_en,
                    "year" = "2013–2015",
                    "title_no" = "Eldre",
                    "title_en" = "Elderly",
                    "map" = kart::eldre),
  "nyfodt"   = list("data_no" = data::nyfodt,
                    "data_en" = data::nyfodt_en,
                    "year" = "2009–2014",
                    "title_no" = "Nyfødt",
                    "title_en" = "Neonatal",
                    "map" = kart::nyfodt),
  "barn"     = list("data_no" = data::barn,
                    "data_en" = data::barn_en,
                    "year" = "2011–2014",
                    "title_no" = "Barn",
                    "title_en" = "Children",
                    "map" = kart::barn),
  "dagir"    = list("data_no" = data::dagkir,
                    "data_en" = data::dagkir_en,
                    "year" = "2011–2013",
                    "title_no" = "Dagkirurgi",
                    "title_en" = "Day surgey",
                    "map" = kart::dagkir)
)

rsconnect::setAccountInfo(name   = Sys.getenv("shinyapps_name"),
                          token  = Sys.getenv("shinyapps_token"),
                          secret = Sys.getenv("shinyapps_secret")
                          )

if (Sys.getenv("TRAVIS_BRANCH") == "master") {
  shinymap::launch_app(
    dataset = all_data,
    publish_app = TRUE,
    name = "helseatlas"
  )
} else {
  shinymap::launch_app(
    dataset = all_data,
    publish_app = TRUE
  )
}
