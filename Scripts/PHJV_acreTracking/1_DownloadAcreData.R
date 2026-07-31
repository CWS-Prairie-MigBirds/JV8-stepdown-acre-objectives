library(httr2)
library(rvest)
library(dplyr)

#Download acre tracking data from the PHJV online database
#create login token
base_url <- "https://phjv.azurewebsites.net"
login_url <- paste0(base_url,"/Identity/Account/Login")
export_page_url <- paste0(base_url, "/AccomplishmentReportExport")
export_post_url <- paste0(base_url, "/AccomplishmentReportExport/ExportAccomplishmentsToCSV")
cookie_file <- tempfile()

Sys.setenv(PHJV_EMAIL = "your-email@example.com",
           PHJV_PASSWORD = "your-password")

login_page_response <- request(login_url) |>
  req_cookie_preserve(cookie_file) |>
  req_perform()

login_page_html <- resp_body_html(login_page_response)

login_token <- login_page_html |>
  html_element('input[name="__RequestVerificationToken"]') |>
  html_attr("value")

login_token #should return a long character string

#submit login token
login_response <- request(login_url) |>
  req_cookie_preserve(cookie_file) |>
  req_body_form(`Input.Email` = Sys.getenv("PHJV_EMAIL"),
                `Input.Password` = Sys.getenv("PHJV_PASSWORD"),
                `__RequestVerificationToken` = login_token) |>
  req_perform()

resp_status(login_response)
resp_url(login_response)

#download the tracking data
export_page_response <- request(export_page_url) |>
  req_cookie_preserve(cookie_file) |>
  req_perform()

resp_status(export_page_response)
resp_url(export_page_response)

export_page_html <- resp_body_html(export_page_response)

export_token <- export_page_html |>
  html_element(
    'input[name="__RequestVerificationToken"]'
  ) |>
  html_attr("value")

export_token #should return a long character string

csv_response <- request(export_post_url) |>
  req_cookie_preserve(cookie_file) |>
  req_headers(Referer = export_page_url) |>
  req_body_form(
    startFiscalYearId = "2021",
    endFiscalYearId = "2025",
    provinceId = "all",
    `__RequestVerificationToken` = export_token
  ) |>
  req_perform()

#insepct to make sure all looks good
resp_status(csv_response)
resp_content_type(csv_response)
resp_headers(csv_response)[
  c("content-type", "content-disposition")
]

#save the csv and clear the environment
output_file <- "Data/AcreTracking/PHJV_acres_2021_2025.csv"
writeBin(resp_body_raw(csv_response),
         output_file)


  

