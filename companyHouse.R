library(httr)
library(jsonlite)

httr::set_config(httr::config(http_version = 0)) # suppress errors

source("config.R")

# Retrieve information about UK-listed companies
load_officers <- function(company_id) {
  base <- "https://api.companieshouse.gov.uk/"
  url <- paste(base, "company", "/", company_id, "/", "officers", sep = "")
  req <- GET(url, authenticate(secret_key, "", type = "basic"))
  json <- content(req, "text", encoding = "utf8")
  data <- jsonlite::fromJSON(json, flatten = TRUE)
  as.data.frame(data)
}

# Collect CompanyHouse IDs of all considered companies
companies <- read.csv("data/company_ids.csv", colClasses=c("ID1"="character", "ID2"="character"))
company_ids <- c()
j <- 1
for (i in seq_len(nrow(companies))) {
    company_ids[[j]] <- companies[i, ]$ID1
    j <- j + 1
}

# Collect officers of companies from UK CompanyHouse
officers <- c()
j <- 1
for (company_id in company_ids) {
  officers[[j]] <- load_officers(company_id)
  j <- j + 1
}

# Check all pairs of companies for connection
company_pairs <- t(combn(seq_len(nrow(companies)), 2)) # pick two
for (k in seq_len(nrow(company_pairs))) {
  i <- company_pairs[k, 1]
  j <- company_pairs[k, 2]

  shared_officers <- merge(officers[[i]], officers[[j]], by.x="items.links.officer.appointments", by.y="items.links.officer.appointments")
  if (nrow(shared_officers) > 0) {
    company1 <- companies[i, ]
    company2 <- companies[j, ]

    for (l in seq_len(nrow(shared_officers))) {
      shared_officer <- shared_officers[l, ]
        print(paste(company1$owner_name, "and", company2$owner_name, "share(d)", shared_officer[["items.officer_role.x"]], shared_officer[["items.name.x"]]))
    }
  }
}
