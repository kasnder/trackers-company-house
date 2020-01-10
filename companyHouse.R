library(httr)
library(jsonlite)

source("config.R")

# Retrieve information about UK-listed companies
load_officers <- function(company_id) {
  base <- "https://api.companieshouse.gov.uk/"

  url <- paste(base, "company", "/", company_id, "/", "officers", sep = "")
  req <- GET(url, authenticate(secret_key, "", type = "basic"))
  json <- content(req, "text", encoding = "utf8")
  data <- jsonlite::fromJSON(json, flatten = TRUE)
  df <- as.data.frame(data)
  sapply(strsplit(df[["items.links.officer.appointments"]], "/"), "[[", 3)
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
for (i in seq_len(nrow(company_pairs))) {
  i <- company_pairs[i, 1]
  j <- company_pairs[i, 2]

  shared_officers <- Reduce(intersect,  list(v1 = officers[i], v2 = officers[j]))
  if (length(shared_officers) > 0) {
    company1 <- companies[i, ]
    company2 <- companies[j, ]

    print(paste("Shared officers:", company1$owner_name, company2$owner_name))
  }
}
