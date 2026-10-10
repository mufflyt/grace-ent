# build_ent_data_dictionary.R
#
# Builds `ent_data_dictionary`  -  a data frame documenting every column in the
# ENT master roster (ent_master_roster_ssot.csv).
# Sourced by run_ent_ssot_standalone.R; also safe to source interactively.
#
# Column schema (modeled on CADR/Lowder AUGS data dictionary format):
#   column        -  variable name in the CSV
#   type          -  R storage type (character / logical / integer / double)
#   file_code     -  short source abbreviation (see legend below)
#   values        -  concise valid-values notation
#   value_labels  -  what each code means (NA when free text)
#   source        -  full citation / data provenance
#   description   -  narrative description with observed counts
#
# File code legend:
#   ENT      -  ENThealth.org physician directory or profile pages
#   NPPES    -  CMS NPPES NPI Registry, February 2026 flat file
#   ABOto    -  ABOto verification portal + ABMS Certification Matters + bulk ABOto download
#   AAO      -  AAO-HNS active member roster (March 5, 2026)
#   Geo      -  US Census Bureau geocoder + ArcGIS fallback
#   Derived  -  Computed from other fields in this dataset
#
# CSV type note: CSV format does not distinguish integer from double. Columns
# declared "integer" here contain only whole numbers; readr reads them back as
# numeric/double, which is correct. Logical columns that are all-NA also read
# back as logical. Use the dictionary type column as the semantic declaration.
#
# Terminology note: "hand search" = systematic, query-based retrieval from a
# public verification portal. No automated crawler was used for any source.

ent_data_dictionary <- tibble::tribble(
  ~column,                   ~type,       ~file_code, ~values,                                  ~value_labels,
  ~source,                   ~description,

  # ── Identity ──────────────────────────────────────────────────────────────
  "url",                     "character", "ENT",      "URL string (unique per physician)",      "format: https://www.enthealth.org/find-ent/{slug}/",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Canonical ENThealth.org profile URL, serving as the unique row identifier for this dataset. The ENThealth.org directory is the AAO-HNS public-facing patient portal. All 5,599 records have a non-NA URL; all are globally unique. The URL is the join key used to link directory-page data to individual physician profile-page data (medical school, residency, fellowship).",

  "full_name",               "character", "ENT",      "free text",                              NA,
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Raw full name string exactly as displayed on ENThealth.org, including all credentials, punctuation, and extra whitespace (e.g., 'Jane  Smith, MD FACS'). No post-processing applied. All 5,599 records are non-NA and globally unique. 127 distinct credential strings appear; most common is 'MD' (4,845 physicians, 86.5%).",

  "clean_name",              "character", "Derived",  "free text",                              NA,
  "Derived: ENThealth.org full_name + custom name parser (R package: stringr 1.5.1)",
  "Display name with credentials stripped and whitespace normalized (e.g., 'A. Michelle  Tanner, MD' → 'A. Michelle Tanner'). All 5,599 records are non-NA and unique. Use first_name + last_name for fuzzy joins; this field is for display only.",

  "first_name",              "character", "Derived",  "free text",                              "top values: Michael=174; David=161; John=124; Robert=97; Mark=88",
  "Derived: custom ENT name parser applied to full_name",
  "Parsed given name extracted by parse_physician_name_enhanced() from R/name_parsing_protocol_enhanced.R (96.7% accuracy on ENT corpus). 0 NA values; 1,749 distinct values across 5,599 records. Physicians listed with only an initial retain that initial as first_name. Used in name-based fuzzy matching to NPPES and board certification records.",

  "middle_name",             "character", "Derived",  "free text / NA",                         "top initials: A.=381; M.=373; J.=371; S.=240; R.=238",
  "Derived: custom ENT name parser applied to full_name",
  "Parsed middle name or initial. 1,293 NA (23.1%); 4,306 non-NA; 377 distinct values. Not used in primary NPI matching because middle name data is sparse in NPPES.",

  "last_name",               "character", "Derived",  "free text",                              "top values: Lee=45; Patel=39; Smith=31; Johnson=30; Kim=26",
  "Derived: custom ENT name parser applied to full_name",
  "Parsed family name. 0 NA values; 4,156 distinct surnames. The most common surnames reflect specialty demographics. Compound surnames preserved as listed. Primary matching token in fuzzy join to NPPES and board certification records.",

  "credentials",             "character", "ENT",      "free text / NA",                         "MD=4845; DO=246; MD PhD=58; MD, FACS=55; MD, PhD=49",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Credential string extracted from the tail of full_name as listed on ENThealth.org. 44 NA (0.8%); 127 distinct credential strings. Parsed into degree_type, is_md, is_do, is_phd, is_facs. Strings can contain multiple designations (e.g., 'MD FACS PhD'); all components extracted independently.",

  "npi",                     "double",    "NPPES",    "10-digit integer / NA",                  NA,
  "NPPES NPI Registry  -  February 2026 public data dissemination (CMS; https://download.cms.gov/nppes/NPI_Files.html). File: npidata_pfile_20050523-20260208.csv.",
  "10-digit National Provider Identifier assigned by CMS. Matched to NPPES February 2026 flat file via multi-pass fuzzy join on name, state, and taxonomy code. 232 NA (4.1%). All 232 NPI-missing physicians also have is_aao_member = FALSE. Primary linkage key for Medicare claims, Open Payments, and all CMS data systems. CSV storage note: readr reads NPI as numeric (no leading zeros exist in 10-digit NPIs). To use as a string in R: as.character(npi). To read directly as string: read_csv(..., col_types = cols(npi = col_character())).",

  # ── Degree / board credentials ─────────────────────────────────────────────
  "degree_type",             "character", "Derived",  "MD / DO / Other / NA",                   "MD=5304 (94.7%); DO=249 (4.4%); Other=2; NA=44",
  "Derived: credentials field",
  "Highest clinical degree category parsed from credentials. 44 NA (0.8%). 'Other' captures 2 physicians with PhD-only credentials. Collapses all MD variants (e.g., 'MD FACS') into a single 'MD' category.",

  "is_md",                   "logical",   "Derived",  "TRUE / FALSE / NA",                      "TRUE=5304 (94.7%); FALSE=251; NA=44",
  "Derived: credentials field",
  "TRUE if credentials contains 'MD'. Physicians with 'MD PhD' or 'MD FACS' are coded TRUE. 44 NA share the same records as credentials = NA.",

  "is_do",                   "logical",   "Derived",  "TRUE / FALSE / NA",                      "TRUE=249 (4.4%); FALSE=5306; NA=44",
  "Derived: credentials field",
  "TRUE if credentials contains 'DO'. DO-trained otolaryngologists complete the same 5-year ACGME residency as MD graduates. Mutually exclusive with is_md in this dataset.",

  "is_phd",                  "logical",   "Derived",  "TRUE / FALSE / NA",                      "TRUE=124 (2.2%); FALSE=5431; NA=44",
  "Derived: credentials field",
  "TRUE if credentials contains 'PhD'. Reflects physician-scientists with combined MD-PhD training. Not mutually exclusive with is_md (most is_phd=TRUE also have is_md=TRUE).",

  "is_facs",                 "logical",   "Derived",  "TRUE / FALSE / NA",                      "TRUE=105 (1.9%); FALSE=5450; NA=44",
  "Derived: credentials field",
  "TRUE if credentials contains 'FACS' (Fellow of the American College of Surgeons). The low rate (1.9%) reflects that ACS fellowship is voluntary and more common in general and thoracic surgery.",

  "is_board_certified",      "logical",   "ABOto",    "TRUE / FALSE",                           "TRUE=4187 (74.8%); FALSE=1412 (25.2%); NA=0",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026) + ABMS Certification Matters portal (https://www.certificationmatters.org/; hand search finished May 5, 2026) + bulk ABOto download",
  "TRUE if verified as currently or previously certified by ABOto through any of three sources: ABOto portal hand search (6,346 records), ABMS Certification Matters (767 ENT records after specialty filter), or bulk ABOto download (17,474 records). Both portals reflect ABOto, a member board of ABMS. The 1,412 uncertified may include new graduates, name-match failures, or non-US-trained physicians.",

  "n_board_certs",           "integer",   "ABOto",    "0-3",                                    "0=1412; 1=3847; 2=334; 3=6",
  "ABOto verification portal (hand search finished May 5, 2026) + bulk ABOto download",
  "Count of distinct board certifications across all three certification sources. 0 NA values. Physicians with 2 certs typically hold ABOto general ENT plus a subspecialty cert (e.g., Facial Plastic Surgery or Sleep Medicine). The 6 physicians with 3 certs hold ABOto plus two certifications from outside ENT.",

  "board_cert_year",         "integer",   "ABOto",    "1972-2025 / NA",                         NA,
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026)",
  "Year of initial ABOto board certification. 1,747 NA (31.2%). Among 3,852 matched: median 2003 (23 years since certification as of 2026). Physicians certified before ~2002 hold non-time-limited certificates and are not subject to MOC renewal.",

  "years_since_board_cert",  "integer",   "Derived",  "1-54 / NA",                              NA,
  "Derived: 2026 − board_cert_year",
  "Years since initial ABOto board certification (2026 − board_cert_year). 1,747 NA. Median 23 years. Highly correlated with years_since_residency (r ≈ 0.97). Prefer years_since_residency for career-stage analyses due to lower NA rate (2.8% vs 31.2%).",

  "board_cert_lapsed",       "logical",   "ABOto",    "TRUE / FALSE / NA",                      "TRUE=3484; FALSE=368; NA=1747",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026)",
  "TRUE if the initial 10-year ABOto certification window has passed. High TRUE rate (90.5% of non-NA) does NOT mean 90% are currently lapsed  -  physicians certified before ~2016 have a lapsed initial certificate by definition and must renew via MOC. FALSE records (n=368) are non-time-limited certs issued before ~2002. NA = no board cert data found.",

  "board_names",             "character", "ABOto",    "pipe-delimited text / NA",               "ABOto - Head & Neck Surgery=3766; + Facial Plastic=243; + Sleep=52; legacy ABOto=51; + Neurological Surgery=16",
  "ABOto verification portal (hand search finished May 5, 2026) + ABMS Certification Matters (hand search finished May 5, 2026) + bulk ABOto download",
  "Pipe-delimited (|) list of all certification names retrieved across all three sources. 1,412 NA; 34 distinct values among 4,187 matched. Pipe delimiter chosen to avoid confusion with semicolons in board names.",

  "primary_board",           "character", "ABOto",    "free text / NA",                         "ABOto - Head & Neck Surgery=4102; legacy ABOto=52; ABOto-1 (multiple certs)=6; ABOto-2=4; American Board of Surgery=4",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026)",
  "Name of the certifying board for the physician's primary (first-listed) certification. 1,412 NA; 16 distinct values. Legacy name 'American Board of Otolaryngology' (without head-neck suffix, n=52) reflects certs issued before the 2004 name change.",

  "has_abohns",              "logical",   "ABOto",    "TRUE / FALSE",                           "TRUE=4122 (73.6%); FALSE=1477 (26.4%); NA=0",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026) + bulk ABOto download",
  "TRUE if physician holds a verified ABOto certification. Stricter than is_board_certified (TRUE=4,187): has_abohns requires an explicit ABOto record; the 65-record difference are physicians verified via ABMS portal only.",

  "has_facial_plastic",      "logical",   "ABOto",    "TRUE / FALSE",                           "TRUE=254 (4.5%); FALSE=5345; NA=0",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026) + bulk ABOto download",
  "TRUE if a Facial Plastic and Reconstructive Surgery subspecialty certificate was retrieved. Note: 644 physicians have ent_subspecialty=FPRS but only 254 hold the ABOto FPRS certificate; the remaining 390 received FPRS designation from ENThealth specialty_primary or fellowship_name.",

  "has_neurotology",         "logical",   "ABOto",    "TRUE / FALSE",                           "TRUE=0; FALSE=5599; NA=0",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026)",
  "TRUE if a Neurotology subspecialty certificate was retrieved. KNOWN DATA QUALITY ISSUE: uniformly FALSE in current dataset despite 525 physicians having ent_subspecialty=OTOL_NEURO. The Neurotology cert appears in board_names but was not extracted into this boolean in the current processing run. Requires recomputation.",

  "board_cert",              "character", "ABOto",    "free text / NA",                         "top: ABOto - Head & Neck Surgery=306; ABOto - 2011=150; ABOto - 2005=137",
  "ABOto verification portal (https://abohns.org/verify-certification; hand search finished May 5, 2026)",
  "Primary board certification name as returned by the ABOto portal, often with a year suffix (e.g., 'American Board of Otolaryngology - Head and Neck Surgery - 2011'). 1,412 NA; 340 distinct values due to year-specific suffixes. Use board_names for the cleaned pipe-delimited version.",

  # ── Practice location ──────────────────────────────────────────────────────
  "company_name",            "character", "ENT",      "free text / NA",                         "ENT and Allergy Associates, LLP=122; Case Western / UH Cleveland=29; Charlotte Eye ENT=25; Mayo Clinic=20",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Practice group or hospital name as listed on ENThealth.org. 553 NA (9.9%); 3,335 distinct values. Group practice affiliation drives shared phone_norm values (3,540 unique phone numbers for 5,599 physicians; large groups share one intake line).",

  "address",                 "character", "ENT",      "free text / NA",                         "top: 660 White Plains Rd Ste 500=58; 6035 Fairview Rd=32; 11100 Euclid Ave=30",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Street address of the primary practice location. 121 NA (2.2%); 3,904 distinct addresses. Multiple physicians share an address at group practices. The 121 NA-address records have geocoded=FALSE and latitude/longitude=NA.",

  "address_had_org_prefix",  "logical",   "Derived",  "TRUE / FALSE",                           "TRUE=205 (3.7%); FALSE=5394; NA=0",
  "Derived: address parser (R/address_core/; February 2026 ENThealth data)",
  "TRUE if the raw address began with a hospital or organization name that was stripped before geocoding (e.g., 'Cleveland Clinic, 9500 Euclid Ave' → prefix stripped). Stripping improves Census geocoder accuracy.",

  "suite",                   "character", "Derived",  "free text / NA",                         "Floor=124; Ste 100=104; Ste 200=104; Ste 500=83; Ste 101=63",
  "Derived: address parser (R/address_core/)",
  "Suite, unit, or floor number extracted from the address string. 2,913 NA (52.0%); 688 distinct values. Removed from the address string before geocoding; retained for address reconstruction.",

  "city",                    "character", "ENT",      "free text",                              "New York=136; Houston=113; Tarrytown=87; Philadelphia=75; Boston=70",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "City of the primary practice location. 0 NA; 1,243 distinct cities across all 5,599 records. May differ slightly from USPS-normalized city in matched_address.",

  "state",                   "character", "ENT",      "2-letter US state/DC code",              "CA=516; NY=476; TX=445; FL=336; PA=264",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "2-letter state abbreviation of the primary practice location. 0 NA; 51 distinct values (50 states + DC). Top 5 states account for 36.3% of the cohort.",

  "zip5",                    "character", "ENT",      "5-digit string",                         "10591=86; 77030=66; 44106=40; 48109=37; 19107=34",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "5-digit US ZIP code as listed on ENThealth.org. 0 NA; 1,946 distinct ZIP codes. Stored as character to preserve leading zeros. Not geocoder-validated; see zip5_geo for the geocoder-returned ZIP.",

  "zip4",                    "character", "ENT",      "4-digit string / NA",                    "0001=167; 5112=57",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "ZIP+4 extension. 1,506 NA (26.9%); 1,891 distinct values. Not used in geocoding; retained for address verification.",

  "zip5_geo",                "character", "Geo",      "5-digit string / NA",                    NA,
  "US Census Bureau Geocoding Service (https://geocoding.geo.census.gov/geocoder/; accessed February-March 2026, Vintage 2020 benchmark)",
  "5-digit ZIP code returned by the geocoder after address normalization; may differ from zip5 when geocoder maps the delivery point to a neighboring postal area. 121 NA (same as geocoded=FALSE); 1,943 distinct values.",

  "latitude",                "double",    "Geo",      "19.7-61.6 / NA",                         NA,
  "US Census Bureau Geocoding Service (https://geocoding.geo.census.gov/geocoder/; accessed February-March 2026, Vintage 2020 benchmark)",
  "WGS-84 decimal latitude of the geocoded practice address. 121 NA (2.2%). Median 39.1°N. Precision: 6 decimal places (~0.1 m). CRS: EPSG:4326. Hawaii (≈19.7°N) and Alaska records are outside CONUS and excluded from isochrone analysis.",

  "longitude",               "double",    "Geo",      "-159.4 to -67.5 / NA",                  NA,
  "US Census Bureau Geocoding Service (https://geocoding.geo.census.gov/geocoder/; accessed February-March 2026, Vintage 2020 benchmark)",
  "WGS-84 decimal longitude of the geocoded practice address. 121 NA (2.2%). Median -86.2°W. Stored as negative values (western hemisphere). Same CRS and precision as latitude.",

  "geocoded",                "logical",   "Geo",      "TRUE / FALSE",                           "TRUE=5478 (97.8%); FALSE=121 (2.2%); NA=0",
  "US Census Bureau Geocoding Service (https://geocoding.geo.census.gov/geocoder/; accessed February-March 2026)",
  "TRUE if a valid latitude/longitude pair was obtained. 121 failures arise from: missing address on ENThealth, PO Box addresses, Puerto Rico addresses, or no Census/ArcGIS match. Physicians with geocoded=FALSE are excluded from isochrone analysis.",

  "geocoder_provenance",     "character", "Geo",      "US_Census_Bureau / ArcGIS_World / NA",   "US_Census_Bureau=4810 (87.8%); ArcGIS_World=668 (12.2%); NA=121",
  "Geocoding pipeline (R/geocode_only.R; February-March 2026)",
  "Which geocoding engine produced the coordinates. Census Bureau is primary; ArcGIS is fallback for addresses with no Census match. No records used Google Maps in the current run.",

  "matched_address",         "character", "Geo",      "free text / NA",                         "top: 660 WHITE PLAINS RD, TARRYTOWN, NY, 10591=84",
  "US Census Bureau Geocoding Service (https://geocoding.geo.census.gov/geocoder/; accessed February-March 2026)",
  "USPS-normalized address string returned by the geocoder. 121 NA; 2,799 distinct values (fewer than 3,904 input addresses due to normalization). Census Bureau returns ALL CAPS; ArcGIS returns mixed case.",

  "match_type",              "character", "Geo",      "rooftop / unknown / zip_centroid / interpolated / NA", "rooftop=256; unknown=222; zip_centroid=139; interpolated=51; NA=4931",
  "ArcGIS REST Geocoding API (accessed March 2026)",
  "Match quality classification. 4,931 NA (88.1%) because this field is only populated for the 668 ArcGIS-geocoded records; Census Bureau records do not return a match_type in this implementation. zip_centroid records have lowest spatial precision.",

  "confidence_score",        "logical",   "Derived",  "all NA (not populated)",                 NA,
  "Derived: NPI matching algorithm (R/canonical_specialty_npi_pipeline.R)",
  "Intended 0-100 NPI match confidence score. KNOWN DATA QUALITY ISSUE: uniformly NA (5,599 NA, 0 non-NA)  -  the per-record score export was skipped in this pipeline run. NPI matching was performed; only the score column was not written. Will be populated in a future update.",

  "country",                 "character", "ENT",      "United States",                          "United States=5599 (100%)",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Country of practice. All 5,599 records are 'United States'. 0 NA. ENThealth.org lists only US-licensed physicians. 14 Puerto Rico physicians are retained here but excluded from isochrone analysis (documented in inclusion_exclusion_appendix.csv).",

  "us_region",               "character", "Derived",  "South / Midwest / West / Northeast",     "South=2054 (36.7%); Midwest=1280 (22.9%); West=1135 (20.3%); Northeast=1130 (20.2%)",
  "Derived: state → US Census Bureau region crosswalk",
  "US Census Bureau region assigned via state-to-region crosswalk. 0 NA. Used for stratified reporting and geographic equity analyses.",

  "acog_district",           "character", "Derived",  "District I-XI (11 values)",              "District IV=780; District VIII=608; District VII=607; District V=581; District VI=581",
  "Derived: state → ACOG district crosswalk (https://www.acog.org/community/districts-and-sections)",
  "ACOG district assigned via state-to-district crosswalk. 0 NA; 11 distinct values. Included for cross-specialty research compatibility with parallel ENT-OB/GYN workforce studies.",

  # ── Training ──────────────────────────────────────────────────────────────
  "medical_school_name",     "character", "ENT",      "free text / NA",                         "Louisiana State Univ=105; Univ of Michigan=102; Baylor=95; Jefferson=94; Indiana Univ=89",
  "ENThealth.org physician profile pages (https://www.enthealth.org/find-ent/{slug}/; hand search finished March 2026)",
  "Medical school name as listed on the ENThealth.org physician profile page. 431 NA (7.7%); 172 distinct schools. International medical schools appear but are not flagged in the current dataset.",

  "medical_school_year",     "integer",   "ENT",      "1962-2025 / NA",                         NA,
  "ENThealth.org physician profile pages (https://www.enthealth.org/find-ent/{slug}/; hand search finished March 2026)",
  "Year of medical school graduation. 470 NA (8.4%); median 2002. KNOWN DATA QUALITY ISSUE: values >2026 (e.g., 2105) are data entry errors on ENThealth.org; ~12 records affected. Exclude year > 2026 from year-based calculations.",

  "residency_name",          "character", "ENT",      "free text / NA",                         "Univ of Iowa=94; Washington Univ=90; Univ of Michigan=89; Univ of Pennsylvania=88; Baylor=83",
  "ENThealth.org physician profile pages (https://www.enthealth.org/find-ent/{slug}/; hand search finished March 2026)",
  "Name of the otolaryngology residency program. 142 NA (2.5%); 216 distinct programs. Top programs align with known high-volume ENT training sites. Not used in subspecialty assignment.",

  "residency_year",          "integer",   "ENT",      "1967-2025 / NA",                         NA,
  "ENThealth.org physician profile pages (https://www.enthealth.org/find-ent/{slug}/; hand search finished March 2026)",
  "Year residency was completed. 158 NA (2.8%); median 2007. KNOWN DATA QUALITY ISSUE: values >2026 (e.g., 2994) are data entry errors; exclude from year-based calculations. Primary input for years_since_residency and career_stage.",

  "fellowship_name",         "character", "ENT",      "15 distinct values / NA",                "Pediatric Otolaryngology=422; Plastic & Reconstructive Surgery=355; Head & Neck Oncology=271; Rhinology=181; Laryngology and the Voice=168; Otology & Neurotology=165; Neurotology and Skull Base Surgery=139; Microvascular=76; Clinical Research=35; Otology=27; Otolaryngic Allergy=24; Sleep Surgery=14; Oral & Facial Surgery=8; Endocrine=2; General Surgery=1",
  "ENThealth.org physician profile pages (https://www.enthealth.org/find-ent/{slug}/; hand search finished March 2026)",
  "Fellowship training program name. 3,711 NA (66.3%); 1,888 non-NA (33.7%). Used as Tier 2.5 in the SSOT hierarchy: when specialty_primary='General Otolaryngology', fellowship_name infers a more specific subspecialty code, rescuing 173 additional subspecialists.",

  "has_med_school",          "logical",   "Derived",  "TRUE / FALSE",                           "TRUE=5168 (92.3%); FALSE=431; NA=0",
  "Derived: medical_school_name is non-NA",
  "TRUE if a medical school name is listed on the ENThealth.org profile. Used as a data completeness indicator.",

  "has_residency",           "logical",   "Derived",  "TRUE / FALSE",                           "TRUE=5457 (97.4%); FALSE=142; NA=0",
  "Derived: residency_name is non-NA",
  "TRUE if a residency program is listed. A listed residency is required for years_since_residency and career_stage to be non-NA.",

  "has_fellowship",          "logical",   "Derived",  "TRUE / FALSE",                           "TRUE=1888 (33.7%); FALSE=3711; NA=0",
  "Derived: fellowship_name is non-NA",
  "TRUE if a fellowship program is listed. One-third of ENT surgeons completed additional subspecialty fellowship beyond the 5-year residency. The inclusion gate for Tier 2.5 SSOT fellowship inference.",

  "years_since_residency",   "integer",   "Derived",  "0-59 / NA",                              NA,
  "Derived: 2026 − residency_year",
  "Years since residency completion. 169 NA; median 19 years. Primary career seniority metric (lower NA than years_since_board_cert). Records with residency_year > 2026 are treated as NA.",

  "career_stage",            "character", "Derived",  "Early (<=10 yrs) / Mid (11-25 yrs) / Senior (>25 yrs) / NA", "Mid (11-25 yrs)=2198 (39.3%); Senior (>25 yrs)=1756 (31.4%); Early (<=10 yrs)=1476 (26.4%); NA=169",
  "Derived: years_since_residency",
  "Categorical career stage based on years since residency. Cutoffs at 10 and 25 years align with the ENT career arc: Early = case volume growth; Mid = peak subspecialty practice; Senior = potential pre-retirement.",

  "data_completeness",       "double",    "Derived",  "5-10 (integer)",                         "10=2779 (49.6%); 9=1788 (31.9%); 8=794 (14.2%); 7=191 (3.4%); 6=33; 5=14",
  "Derived: count of non-NA values across 10 key profile fields",
  "Integer score 0-10 counting non-NA values across: medical_school_name, medical_school_year, residency_name, residency_year, fellowship_name, is_board_certified, board_cert_year, npi, latitude, and one additional pipeline field. 0 NA. Scores ≥9 indicate complete, linkable physician profiles.",

  # ── Contact ───────────────────────────────────────────────────────────────
  "phone_norm",              "character", "ENT",      "(NXX) NXX-XXXX format",                  "top: (215) 955-6784=17; (612) 871-1144=15; (651) 702-0750=14",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "US phone number with area code. 0 NA; 3,540 distinct numbers for 5,599 physicians (group practices share intake lines). Required by the mystery-caller protocol. Physicians without a listed phone were excluded before this dataset was assembled (n=173, see inclusion_exclusion_appendix.csv).",

  "phone_digits",            "double",    "ENT",      "10-digit number",                        NA,
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Phone number as a numeric value with all formatting removed. 0 NA. Stored as double because R integer cannot hold 10-digit values without overflow. Use phone_norm for display; phone_digits for numeric deduplication.",

  "phone_source",            "character", "ENT",      "enthealth",                              "enthealth=5599 (100%)",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Data source for the phone number. 0 NA; 1 distinct value ('enthealth') in the current dataset. Field retained for future enrichment from additional sources if ENThealth phone numbers prove invalid during mystery-caller calls.",

  # ── Subspecialty assignment ────────────────────────────────────────────────
  "specialty_primary",       "character", "ENT",      "11 distinct values / NA",                "General Otolaryngology=2760; Head and Neck Surgery=587; Facial Plastic and Reconstructive Surgery=524; Pediatric Otolaryngology=513; Neurotology=324; Rhinology=235; Laryngology=224; Otology/Audiology=124; Allergy=65; Sleep Medicine=54; Endocrine Surgery=27; NA=162",
  "ENThealth.org physician directory + profile pages (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Primary clinical subspecialty. 162 NA (2.9%). For 3,907 physicians (69.8%), taken directly from ENThealth.org listing; remainder inferred from NPPES taxonomy (1,170), fellowship_name (352), or board cert (8). 'Endocrine Surgery' maps to no canonical ENT code and defaults to GEN_ENT.",

  "specialty_secondary",     "character", "ENT",      "11 distinct values / NA",                "Head and Neck Surgery=336; General Otolaryngology=261; Otology/Audiology=190; Rhinology=178; Allergy=169; NA=3993",
  "ENThealth.org physician directory (https://www.enthealth.org/find-ent/; accessed February 2026)",
  "Self-reported secondary subspecialty from ENThealth.org listing. 3,993 NA (71.3%). Same 11 values as specialty_primary. Not used in SSOT hierarchy; retained for breadth-of-practice analyses.",

  "specialty_inferred_source", "character", "Derived", "original / nppes_taxonomy / fellowship / board_cert / NA", "original=3907 (69.8%); nppes_taxonomy=1170 (20.9%); fellowship=352 (6.3%); board_cert=8 (0.1%); NA=162",
  "Derived: canonical_specialty_npi_pipeline.R",
  "Method used to assign specialty_primary. 'original' = taken directly from ENThealth.org; 'nppes_taxonomy' = inferred from NUCC taxonomy code; 'fellowship' = inferred from fellowship_name; 'board_cert' = inferred from board certification name. Inferred specialties are lower confidence than 'original'.",

  "ent_subspecialty",        "character", "Derived",  "GEN_ENT / FPRS / H_N_ONCO / PED_ENT / OTOL_NEURO / RHINOL / SLEEP / LARYNGOL / ALLERGY", "GEN_ENT=2406 (43.0%); FPRS=644 (11.5%); H_N_ONCO=637 (11.4%); PED_ENT=594 (10.6%); OTOL_NEURO=525 (9.4%); RHINOL=252 (4.5%); SLEEP=237 (4.2%); LARYNGOL=233 (4.2%); ALLERGY=71 (1.3%)",
  "SSOT hierarchy combining 5 data sources (see ssot_source); R/apply_ent_ssot_subspecialty.R; run May 7, 2026",
  "Canonical ENT subspecialty code assigned via the 5-tier SSOT hierarchy. 0 NA  -  every physician receives exactly one code. GEN_ENT = General Otolaryngology (no subspecialty identified). Assignment tiers: Tier 1 ABOto/ABMS subspecialty cert (688); Tier 2 ENThealth non-GEN_ENT (2,132); Tier 2.5 fellowship inference (173); Tier 3 NPPES taxonomy (2,509); Tier 4 general cert (26); Tier 5 default GEN_ENT (18).",

  "subspecialty",            "character", "Derived",  "GEN_ENT / FPRS / H_N_ONCO / PED_ENT / OTOL_NEURO / RHINOL / SLEEP / LARYNGOL / ALLERGY", "same as ent_subspecialty",
  "Mirrors ent_subspecialty",
  "Exact alias of ent_subspecialty. Preserved for schema compatibility with downstream pipeline steps that expect a column named 'subspecialty'. Use ent_subspecialty in new code.",

  "ssot_source",             "character", "Derived",  "7 distinct values",                      "NPPES Taxonomy=2509 (44.8%); ENThealth=2132 (38.1%); Board Cert (Subspecialty)=688 (12.3%); Fellowship Name=173 (3.1%); ENThealth (GEN_ENT)=53 (0.9%); Board Cert (General ENT)=26 (0.5%); Default (GEN_ENT)=18 (0.3%)",
  "SSOT hierarchy (R/apply_ent_ssot_subspecialty.R; run May 7, 2026)",
  "Which SSOT tier assigned ent_subspecialty. 0 NA. 'Board Cert (Subspecialty)' = subspecialty cert from ABOto portal, ABMS portal, or bulk download (hand searches finished May 5, 2026). 'Default (GEN_ENT)' = no cert, ENThealth, fellowship, or taxonomy data matched.",

  "taxonomy_code",           "character", "NPPES",    "207Y* codes (16 distinct) / NA",         "207Y00000X=4230 (76.4%); 207YP0228X=305 (5.5%); 207YX0905X=251 (4.5%); 207YS0123X=174 (3.1%); 207YX0901X=167; 207YX0007X=116; 207YX0602X=56; 207YS0012X=53; other=12; NA=233",
  "NPPES NPI Registry  -  February 2026 (CMS; https://download.cms.gov/nppes/NPI_Files.html). NUCC Taxonomy version 24.1.",
  "NUCC Healthcare Provider Taxonomy Code from NPPES February 2026 flat file (primary taxonomy). 233 NA (4.2%). ENT codes: 207Y00000X=General ENT; 207YP0228X=Pediatric ENT; 207YX0905X=Sleep Medicine; 207YS0123X=Facial Plastic; 207YX0901X=Pediatric ENT alt; 207YX0007X=Facial Plastic / H&N; 207YX0602X=Otology/Neurotology; 207YS0012X=Sleep alt. Used in SSOT Tier 3.",

  # ── AAO membership ─────────────────────────────────────────────────────────
  "is_aao_member",           "logical",   "AAO",      "TRUE / FALSE",                           "TRUE=5367 (95.9%); FALSE=232 (4.1%); NA=0",
  "AAO-HNS active member roster (provided by Tina Maggio, Chief Communications Officer, AAO-HNS; roster date: March 5, 2026)",
  "TRUE if confirmed active AAO-HNS member as of March 5, 2026. Matched by NPI (exact match). The 232 FALSE values are exactly the physicians with npi=NA  -  NPI matching failures, not true non-members. An additional 1,346 AAO members not on ENThealth are excluded from this dataset (no profile → no phone number).",

  "aao_taxonomy",            "character", "AAO",      "207Y* codes (13 distinct) / NA",         "207Y00000X=4182 (77.9%); 207YP0228X=302; 207YX0905X=248; 207YX0007X=186; 207YS0123X=173; NA=232",
  "AAO-HNS member roster NPI match → NPPES February 2026 flat file (https://download.cms.gov/nppes/NPI_Files.html)",
  "NUCC taxonomy code from NPPES as retrieved during AAO-HNS member NPI matching. 232 NA (same NPI-missing records as is_aao_member=FALSE). Same NUCC taxonomy version (24.1) as taxonomy_code. Combined with taxonomy_code via coalesce() in SSOT Tier 3 logic."
)

cat(sprintf("  Data dictionary built: %d fields described\n", nrow(ent_data_dictionary)))
