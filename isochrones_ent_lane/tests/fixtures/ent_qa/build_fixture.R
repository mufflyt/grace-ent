# =============================================================================
# Build hand-curated test fixture for ent_registry_counts QA flags
# =============================================================================
# Writes four small input files to tests/fixtures/ent_qa/ that exercise
# every QA bucket in compute_ent_registry_counts():
#
#   N001  Triple Match, General Otolaryngology, CONUS, valid phone, current
#         activity. The canonical happy-path row.
#   N002  Triple Match, Pediatric subspecialty (priority test).
#   N003  Triple Match, Neurotology subspecialty.
#   N004  Triple Match, Sleep Medicine subspecialty.
#   N005  Triple Match, multi-cert (Pediatric + Sleep) — priority resolves
#         to Pediatric.
#   N006  Board only, NPPES non-CONUS / ABOto CONUS — state-mismatch case.
#   N007  Excluded by CONUS filter (state_full = Hawaii).
#   N008  Excluded by retirement filter (practicing_status = "Likely
#         Retired/Inactive", NPI is NA).
#   N009  NPPES + ENThealth, NOT in active ABOto cohort (zone_712,
#         absent_from_abohns).
#   N010  NPPES only — no ABOto match, no ENThealth listing.
#   N011  Two abohns_ids share the same NPI (duplicate-NPI dedup test).
#   N012  Second abohns_id sharing the same NPI as N011.
#   N013  ENThealth-only with NA NPI (orphan).
# =============================================================================

suppressPackageStartupMessages({
  library(arrow); library(readr); library(tibble); library(here)
})

out <- here::here("tests", "fixtures", "ent_qa")
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# ── 1. abohns_raw (cert-level, deduplication input) ──────────────────────────
abohns_raw <- tribble(
  ~abohns_id, ~first_name, ~last_name, ~certification,
  # N001 — General only (one cert)
  1L,         "Alice",     "One",      "Otolaryngology - Head & Neck Surgery",
  # N002 — Pediatric subspec (two certs)
  2L,         "Bob",       "Two",      "Otolaryngology - Head & Neck Surgery",
  2L,         "Bob",       "Two",      "Complex Pediatric Otolaryngology",
  # N003 — Neurotology subspec
  3L,         "Carol",     "Three",    "Otolaryngology - Head & Neck Surgery",
  3L,         "Carol",     "Three",    "Neurotology",
  # N004 — Sleep subspec
  4L,         "Dan",       "Four",     "Otolaryngology - Head & Neck Surgery",
  4L,         "Dan",       "Four",     "Sleep Medicine",
  # N005 — Pediatric + Sleep (priority test → Pediatric)
  5L,         "Eve",       "Five",     "Otolaryngology - Head & Neck Surgery",
  5L,         "Eve",       "Five",     "Complex Pediatric Otolaryngology",
  5L,         "Eve",       "Five",     "Sleep Medicine",
  # N006 — state-mismatch case (CONUS in ABOto, non-CONUS in NPPES)
  6L,         "Frank",     "Six",      "Otolaryngology - Head & Neck Surgery",
  # N007 — Hawaii (CONUS filter exclusion)
  7L,         "Grace",     "Seven",    "Otolaryngology - Head & Neck Surgery",
  # N008 — Retired (no NPI in matched file)
  8L,         "Hank",      "Eight",    "Otolaryngology - Head & Neck Surgery",
  # N011 + N012 — same NPI in two abohns_id rows (dup-NPI test)
  11L,        "Ivy",       "Eleven",   "Otolaryngology - Head & Neck Surgery",
  12L,        "Ivy",       "Twelve",   "Otolaryngology - Head & Neck Surgery"
)
write_csv(abohns_raw, file.path(out, "abohns_certifications_clean.csv"))

# ── 2. abohns_matched (one row per abohns_id) ────────────────────────────────
abohns_matched <- tribble(
  ~abohns_id, ~npi,         ~first_name, ~last_name, ~state_full, ~certification, ~practicing_status,
  1L,         "1000000001", "Alice",     "One",      "Texas",     "Otolaryngology - Head & Neck Surgery", "Practicing",
  2L,         "1000000002", "Bob",       "Two",      "California","Otolaryngology - Head & Neck Surgery", "Practicing",
  3L,         "1000000003", "Carol",     "Three",    "New York",  "Otolaryngology - Head & Neck Surgery", "Practicing",
  4L,         "1000000004", "Dan",       "Four",     "Florida",   "Otolaryngology - Head & Neck Surgery", "Practicing",
  5L,         "1000000005", "Eve",       "Five",     "Illinois",  "Otolaryngology - Head & Neck Surgery", "Practicing",
  6L,         "1000000006", "Frank",     "Six",      "California","Otolaryngology - Head & Neck Surgery", "Practicing",
  7L,         "1000000007", "Grace",     "Seven",    "Hawaii",    "Otolaryngology - Head & Neck Surgery", "Practicing",
  8L,         NA_character_,"Hank",      "Eight",    "Texas",     "Otolaryngology - Head & Neck Surgery", "Likely Retired/Inactive",
  11L,        "1000000011", "Ivy",       "Eleven",   "Pennsylvania", "Otolaryngology - Head & Neck Surgery", "Practicing",
  12L,        "1000000011", "Ivy",       "Twelve",   "Pennsylvania", "Otolaryngology - Head & Neck Surgery", "Practicing"
)
write_csv(abohns_matched, file.path(out, "abohns_with_npi_master.csv"))

# ── 3. NPPES parquet (one row per NPI in the universe) ───────────────────────
nppes <- tribble(
  ~npi,         ~last_name, ~first_name, ~middle_name, ~organization_name,
  ~credential, ~address_line1, ~city, ~state, ~zip, ~phone,
  ~primary_taxonomy, ~all_taxonomy_codes,
  # N001-N005 — Triple Match candidates, all CONUS
  "1000000001", "One",     "Alice",     "A",          NA_character_,
  "MD", "1 Main",     "Houston",   "TX", "770010001", "1234567890",
  "207Y00000X",      "207Y00000X",
  "1000000002", "Two",     "Bob",       NA_character_,NA_character_,
  "MD", "2 Main",     "Los Angeles","CA","900010002", "2234567890",
  "207YP0228X",      "207YP0228X",
  "1000000003", "Three",   "Carol",     NA_character_,NA_character_,
  "MD", "3 Main",     "New York",  "NY", "100010003", "3234567890",
  "207YX0905X",      "207YX0905X",
  "1000000004", "Four",    "Dan",       NA_character_,NA_character_,
  "MD", "4 Main",     "Miami",     "FL", "330010004", "4234567890",
  "207YS0123X",      "207YS0123X",
  "1000000005", "Five",    "Eve",       NA_character_,NA_character_,
  "MD", "5 Main",     "Chicago",   "IL", "606010005", "5234567890",
  "207YP0228X",      "207YP0228X",
  # N006 — non-CONUS NPPES (state-mismatch — physician's NPPES practice is in HI
  # but their ABOto record says California)
  "1000000006", "Six",     "Frank",     NA_character_,NA_character_,
  "MD", "6 Main",     "Honolulu",  "HI", "968010006", "6234567890",
  "207Y00000X",      "207Y00000X",
  # N009 — in NPPES + ENThealth but absent from ABOto entirely
  "1000000009", "Nine",    "Iris",      NA_character_,NA_character_,
  "DO", "9 Main",     "Seattle",   "WA", "981010009", "9234567890",
  "207Y00000X",      "207Y00000X",
  # N010 — NPPES only
  "1000000010", "Ten",     "Jack",      NA_character_,NA_character_,
  "MD", "10 Main",    "Denver",    "CO", "802010010", "1023456789",
  "207Y00000X",      "207Y00000X",
  # N011 — duplicate NPI test
  "1000000011", "Eleven",  "Ivy",       NA_character_,NA_character_,
  "MD", "11 Main",    "Philadelphia","PA","191010011", "1123456789",
  "207Y00000X",      "207Y00000X"
)
write_parquet(nppes, file.path(out, "ent_from_nppes_flat_file.parquet"))

# ── 4. ENThealth roster ──────────────────────────────────────────────────────
ent <- tribble(
  ~npi,         ~first_name, ~last_name, ~credentials, ~specialty_primary,
  ~ent_subspecialty, ~address, ~suite, ~city, ~state, ~zip5,
  ~latitude, ~longitude, ~geocoder_provenance,
  ~phone_norm,        ~phone_source,
  # N001-N005 — Triple Match (ENThealth listed)
  "1000000001", "Alice", "One",   "MD", "General Otolaryngology",
  "GEN_ENT", "1 Main",  NA_character_, "Houston", "TX", "77001",
  29.76, -95.37, "US_Census_Bureau",
  "(123) 456-7890",   "enthealth",
  "1000000002", "Bob",   "Two",   "MD", "Pediatric Otolaryngology",
  "PEDS_ENT","2 Main",  NA_character_, "Los Angeles", "CA", "90001",
  34.05, -118.24, "US_Census_Bureau",
  "(223) 456-7890",   "enthealth",
  "1000000003", "Carol", "Three", "MD", "Neurotology",
  "OTOL",    "3 Main",  NA_character_, "New York", "NY", "10001",
  40.71, -74.00, "US_Census_Bureau",
  "(323) 456-7890",   "enthealth",
  "1000000004", "Dan",   "Four",  "MD", "Sleep Medicine",
  "SLEEP",   "4 Main",  NA_character_, "Miami", "FL", "33001",
  25.76, -80.19, "ArcGIS_World",
  "(423) 456-7890",   "enthealth",
  "1000000005", "Eve",   "Five",  "MD", "Pediatric Otolaryngology",
  "PEDS_ENT","5 Main",  NA_character_, "Chicago", "IL", "60601",
  41.87, -87.62, "US_Census_Bureau",
  "(523) 456-7890",   "enthealth",
  # N009 — NPPES + ENThealth but not in active ABOto cohort
  "1000000009", "Iris",  "Nine",  "DO", "General Otolaryngology",
  "GEN_ENT", "9 Main",  NA_character_, "Seattle", "WA", "98101",
  47.61, -122.33, "US_Census_Bureau",
  "(923) 456-7890",   "enthealth",
  # N013 — ENThealth-only with NA NPI (orphan — tests ent_only zone)
  NA_character_,"Kate", "Thirteen","MD","General Otolaryngology",
  "GEN_ENT", "13 Main", NA_character_, "Boston", "MA", "02101",
  42.36, -71.06, "ArcGIS_World",
  "(133) 456-7890",   "enthealth"
)
write_csv(ent, file.path(out, "ent_master_roster_final.csv"))

cat("Wrote test fixture to:", out, "\n")
cat("  abohns_certifications_clean.csv :", nrow(abohns_raw), "rows\n")
cat("  abohns_with_npi_master.csv      :", nrow(abohns_matched), "rows\n")
cat("  ent_from_nppes_flat_file.parquet:", nrow(nppes), "rows\n")
cat("  ent_master_roster_final.csv     :", nrow(ent), "rows\n")
