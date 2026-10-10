# Introduction

Otolaryngologic conditions impose an enormous burden on the United States health care system. Chronic rhinosinusitis affects approximately 12% of American adults (nearly 30 million people), making it one of the most prevalent chronic diseases in the country.^1^ Disabling hearing loss affects an estimated 22.2% of the US population, or approximately 73 million individuals.^2^ Head and neck cancers account for an estimated 71,110 new diagnoses and 16,110 deaths annually, with HPV-associated oropharyngeal malignancies continuing to rise.^3^ Pediatric otolaryngologic disease is equally prevalent: tonsillectomy, performed primarily for recurrent tonsillitis and obstructive sleep-disordered breathing, remains one of the most common surgical procedures in American children, with approximately 567,000 procedures performed in 2019 alone.^4^ Taken together, these conditions generate a substantial and growing demand for subspecialty otolaryngologic care.

Despite this demand, access to otolaryngologists is geographically uneven and projected to worsen. Approximately 11,800 otolaryngologists were practicing in the United States in 2021, but their distribution is highly concentrated in metropolitan areas: roughly 62% of otolaryngologists practice in cities with populations exceeding one million, while rural communities face a deepening shortage.^5,6^ Geospatial analyses have documented that the average travel distance to the nearest otolaryngologist is 22 miles nationally but rises to 47 miles in low-access counties, which are disproportionately rural, lower-income, and located in the Midwest and Great Plains.^7^ Workforce projection models estimate that by 2036, workforce adequacy will reach 98% in metropolitan areas but only 35% in nonmetropolitan areas, a gap that will widen as the population ages and demand for complex otolaryngologic care increases.^5^ Access to fellowship-trained subspecialists, including neurotologists, pediatric otolaryngologists, rhinologists, laryngologists, facial plastic and reconstructive surgeons, and head and neck oncologic surgeons, is even more geographically restricted than access to general otolaryngologists, and meaningful data on the actual appointment availability of these providers remain scarce.

Existing evidence suggests that appointment access to otolaryngologists is further stratified by insurance status, race, and socioeconomic position. Ruthberg and colleagues found that Black patients were 43% less likely than White patients to have an outpatient otolaryngology visit (adjusted odds ratio 0.57; 95% CI 0.50-0.65), and that uninsured patients were 62% less likely to do so (aOR 0.38; 95% CI 0.29-0.51), even after controlling for condition prevalence and demographic covariates.^8^ Studies of appointment wait times have confirmed insurance-based disparities: a national mystery caller study by Corbisiero and colleagues found that Medicaid patients seeking ENT appointments waited 5.73% longer than commercially insured patients (36.8 vs. 32.4 days), with particularly prolonged waits for pediatric otolaryngology and neurotology subspecialists.^9^ These findings parallel patterns documented across other medical specialties, including obstetrics and gynecology, orthopaedics, cardiology, and psychiatry, in which standardized audit studies have consistently found that patients with public insurance or who reside in rural areas face substantially longer waits or higher rates of appointment denial.^10,11^

The mystery caller (audit call) study design, in which trained callers pose as patients to request new appointments without disclosing their research purpose, provides the most direct and ecologically valid measure of appointment access. This methodology was established in the landmark study by Bisgaier and Rhodes, who found that 66% of Medicaid and CHIP-insured children were denied specialty appointments compared with only 11% of privately insured children across eight specialties in Cook County, Illinois.^10^ To date, however, no study has used this methodology to examine whether access to fellowship-trained ENT subspecialists varies by geographic setting, or to systematically characterize the appointment landscape for condition-specific presentations across the full spectrum of recognized ENT subspecialties.

The objective of this study was to measure new-patient appointment access across seven ENT subspecialties using standardized mystery caller methodology in a nationally representative sample of otolaryngologists. We hypothesized that (1) access to fellowship-trained subspecialists would be lower in rural than in urban practice settings; (2) wait times would be longer for subspecialty-specific presentations than for general ENT complaints; and (3) physicians without board certification or fellowship training would be less likely to offer an appointment for subspecialty-appropriate chief complaints. Our findings have implications for health care policy, medical education, and health system planning in a specialty facing a projected workforce crisis.

# Materials and Methods

## Study Design

We conducted a cross-sectional audit study using standardized mystery callers (secret shoppers) to measure new-patient appointment access to otolaryngologists across subspecialties and geographic settings. This design, established by Bisgaier and Rhodes,^10^ uses trained callers posing as patients or patient proxies to elicit appointment availability from physician offices without disclosing their research purpose. No appointments were scheduled at any point. The study was reviewed by the institutional review board and determined to be exempt under 45 CFR 46.104(d)(2) because data were collected through observation of public-facing administrative interactions and no identifiable private information about patients was obtained.

## Physician Identification and Cohort Assembly

We identified otolaryngologists practicing in the contiguous United States using a multi-source data strategy. The primary roster was obtained by a hand search of the ENThealth.org Find-an-ENT patient directory (American Academy of Otolaryngology-Head and Neck Surgery Foundation),^12^ which yielded 7,911 unique physician profiles. This directory is maintained by the specialty's national organization and contains self-reported practice location, credentials, and subspecialty designation for clinically active members.

To supplement and verify this roster, we incorporated three additional data sources: (1) a membership list from the American Academy of Otolaryngology-Head and Neck Surgery (AAO-HNS)^13^ obtained in March 2026, which provided 7,218 NPI-matched records; (2) the publicly searchable certification registry of the American Board of Otolaryngology-Head and Neck Surgery (ABOHNS),^14^ from which we identified 17,474 individual certification records by hand search, including initial certification year, recertification status, and subspecialty designation; and (3) the American Board of Medical Specialties (ABMS) public verification portal (certificationmatters.org) from which we retrieved board certification data for 1,020 targeted physicians, with a successful retrieval rate of 91.9%.

## National Provider Identifier Linkage

Each physician in the ENThealth.org roster was linked to a National Provider Identifier (NPI) using a sequential multi-strategy matching algorithm applied to the February 2026 release of the National Plan and Provider Enumeration System (NPPES) flat file.^15^ The algorithm first attempted exact matching on last name, first name, and practice state; unresolved records advanced to normalized name matching with the CMS Provider Data Catalog,^16^ restricted to records carrying an otolaryngology specialty designation. Records remaining unmatched after both automated strategies were flagged for manual review. NPPES taxonomy codes in the 207Y family were used as an independent signal to confirm specialty affiliation (Appendix 1). After matching, the final cohort comprised 5,599 physicians, of whom 5,367 (95.9%) carried at least one NPI; 5,284 unique NPIs were retained after deduplication of multi-address records.

## Subspecialty Classification

Fellowship-level subspecialty was assigned using a tiered single-source-of-truth hierarchy that resolves conflicts by preferring the most authoritative available source for each physician. In order of precedence:

1. **ABMS certification data** (certificationmatters.org): when a physician's subspecialty certificate was retrieved, the ABMS subspecialty field took precedence over all other sources.
2. **Targeted ABOHNS verification**: board results obtained by direct query to the ABOHNS registry (6,346 records), with subspecialty certificates (e.g., Neurotology, Complex Pediatric Otolaryngology, Sleep Medicine) assigned priority over the general Otolaryngology-Head and Neck Surgery certificate.
3. **Bulk ABOHNS registry data**: the full 17,474-record ABOHNS hand-search dataset was used for any physician not resolved by targeted verification, matched on last name, first five characters of first name, and state.
4. **ENThealth.org specialty field**: the self-reported subspecialty entered by the physician in the AAO-HNS directory.
5. **NPPES taxonomy code**: the 207Y-family taxonomy code registered in NPPES, used as the lowest-precedence fallback.

Subspecialty codes were mapped to nine canonical categories: General Otolaryngology, Head and Neck Oncologic Surgery, Pediatric Otolaryngology, Otology/Neurotology, Facial Plastic and Reconstructive Surgery, Rhinology, Laryngology, Sleep Medicine, and Allergy. The distribution of the final cohort across these categories is shown in Appendix 2.

## Practice Location Geocoding

Practice addresses extracted from ENThealth.org profiles were geocoded in a two-stage cascade. The primary geocoder was the U.S. Census Bureau Geocoding Services API,^17^ which resolved 4,810 addresses (85.9%). Addresses unresolved by the Census API were submitted to the ArcGIS World Geocoding Service,^18^ which resolved an additional 668 addresses (11.9%). The overall geocoding success rate was 97.8% (5,478 of 5,599 physicians). Physicians whose coordinates fell outside the geographic bounds of the contiguous United States or for whom geocoding produced missing coordinates were excluded from spatial analyses. Analyses were restricted to the 48 contiguous states and the District of Columbia; Alaska, Hawaii, and U.S. territories were excluded because drive-time isochrone modeling based on road networks is not applicable to non-contiguous jurisdictions.

## Physician Sampling

From the final geocoded cohort, physicians were sampled for mystery caller contact using stratified random sampling. Strata were defined by subspecialty (nine categories: General Otolaryngology, Pediatric Otolaryngology, Rhinology, Otology/Neurotology, Laryngology, Facial Plastic and Reconstructive Surgery, Head and Neck Oncologic Surgery, Sleep Medicine, and Otolaryngic Allergy) and rurality (urban, large rural, and small rural/isolated, as defined by 2020 USDA Rural-Urban Commuting Area codes; see below). A sample size of [X] physicians per stratum was targeted, providing [X]% power to detect a [X]-percentage-point difference in appointment offer rate between strata at a two-sided alpha of 0.05, assuming a baseline appointment offer rate of [X]%. The final sample included [X] physicians across [X] states.

## Fellowship Training Verification Prior to Each Call

Before each call, the caller searched the sampled physician by name and state of practice using a standard internet search. The practice website appearing among the first search results and matching the physician's name was identified in under one minute. Based on website biography content, the caller determined whether the physician was fellowship trained in a recognized ENT subspecialty. This website-based determination was cross-referenced against the ABOHNS board certification lookup tool^14^ and the tiered SSOT subspecialty classification described above. If the physician was fellowship trained and the subspecialty could be confirmed, the subspecialty-matched call script was selected. If the physician was not fellowship trained, if subspecialty could not be confirmed, or if no practice website was identified, the General Otolaryngology script was used and the absence of a website was recorded. Board certification status was also recorded at this stage; if the physician was not listed as board certified by ABOHNS, callers were instructed to ask "Is Dr. ___ board certified? I want to make sure I see a board-certified doctor" after an appointment was offered.

## Call Scripts

Callers used one of nine standardized scripts matched to the sampled physician's scope of practice (Appendix 3). Each script presented a clinically realistic new-patient scenario requiring referral to a specific ENT subspecialist, based on a chief complaint appropriate to that subspecialty. Scripts were developed with input from subspecialty-trained otolaryngologists and were piloted on a small set of non-study practices before the main data collection period to confirm that responses were elicited without prompting.

## Call Execution

All calls were placed during standard business hours (Monday through Friday, 8:00 a.m. to 5:00 p.m. local time) between [start date] and [end date]. Each sampled physician was contacted at the phone number listed on the practice website identified during pre-call preparation; if no website was found, the phone number registered in the NPPES database was used. Up to three call attempts were made on different days and times before a physician was classified as unreachable. If a voicemail was reached, callers did not leave a message but recorded the attempt and tried again. Calls were documented immediately upon completion using a structured data collection instrument in REDCap (Research Electronic Data Capture).^19^

If a receptionist requested identifying information before providing appointment availability, callers stated: "I want to make sure I can find an appointment that works with my schedule before moving forward with scheduling." If name and date of birth were ultimately required to proceed, callers provided the standardized identity: John Smith (male caller) or Jessica Smith (female caller), with a date of birth of June 1 of the year corresponding to the patient age in the script. No appointments were ever confirmed or scheduled.

## Rurality Classification

Practice rurality was classified using 2020 USDA Rural-Urban Commuting Area (RUCA) codes^20^ assigned to each physician's practice ZIP code. RUCA codes were collapsed into three analytic categories: urban (codes 1-3), large rural (codes 4-6), and small rural or isolated (codes 7-10), consistent with the Rural Health Information Hub classification schema.^21^ Where a physician practiced at multiple locations, rurality was assigned based on the primary practice address identified from the practice website.

## Outcomes

The **primary outcome** was appointment offer rate, defined as the proportion of contacted physicians for whom a new-patient appointment with the named physician was offered. The **secondary outcomes** were: (1) days to next available appointment among physicians offering an appointment; (2) whether the offered appointment was with the named physician or an advanced practice provider (APP); and (3) whether the responding practice indicated the named physician was no longer accepting new patients, no longer practicing at that location, or did not see patients for the presented complaint. When a practice offered an appointment with an APP in lieu of the named physician, the provider type and the wait time for that alternative appointment were recorded separately.

## Statistical Analysis

Appointment offer rates were compared across subspecialties, rurality categories, and physician characteristics using multivariable logistic regression. Wait time to appointment (in days) among physicians offering an appointment was modeled using negative binomial regression to account for overdispersion. Covariates in both models included subspecialty, RUCA category, physician sex, U.S. census region, board certification status, fellowship training status, and practice type (solo vs. group). Physicians classified as unreachable after three attempts were excluded from the primary analysis; a sensitivity analysis treated unreachable physicians as having no appointment offered. All analyses were performed in R (version 4.4.0; R Foundation for Statistical Computing).^22^ Statistical significance was defined as a two-sided p-value less than 0.05.

## Limitations

Several limitations of this study warrant consideration. First, the mystery caller design measures appointment offer rate and wait time but cannot assess the quality of care delivered once an appointment is obtained, the accuracy of triage decisions made by office staff, or patient-reported outcomes. Second, the physician cohort was assembled from the ENThealth.org directory, which reflects AAO-HNS members who maintain a public profile; otolaryngologists who are not AAO-HNS members, who have inactive profiles, or who practice exclusively in hospital-based settings without an outpatient schedule may be underrepresented. Third, fellowship training status was verified using publicly available practice website biographies and the ABOHNS certification registry; physicians who completed fellowship training but do not report it on their practice website or have not sought subspecialty certification may have been misclassified as general otolaryngologists. Fourth, callers used a single standardized patient identity (John or Jessica Smith) and did not vary insurance status, race, or socioeconomic identifiers across calls; accordingly, this study cannot estimate the independent contribution of insurance type or patient race to appointment access, and a separate study varying these characteristics would be required to address those questions. Fifth, rurality was assigned based on the primary practice ZIP code, which may not reflect the full geographic catchment area of a practice or the rural status of the patients it serves. Sixth, because this is a cross-sectional study conducted over a single data collection period, findings reflect appointment availability at a specific point in time and may not capture seasonal variation or longer-term trends in access. Finally, the contiguous United States scope excludes Alaska, Hawaii, and U.S. territories, where otolaryngology access barriers may differ substantially from those in the contiguous states.

---

## References

1. Bhattacharyya N. The economic burden and symptom manifestations of chronic rhinosinusitis. *Am J Rhinol*. 2003;17:27-32.

2. Bhattacharyya N. Prevalence of potential adult chronic rhinosinusitis symptoms in the United States. *Otolaryngol Head Neck Surg*. 2018;159:522-525. doi:10.1177/0194599818774006

3. American Cancer Society. *Cancer Facts & Figures 2024*. Atlanta, GA: American Cancer Society; 2024. Available from: https://www.cancer.org/research/cancer-facts-statistics/all-cancer-facts-figures/2024-cancer-facts-figures.html. Accessed March 15, 2026.

4. Johnson RF, Raman S, Katbamna B, et al. Estimations of pediatric tonsillectomy in the United States 2019. *Otolaryngol Head Neck Surg*. 2023. doi:10.1002/ohn.268

5. Berisha V, Garcia I, Doan D, et al. Otolaryngology workforce projections in the United States, 2021-2036. *Laryngoscope Investig Otolaryngol*. 2025. doi:10.1002/lio2.70148

6. Vickery TW, Weterings R, Cabrera-Muffly C. Geographic distribution of otolaryngologists in the United States. *Ear Nose Throat J*. 2016;95:218-223.

7. Pozin MA, Chung S, Nguyen SA, et al. Geospatial evaluation of access to otolaryngology care in the United States. *Laryngoscope Investig Otolaryngol*. 2024. doi:10.1002/lio2.1233

8. Ruthberg JS, Khan HA, Knusel KD, Rabah NM, Otteson TD. Health disparities in the access and cost of health care for otolaryngologic conditions. *Otolaryngol Head Neck Surg*. 2020;163:716-723. doi:10.1177/0194599820921823

9. Corbisiero EC, Hoff PT, Stucken CL. Insurance status and access to otolaryngology care: national mystery caller study. *Otolaryngol Head Neck Surg*. 2024. doi:10.1002/ohn.752

10. Bisgaier J, Rhodes KV. Auditing access to specialty care for children with public insurance. *N Engl J Med*. 2011;364:2324-2333. doi:10.1056/NEJMsa1013285

11. Corbisiero MF, Tolbert B, Sanches M, Shelden N, Hachicha Y, Dao H, Thillot B, Adkins K, Muffly TM. Medicaid coverage and access to obstetrics and gynecology subspecialists: findings from a national mystery caller study in the United States. *Am J Obstet Gynecol*. 2023;228(6):722.e1-722.e9. doi:10.1016/j.ajog.2023.03.004. PMID: 36907536

12. American Academy of Otolaryngology-Head and Neck Surgery Foundation. ENThealth.org Find-an-ENT patient directory. https://www.enthealth.org/find-ent/. Accessed March 15, 2026.

13. American Academy of Otolaryngology-Head and Neck Surgery. Member resources. https://www.entnet.org/. Accessed March 15, 2026.

14. American Board of Otolaryngology-Head and Neck Surgery. Verify Certification. https://www.abohns.org/verify-certification. Accessed March 15, 2026.


15. Centers for Medicare & Medicaid Services. National Plan and Provider Enumeration System (NPPES): data dissemination. https://www.cms.gov/medicare/regulations-guidance/administrative-simplification/data-dissemination. Accessed March 15, 2026.

16. Centers for Medicare & Medicaid Services. Provider Data Catalog. https://data.cms.gov/provider-data/. Accessed March 15, 2026.

17. United States Census Bureau. Census Geocoder: batch and single-address geocoding service with address standardization, geographic boundary determination, and coordinate return [Internet]. Washington, DC: US Department of Commerce; updated 2024. Available from: https://geocoding.geo.census.gov/. Accessed March 15, 2026.

18. Esri. ArcGIS REST APIs: geocoding service. https://developers.arcgis.com/rest/geocode/. Accessed March 15, 2026.

19. Harris PA, Taylor R, Thielke R, Payne J, Gonzalez N, Conde JG. Research electronic data capture (REDCap), a metadata-driven methodology and workflow process for providing translational research informatics support. *J Biomed Inform*. 2009;42:377-381. doi:10.1016/j.jbi.2008.08.010

20. United States Department of Agriculture Economic Research Service. Rural-Urban Commuting Area Codes. https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/. Accessed March 15, 2026.

21. Rural Health Information Hub. Am I Rural? https://www.ruralhealthinfo.org/. Accessed March 15, 2026.

22. R Core Team. R: A Language and Environment for Statistical Computing. Version 4.4.0. Vienna, Austria: R Foundation for Statistical Computing; 2024. https://www.r-project.org/.

---

**Appendix 1.** NUCC Healthcare Provider Taxonomy Codes Used to Identify Otolaryngologists

| Code | Designation |
|------|-------------|
| 207Y00000X | Otolaryngology - General |
| 207YS0123X | Otolaryngology - Facial Plastic Surgery |
| 207YX0602X | Otolaryngology - Otology / Neurotology |
| 207YX0901X | Otolaryngology - Pediatric |
| 207YP0228X | Otolaryngology - Pediatric Otolaryngology |
| 207YX0905X | Otolaryngology - Sleep Medicine |
| 207YS0012X | Otolaryngology - Sleep Medicine (alternate) |
| 207YX0007X | Otolaryngology - Facial Plastic / Head and Neck Plastic Surgery |

---

**Appendix 2.** Final Cohort Subspecialty Distribution (*n* = 5,599)

| Subspecialty | *n* | % |
|---|---|---|
| General Otolaryngology | 4,596 | 82.1 |
| Pediatric Otolaryngology | 322 | 5.7 |
| Otology / Neurotology | 313 | 5.6 |
| Sleep Medicine | 171 | 3.1 |
| Head and Neck Oncologic Surgery | 97 | 1.7 |
| Rhinology | 34 | 0.6 |
| Laryngology | 31 | 0.6 |
| Facial Plastic and Reconstructive Surgery | 28 | 0.5 |
| Allergy | 7 | 0.1 |

---

**Appendix 3.** Mystery Caller Scripts by Subspecialty

| Subspecialty | Patient Scenario | Chief Complaint |
|---|---|---|
| General Otolaryngology | 40-year-old with nasal obstruction, failed medical therapy, referred by PCP for deviated septum evaluation | Nasal obstruction |
| Pediatric Otolaryngology | 12-year-old with 7 episodes of strep throat in 12 months, new-onset loud snoring and daytime somnolence, referred by pediatrician to discuss tonsillectomy | Recurrent tonsillitis |
| Rhinology | 40-year-old with chronic sinusitis and prior sinus surgery 10 years ago, recurrent symptoms, referred for possible revision surgery | Revision chronic sinusitis |
| Otology / Neurotology | 70-year-old woman with progressive hearing loss no longer benefiting from hearing aids, referred by audiologist for cochlear implant evaluation | Cochlear implant evaluation |
| Laryngology | 70-year-old man with left vocal cord paralysis following open heart surgery, completed voice therapy without improvement, referred to discuss surgical options | Dysphonia / vocal cord paralysis |
| Facial Plastic and Reconstructive Surgery | 25-year-old with nasal deformity and obstruction following motor vehicle accident one year prior, referred for surgical evaluation | Post-traumatic rhinoplasty |
| Head and Neck Oncologic Surgery | 50-year-old with thyroid nodule biopsied by endocrinologist showing 70% probability of malignancy, referred to discuss thyroidectomy | Concerning thyroid nodule |
