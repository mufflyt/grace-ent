# ENT Procedure CPT/HCPCS Code Reference

Source: `config/ent_procedure_categories.yml` (version 1.0.0, created 2026-05-15, board ABOto). 11 categories, 71 codes.

**No ICD-10 codes are part of this classifier** -- it is a CPT/HCPCS-only procedure map used to verify that a physician's Medicare Part B billing pattern is consistent with otolaryngology practice. Nothing in this repo currently pairs these procedures with ICD-10 diagnosis codes; adding that mapping would be new work, not a transcription of existing data.

> Anchor CPTs were selected for clinical centrality, not billing volume.
> Categories are intentionally narrow: a category is a single procedure type (e.g., 'Sinus surgery' covers FESS variations; FESS is a category, not a subspecialty).
> For SUBSPECIALTY-level mapping (Rhinology vs Head & Neck Oncology vs Otology / etc.), see config/phase3_procedure_map_ent.yml.
> v1.1 (2026-05-15) added three diagnostic / office-procedure categories (ENT diagnostic procedures, Otolaryngic allergy, Vestibular evaluation) to raise empirical match rate from 36% to ~90% of the Triple Match cohort (4,578 NPIs). Empirical data: scripts/discover_ent_billed_codes.R, run 2026-05-15.
> Codes were chosen by frequency of billing among Triple Match NPIs AND specificity to ENT practice. Generic E&M codes (99202-99215, 99211) are intentionally EXCLUDED because they are billed by every specialty and would destroy specificity.

## Category index

| Category | Anchor CPT | # codes |
|---|---|---|
| Sinus surgery | `31256` | 13 |
| Septoplasty | `30520` | 1 |
| Direct laryngoscopy with biopsy | `31535` | 1 |
| Esophageal dilation | `43450` | 5 |
| Cochlear implant | `69930` | 1 |
| Mastoidectomy | `69501` | 12 |
| Thyroidectomy | `60240` | 10 |
| Neck dissection | `38724` | 3 |
| ENT diagnostic procedures | `31575` | 17 |
| Otolaryngic allergy | `95165` | 5 |
| Vestibular evaluation | `95992` | 3 |

## Sinus surgery

Anchor code: **`31256`** -- Nasal/sinus endoscopy, surgical; with maxillary antrostomy

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `31237` | Nasal/sinus endoscopy, surgical; with biopsy, polypectomy or debridement |  |
| `31253` | Nasal/sinus endoscopy, surgical; total ethmoidectomy |  |
| `31254` | Nasal/sinus endoscopy, surgical; partial ethmoidectomy |  |
| `31255` | Nasal/sinus endoscopy, surgical; total (anterior and posterior) ethmoidectomy |  |
| `31256` | Nasal/sinus endoscopy, surgical; with maxillary antrostomy | ✓ |
| `31259` | Nasal/sinus endoscopy, surgical; with sphenoidotomy and tissue removal |  |
| `31267` | Nasal/sinus endoscopy, surgical; with removal of tissue from maxillary sinus |  |
| `31276` | Nasal/sinus endoscopy, surgical; with frontal sinus exploration |  |
| `31287` | Nasal/sinus endoscopy, surgical; with sphenoid sinusotomy |  |
| `31288` | Nasal/sinus endoscopy, surgical; with sphenoid sinusotomy and tissue removal |  |
| `31295` | Nasal/sinus endoscopy, surgical; with balloon dilation of maxillary sinus ostium |  |
| `31296` | Nasal/sinus endoscopy, surgical; with balloon dilation of frontal sinus ostium |  |
| `31297` | Nasal/sinus endoscopy, surgical; with balloon dilation of sphenoid sinus ostium |  |

## Septoplasty

Anchor code: **`30520`** -- Septoplasty or submucous resection, with or without cartilage scoring

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `30520` | Septoplasty or submucous resection, with or without cartilage scoring | ✓ |

## Direct laryngoscopy with biopsy

Anchor code: **`31535`** -- Laryngoscopy, direct, operative, with biopsy

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `31535` | Laryngoscopy, direct, operative, with biopsy | ✓ |

## Esophageal dilation

Anchor code: **`43450`** -- Dilation of esophagus, by unguided sound or bougie, single or multiple passes

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `43220` | Esophagoscopy, flexible, transoral; with transendoscopic balloon dilation |  |
| `43226` | Esophagoscopy, flexible, transoral; with insertion of guide wire followed by dilation |  |
| `43249` | Esophagogastroduodenoscopy, flexible, transoral; with transendoscopic balloon dilation |  |
| `43450` | Dilation of esophagus, by unguided sound or bougie, single or multiple passes | ✓ |
| `43453` | Dilation of esophagus, over guide wire |  |

## Cochlear implant

Anchor code: **`69930`** -- Cochlear device implantation, with or without mastoidectomy

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `69930` | Cochlear device implantation, with or without mastoidectomy | ✓ |

## Mastoidectomy

Anchor code: **`69501`** -- Transmastoid antrotomy (simple mastoidectomy)

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `69501` | Transmastoid antrotomy (simple mastoidectomy) | ✓ |
| `69502` | Mastoidectomy; complete |  |
| `69505` | Mastoidectomy; modified radical |  |
| `69511` | Mastoidectomy; radical |  |
| `69530` | Petrous apicectomy, including radical mastoidectomy |  |
| `69603` | Tympanoplasty with mastoidectomy; with antrotomy or atticotomy |  |
| `69641` | Tympanoplasty with mastoidectomy (including canalplasty, atticotomy, middle ear surgery, and/or tympanic membrane repair); without ossicular chain reconstruction |  |
| `69642` | Tympanoplasty with mastoidectomy; with ossicular chain reconstruction |  |
| `69643` | Tympanoplasty with mastoidectomy; with intact or reconstructed wall, without ossicular chain reconstruction |  |
| `69644` | Tympanoplasty with mastoidectomy; with intact or reconstructed wall, with ossicular chain reconstruction |  |
| `69645` | Tympanoplasty with mastoidectomy; radical or complete, without ossicular chain reconstruction |  |
| `69646` | Tympanoplasty with mastoidectomy; radical or complete, with ossicular chain reconstruction |  |

## Thyroidectomy

Anchor code: **`60240`** -- Thyroidectomy, total or complete

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `60210` | Partial thyroid lobectomy, unilateral; with or without isthmusectomy |  |
| `60212` | Partial thyroid lobectomy, unilateral; with contralateral subtotal lobectomy, including isthmusectomy |  |
| `60220` | Total thyroid lobectomy, unilateral; with or without isthmusectomy |  |
| `60225` | Total thyroid lobectomy, unilateral; with contralateral subtotal lobectomy, including isthmusectomy |  |
| `60240` | Thyroidectomy, total or complete | ✓ |
| `60252` | Thyroidectomy, total or subtotal for malignancy; with limited neck dissection |  |
| `60254` | Thyroidectomy, total or subtotal for malignancy; with radical neck dissection |  |
| `60260` | Thyroidectomy, removal of all remaining thyroid tissue following previous removal of a portion of thyroid |  |
| `60270` | Thyroidectomy, including substernal thyroid; sternal split or transthoracic approach |  |
| `60271` | Thyroidectomy, including substernal thyroid; cervical approach |  |

## Neck dissection

Anchor code: **`38724`** -- Cervical lymphadenectomy, modified radical (modified neck dissection)

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `38700` | Suprahyoid lymphadenectomy |  |
| `38720` | Cervical lymphadenectomy, complete (radical neck dissection) |  |
| `38724` | Cervical lymphadenectomy, modified radical (modified neck dissection) | ✓ |

## ENT diagnostic procedures

Anchor code: **`31575`** -- Laryngoscopy, flexible; diagnostic

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `31575` | Laryngoscopy, flexible; diagnostic | ✓ |
| `31231` | Nasal endoscopy, diagnostic, unilateral or bilateral (separate procedure) |  |
| `31579` | Laryngoscopy, flexible or rigid telescopic, with stroboscopy |  |
| `31238` | Nasal/sinus endoscopy, surgical; with control of nasal hemorrhage |  |
| `92511` | Nasopharyngoscopy with endoscope (separate procedure) |  |
| `69210` | Removal impacted cerumen requiring instrumentation, unilateral |  |
| `69220` | Debridement, mastoidectomy cavity, simple |  |
| `69200` | Removal foreign body from external auditory canal; without general anesthesia |  |
| `G0268` | Removal of impacted cerumen by physician, same day as audiologic function testing |  |
| `92557` | Comprehensive audiometry threshold evaluation and speech recognition |  |
| `92567` | Tympanometry (impedance testing) |  |
| `92504` | Binocular microscopy (separate diagnostic procedure) |  |
| `92550` | Tympanometry and reflex threshold measurements |  |
| `69433` | Tympanostomy (requiring insertion of ventilating tube), local or topical anesthesia |  |
| `30901` | Control nasal hemorrhage, anterior, simple (limited cautery and/or packing) |  |
| `30903` | Control nasal hemorrhage, anterior, complex (extensive cautery and/or packing) |  |
| `30140` | Submucous resection inferior turbinate, partial or complete, any method |  |

## Otolaryngic allergy

Anchor code: **`95165`** -- Preparation of antigens for allergen immunotherapy (per dose)

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `95115` | Professional services for allergen immunotherapy; single injection |  |
| `95117` | Professional services for allergen immunotherapy; 2 or more injections |  |
| `95165` | Professional services for preparation of antigens for allergen immunotherapy (single or multiple antigens, per dose) | ✓ |
| `95004` | Percutaneous (scratch) allergy skin tests, with allergenic extracts (per test) |  |
| `95024` | Intracutaneous (intradermal) allergy tests with allergenic extracts, immediate reaction (per test) |  |

## Vestibular evaluation

Anchor code: **`95992`** -- Canalith repositioning procedure(s) (e.g., Epley maneuver), per day

| CPT/HCPCS | Description | Anchor |
|---|---|---|
| `95992` | Canalith repositioning procedure(s) (e.g., Epley maneuver, Semont maneuver), per day | ✓ |
| `92540` | Basic vestibular evaluation: oculomotor evaluation + positional nystagmus test + caloric stimulation |  |
| `92537` | Caloric vestibular test with recording, bilateral; bithermal |  |
