# Proposal: Scientifically Valid Dummy Data Generation for Monolith 0.8.3

## 1. Spatial Integrity
- **Preserved Columns:** `x`, `y`, `latitude`, `longitude`, `locality`, `point id`, `sample_no`, `data_from`, `subset`.
- **Reason:** Ensuring mapping and subsetting functions of the app remain fully functional at the original locations.

## 2. Soil Physicochemistry (ph, ec, som, nutrients, etc.)
- **Method:** Synthetic generation using representative statistical distributions for agricultural/natural soils.
- **Distributions:**
    - **Normal/Truncated Normal:** for `ph` (range 4.0-9.0), `bulk_density`, `porosity`.
    - **Lognormal:** for `ec`, `som`, `tn`, `p`, `k`, `ca`, `mg`, `fe`, `mn`, `cu`, `zn` (reflects typical soil nutrient skewness).
    - **Dirichlet / Constraint-based:** for `clay`, `sand`, `silt` (ensuring they sum to 100%).
- **Spatial Pattern:** For the large dataset (`samp_data_1.xlsx`), we will use a **Gaussian Random Field (GRF)** approach or a spatially-weighted noise filter to create realistic spatial gradients and clusters, rather than pure white noise. This makes the generated maps look scientifically plausible.

## 3. Fake Predictions (`_cve` and `_ss`)
- **Method:** `_cve` (uncertainty) will be modeled as a small positive random field (e.g., 0.05-0.20 range).
- **Method:** `_ss` (standardized scores) will be generated as a function of the underlying fake property with added prediction error noise.

## 4. Privacy & Non-Exposure
- **Zero-Value Dependency:** No original values will be used in the generation process.
- **Independent Seed:** A new random seed will be used for each generation run.
- **No Shuffling:** Shuffling is avoided in favor of complete synthesis to ensure even the original value distribution is not exposed.

## 5. Implementation Plan
- **Script:** A Python script using `pandas`, `numpy`, and `scipy` for efficient generation and spatial filtering.
- **Output:** New `.xlsx` files that exactly match the schema of the original files but contain 100% synthetic numeric data.
