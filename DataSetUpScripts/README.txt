To create and attribute wolf home ranges, I followed this process:

1. CreatingaKDEs: Create autocorrelated Kernal Density Estimators (aKDEs) from cleaned GPS data
2. CleaningaKDEs: Clean aKDEs by removing extra-territorial movements (removing IOU home ranges, and visual inspection) 
3. CreatingBufferHRs: Create buffered home ranges to quantify environmental attributes availably to each individual/season
4. ExtractingEVI: Extract Enhanced Vegetation Index (EVI) to buffered home ranges using rgee
5. ExtractingLFD: Extract linear feature density to buffered home ranges
6. AttributingaKDEs: Attribute aKDEs with available environmental attributes, monitoring information, and pack size

To set up moose and productivity analysis, I:
1. ExtractEVIToMooseSurveys: Extract EVI using rgee to each moose survey area and merge with density estimates