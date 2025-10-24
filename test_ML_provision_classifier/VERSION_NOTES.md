# Version Notes - Enhanced Implements Extraction

## Version 1.2 (Current)

### Bug Fixes - ML Script
- **Fixed NA values error**: Added proper handling for missing/NA values in numeric and logical columns
- **Fixed memory issue**: Reduced feature set from unlimited to max 500 features, increased minimum term frequency
- **Fixed sparse matrix conversion**: Changed from `convert()` to manual `as.matrix()` conversion to avoid 1.7GB memory allocation
- **Fixed column name mismatch**: Corrected merge operations to use `paragraph_id` consistently

### Files Updated
- `ML_Provision_Classifier.R`

### Memory Optimization
- Reduced term frequency threshold: 5 → 10
- Reduced document frequency threshold: 3 → 5  
- Added max feature limit: 500 most frequent terms
- Improved sparse matrix handling

---

## Version 1.1

### Bug Fixes
- **Fixed Excel sheet name length error**: Renamed "Implement-Official Co-occurrence" to "Implement-Official Co-occur" to comply with Excel's 31-character limit

### Files Updated
- `Extract_Implements_Enhanced.R`

---

## Version 1.0 (Initial Release)

### Features
- Context-aware implement extraction
- Dependency parsing with grammatical analysis
- Confidence scoring (high/medium)
- 27 implement types
- 14 official types
- 8 provision types
- Co-occurrence analysis

### Known Issues
- ~~Sheet name too long error~~ (Fixed in v1.1)

---

## Troubleshooting

### Excel Sheet Name Error
**Error**: `sheetName 'XYZ' too long! Max length is 31 characters`

**Cause**: Excel limits worksheet names to 31 characters

**Solution**: Use abbreviated sheet names. Fixed in v1.1.

### Other Common Issues

See QUICKSTART.md for complete troubleshooting guide.

---

## Change Log

### 2025-10-23 (v1.2)
- Fixed NA values causing training errors
- Fixed memory issue (1.7GB allocation)
- Optimized feature selection for memory efficiency
- Fixed column name consistency in merges

### 2025-10-23 (v1.1)
- Fixed sheet name length issue

### 2025-10-23 (v1.0)
- Initial release with all enhanced features

---

## Future Enhancements (Planned)

### Potential Improvements
- [ ] Add more implement types based on user feedback
- [ ] Optimize dependency parsing speed
- [ ] Add visualization exports (charts/graphs)
- [ ] Support for regulations (in addition to Acts)
- [ ] Batch processing mode for multiple databases
- [ ] Integration with Shiny dashboard

### User Requests
*Submit feedback and feature requests to the project team*

---

## Compatibility

### Tested With
- R 4.0.0+
- RStudio 2023.06.0+
- udpipe 0.8.11
- quanteda 3.3.1
- openxlsx 4.2.5

### Known Compatible Systems
- Windows 10/11
- macOS 12+
- Linux (Ubuntu 20.04+)

### Excel Compatibility
- Microsoft Excel 2016+
- LibreOffice Calc 7.0+
- Google Sheets (upload .xlsx file)

---

## Support

For issues or questions:
1. Check QUICKSTART.md
2. Review README_Enhanced_Scripts.md
3. Contact: Joe Enns, Cory Lagasse

---

## License

Part of the LAPSE (Legislation Applicable to Pacific Salmon and Ecosystems) project.
