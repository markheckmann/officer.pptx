# Journal

## 2026-08-15

**What was done:** Comprehensive improvements to `text_replace()` code and documentation: performance optimization (log accumulation), automatic pattern sorting (longest-first), input validation, snapshot tests, example files. Set up CI/CD pipelines (R-CMD-check, test-coverage, pkgdown), configured pkgdown site, updated README, added hex logo, switched license to MIT, merged `feat/text_replace` and `0.1.1` branches into `main` and cleaned up.

**Result:** Package now has full CI/CD (R-CMD-check multi-platform, Codecov, pkgdown via GitHub Pages), MIT license, hex logo, updated README, and `text_replace()` is significantly more robust with auto-sort, slide_idx validation, and better documentation. Dev vignettes moved to `dev_local/` due to quarto rendering issues in pkgdown. AGENTS.md extended with GitHub/CI and behavior-to-preserve sections.

**Next steps:**
- Verify R-CMD-check and pkgdown pass after latest fixes (locatexec guard, LibreOffice install)
- Check pkgdown site live after successful deploy
