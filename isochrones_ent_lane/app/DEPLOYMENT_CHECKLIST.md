# Shiny App Deployment Checklist

**App Status**: ✅ **READY TO DEPLOY**
**All Critical Bugs**: ✅ **FIXED**
**Testing**: ✅ **PASSED**

---

## Pre-Deployment Checklist

### ✅ Code Quality
- [x] All 14 critical bugs fixed
- [x] Input validation added
- [x] Error handling comprehensive
- [x] Helper functions tested
- [x] App runs without errors

### ✅ Testing Complete
- [x] App starts successfully
- [x] Demo data loads correctly
- [x] All tabs render without errors
- [x] Value boxes handle edge cases
- [x] Filters work correctly
- [x] Data table displays properly
- [x] Charts render without crashes

### ✅ Documentation
- [x] Bug report created (SHINY_APP_CRITICAL_BUGS.md)
- [x] Fixes documented (SHINY_APP_BUGS_FIXED.md)
- [x] Deployment guide ready (app/README.md)
- [x] Manuscript integration guide ready

---

## Deployment Steps

### Step 1: Configure shinyapps.io (One-time, 2 min)

```bash
cd app
Rscript setup_shinyapps.R
```

Follow the interactive prompts to enter your credentials.

**Don't have an account?** Sign up at https://www.shinyapps.io/admin/#/signup

---

### Step 2: Deploy (3-5 min)

```bash
Rscript DEPLOY.R
```

The script will:
- ✅ Verify configuration
- ✅ Upload app and dependencies
- ✅ Provide your live URL
- ✅ Open app in browser

Your URL will be: `https://YOUR_USERNAME.shinyapps.io/obgyn-subspecialty-access`

---

### Step 3: Test Live App (5 min)

Once deployed, verify:
- [ ] App loads in browser
- [ ] All tabs accessible
- [ ] Filters work
- [ ] Charts display correctly
- [ ] Data table shows
- [ ] Export functions work
- [ ] No console errors

**Check browser console**: Press F12, look for errors in Console tab

---

### Step 4: Update Manuscript (30 min)

Use the ready-to-paste text from `docs/manuscript-shiny-app-integration.md`:

- [ ] Note your app URL
- [ ] Add to Abstract (1 sentence)
- [ ] Add Methods section "Interactive Data Explorer"
- [ ] Update Results with inline app references
- [ ] Add "Data and Code Availability" section
- [ ] Add Discussion paragraph
- [ ] Update figure captions

**Find and replace**: `[URL]` → `https://YOUR_USERNAME.shinyapps.io/obgyn-subspecialty-access`

---

### Step 5: Share with Collaborators (Optional)

- [ ] Share app URL with co-authors
- [ ] Gather feedback
- [ ] Make any requested adjustments
- [ ] Re-deploy with: `Rscript DEPLOY.R`

---

## Post-Deployment (Optional)

### Add Real Data

If you want to replace demo data with real accessibility metrics:

1. **Run pipeline**:
   ```bash
   cd /Users/tmuffly/isochrones
   Rscript 00_MASTER_PUBLICATION_PIPELINE.R
   ```

2. **Copy data to app**:
   ```bash
   mkdir -p app/data/outputs/accessibility
   cp data/outputs/accessibility/access_by_group*.csv app/data/outputs/accessibility/
   ```

3. **Simplify isochrones** (optional, for map tab):
   ```bash
   Rscript scripts/prepare_shiny_data.R
   mkdir -p app/data/outputs/spatial
   cp data/outputs/spatial/isochrones_for_shiny.rds app/data/outputs/spatial/
   ```

4. **Re-deploy**:
   ```bash
   cd app
   Rscript DEPLOY.R
   ```

---

## Troubleshooting

### Deployment fails

**Check credentials**:
```r
rsconnect::accounts()  # Should show your account
```

**Re-configure if needed**:
```bash
Rscript setup_shinyapps.R
```

**Check quota**:
- Go to: https://www.shinyapps.io/admin/#/dashboard
- Free tier: 5 apps max
- Delete old apps if needed

---

### App deployed but shows errors

1. **Check logs**:
   - Go to: https://www.shinyapps.io/admin/#/dashboard
   - Click on your app
   - Click "Logs" tab

2. **Common issues**:
   - Missing packages (should auto-install)
   - Data file paths (use relative paths in config)
   - Memory limits (reduce data file size)

---

### App works locally but not live

- **Check data paths**: Must be relative, not absolute
- **Check file sizes**: Free tier has 1GB limit
- **Check package versions**: May differ from local

---

## Deployment Metrics

**Expected deployment time**: 3-5 minutes
**Expected file size**: ~5 MB (with demo data)
**Expected URL format**: `https://USERNAME.shinyapps.io/obgyn-subspecialty-access`

---

## Success Criteria

Your deployment is successful when:

✅ App loads at the URL  
✅ All tabs are accessible  
✅ Filters work without errors  
✅ Charts and tables display correctly  
✅ No console errors in browser  
✅ Demo data (or real data) shows properly  

---

## Next Steps After Deployment

1. ✅ **Test the live app thoroughly**
2. ✅ **Share URL with co-authors**
3. ✅ **Integrate into manuscript**
4. ✅ **Submit manuscript with app link**
5. ✅ **Monitor usage** (shinyapps.io provides analytics)

---

## Support

- **Deployment guide**: `app/README.md`
- **Bug fixes documentation**: `SHINY_APP_BUGS_FIXED.md`
- **Manuscript integration**: `docs/manuscript-shiny-app-integration.md`
- **shinyapps.io help**: https://docs.posit.co/shinyapps.io/

---

**Ready to deploy?** Run:

```bash
cd app
Rscript setup_shinyapps.R  # If first time
Rscript DEPLOY.R           # Deploy!
```

🚀 **Good luck!**
