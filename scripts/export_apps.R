# scripts/export_apps.R
# Export Shiny apps and render docs/index.md -> docs/index.html (with .nojekyll kept)
# - Legacy main app:        study-*/app                      -> docs/studies/<study>/app
# - Sibling apps (flat):    study-*/X/app.R                  -> docs/studies/<study>/X
# - Sibling apps (nested):  study-*/X/app/app.R              -> docs/studies/<study>/X
# - Sub-apps under app/:    study-*/app/Y[/app].R            -> docs/studies/<study>/Y

# install.packages(c("fs","shinylive","rmarkdown"))  # run once if needed
library(fs)
library(shinylive)

is_shiny_app_dir <- function(p) {
  file_exists(path(p, "app.R")) ||
    (file_exists(path(p, "ui.R")) && file_exists(path(p, "server.R")))
}

export_one <- function(app_dir, out_dir) {
  message("🚀 Exporting ", app_dir, "  -->  ", out_dir)
  if (dir_exists(out_dir)) dir_delete(out_dir)
  dir_create(out_dir, recurse = TRUE)
  shinylive::export(appdir = app_dir, destdir = out_dir)
}

study_dirs <- dir_ls(".", type = "directory", glob = "study-*", recurse = FALSE)

if (length(study_dirs) == 0) {
  message("ℹ️  No study-* directories found. Nothing to export.")
} else {
  dir_create("docs", recurse = TRUE, showWarnings = FALSE)
  file.create(path("docs", ".nojekyll"))  # keep this for ShinyLive safety
  
  for (study_dir in study_dirs) {
    study_name <- path_file(study_dir)
    
    # 1) Legacy main app at study-*/app
    legacy_app <- path(study_dir, "app")
    if (dir_exists(legacy_app) && is_shiny_app_dir(legacy_app)) {
      export_one(legacy_app, path("docs", "studies", study_name, "app"))
    }
    
    # 2) Sibling apps at study-*/X and study-*/X/app
    if (dir_exists(study_dir)) {
      siblings <- dir_ls(study_dir, type = "directory", recurse = FALSE)
      for (sib in siblings) {
        app_name <- path_file(sib)
        if (identical(app_name, "app")) next  # handled above
        
        # Flat: study-*/X/app.R
        if (is_shiny_app_dir(sib)) {
          export_one(sib, path("docs", "studies", study_name, app_name))
          next
        }
        # Nested: study-*/X/app/app.R
        nested_sib <- path(sib, "app")
        if (dir_exists(nested_sib) && is_shiny_app_dir(nested_sib)) {
          export_one(nested_sib, path("docs", "studies", study_name, app_name))
          next
        }
      }
    }
    
    # 3) Sub-apps inside legacy app/: study-*/app/Y and study-*/app/Y/app
    if (dir_exists(legacy_app)) {
      subapps <- dir_ls(legacy_app, type = "directory", recurse = FALSE)
      for (sub in subapps) {
        sub_name <- path_file(sub)
        
        # Flat sub-app: study-*/app/Y/app.R
        if (is_shiny_app_dir(sub)) {
          export_one(sub, path("docs", "studies", study_name, sub_name))
          next
        }
        # Nested sub-app: study-*/app/Y/app/app.R
        nested_sub <- path(sub, "app")
        if (dir_exists(nested_sub) && is_shiny_app_dir(nested_sub)) {
          export_one(nested_sub, path("docs", "studies", study_name, sub_name))
          next
        }
      }
    }
  }
  
  message("🧩 Export complete. Now rendering docs/index.md (if present)...")
  
  # ---- Render docs/index.md -> docs/index.html (keeps .nojekyll) ----
  md_path <- path("docs", "index.md")
  if (file_exists(md_path)) {
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      # Render in a clean environment to avoid side effects
      rmarkdown::render(
        input        = md_path,
        output_format = "html_document",
        output_file   = "index.html",
        output_dir    = "docs",
        envir         = new.env(parent = emptyenv()),
        quiet         = TRUE
      )
      message("✅ Rendered docs/index.md -> docs/index.html")
    } else {
      message("⚠️  'rmarkdown' not installed; cannot render docs/index.md.")
      message("    Install with: install.packages('rmarkdown')  (or remove .nojekyll to let Jekyll render .md)")
    }
  } else {
    message("ℹ️  docs/index.md not found; skipping homepage render.")
  }
  
  message("✅ Done. Apps are under docs/studies/<study>/<app_folder>; homepage served from docs/index.html")
}
