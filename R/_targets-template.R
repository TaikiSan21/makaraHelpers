library(targets)
# Set target options:
tar_option_set(
    packages = c('dplyr', 
                 'lubridate',
                 'bigrquery',
                 'tidyr', 
                 'makaraValidatr')
)

# Run the R scripts in the R/ folder with your custom functions:
# uncomment this chunk to force janky single-file update of my common functions
# updateFunctions <- download.file('https://api.github.com/repos/TaikiSan21/makaraHelpers/contents/R/makara-functions.R',
#                      destfile = 'functions/makara-functions.R',
#                      method='libcurl',
#                      headers=c('Accept'= 'application/vnd.github.v3.raw'),
#                      extra='-O -L')
tar_source('functions/makara-functions.R')

# Set TRUE to force re-loading BigQuery database
reload_database <- FALSE

### dont change below ###
if(!tar_exist_objects('db_raw')) {
    reload_database <- TRUE
}
if(isTRUE(reload_database)) {
    reload_database <- 'always'
} else if(isFALSE(reload_database)) {
    reload_database <- 'thorough'
}
####

list(
    # parameters ----
    # Values you can adjust to change how things run
    tar_target(params, {
        list(
            # Whether or not to export data already present in DB TRUE/FALSE,
            'export_already_in_db' = TRUE,
            # Allow replacing non-NA database values with NA - almost always FALSE
            'replace_db_with_na' = FALSE,
            # keep extra columns with output - for testing
            'keep_extra_columns' = FALSE,
            'update_device_orgs' = TRUE,
            # set this to TRUE to remove mandatory fields with NA values
            'drop_mandatory_na' = FALSE
        )
    }),
    # constants ----
    # Put any hard coded values here for transparency
    tar_target(constants, {
        list(
        )
    }),
    # templates ----
    # tar_target(template_dir, 'templates'),
    tar_target(templates, {
        # now uses makaraValidatr
        formatBasicTemplates()
    }),
    # db tables ----
    tar_target(db_raw, {
        downloadBqMakara()
    }, cue=tar_cue(reload_database)),
    tar_target(db, {
        formatBqMakara(db_raw)
    }),
    # combine all outputs into a list
    # e.g. $deployments, $recordings
    tar_target(combined_data, {
        
    }),
    # final checks ----
    tar_target(db_check, {
        out <- checkAlreadyDb(combined_data, db)
        out <- dropAlreadyDb(out, drop=!params$export_already_in_db)
        out <- checkMakTemplate(out,
                                templates=templates,
                                ncei=FALSE,
                                dropEmpty = TRUE,
                                dropExtra=!params$keep_extra_columns,
                                dropMandatoryNA=params$drop_mandatory_na)
        out <- checkDbValues(out, db, updateOrgs=params$update_device_orgs)
        out <- checkDbReplacements(out, db, replaceWithNA = params$replace_db_with_na)
        checkWarnings(out)
        out
    }),
    tar_target(output_dir, 'outputs'),
    tar_target(output, {
        writeTemplateOutput(db_check, folder=output_dir)
        output_dir
    }, format='file'),
    tar_target(validatr, {
        validate_submission(output, 
                            output_file = file.path(output_dir, 'validation_results.csv'),
                            verbose=FALSE)
    })
)
