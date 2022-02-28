#' Symlink Fastqs
#'
#' A function to create symlinks with easier to parse names that the typical names that files have coming off the sequencing machine
#' @param seq.dirs The path(s) to the directory(s) in which the raw sequence files are contained. If there are multiple paths, this needs to be a named vector that is reflected in the ids.tbl provided as well.
#' @param ids.tbl A data.frame or data.table containing sample names in one column and their corresponding file IDs in another (e.g. barcodes or sample IDs as used in the raw sequence files). Sample names and file IDs *may* be identical in some instances, please still provide a two column table.
#' @param smpl.id.col The name/number of the column in the `ids.tbl` that contains the name of the samples, which will be used in naming the symlinks. Default is 'Sample'.
#' @param file.id.col The name/number of the column in the `ids.tbl` that contains the corresponding file IDs for matching raw sequence files. Default is 2.
#' @param split.pattern A character string containg the pattern you want to use to separate the sample name and R1/R2 in the symlink names. Default is '--'.
#' @param quiet Logical, if TRUE, there will be no printing of progress. Default is FALSE
#' @export

symlink.fastqs <- function(
  seq.dirs,
  ids.tbl,
  smpl.id.col = "Sample",
  file.id.col = 2,
  run.id.col = NULL,
  split.pattern = "--",
  quiet = FALSE
) {
  if (length(seq.dirs) > 1) {
    if (is.null(names(seq.dirs))) {
      stop("argument `seq.dirs` is greater than one, but has no run IDs")
    }
  } else {
    names(seq.dirs) <- "Run1"
  }
  if ("data.table" %in% class(ids.tbl)) {
    ids.dt <- ids.tbl
  } else {
    ids.dt <- as.data.table(ids.dt)
  }
  if (is.null(run.id.col)) {
    smpl.id.counts <- table(ids.dt[[smpl.id.col]])
    if (any(smpl.id.counts > 1)) {
      stop(
        paste(
          "The following samples occur more than once in the supplied table:\n\t",
          paste(names(smpl.id.counts[smpl.id.counts > 1]), collapse = "\n\t"),
          "If there are multiple runs in this analysis please provide an argument",
          "to `run.id.col'"
        )
      )
    } else {
      if (length(names(seq.dirs)) > 1) {
        matching.col <- which(apply(ids.dt, 2, function(col) all(run.ids %in% col)))
        if (length(matching.col) == 1) {
          proceed <- readline(
            prompt = paste0(
              "No run.id.col was provided, but there are multiple seq.dirs.\n",
              "The run IDs appear to match the data in column ",
              names(matching.col),
              ". Do you want to proceed using this column? [y/n]: "
            )
          )
          while (!(proceed %in% c("y", "n"))) {
            proceed <- readline(prompt="Please answer y or n: ")
          }
          if (proceed == "n") {
            return("stopped")
          } else {
            run.id.col <- names(matching.col)
          }
        } else if (length(matching.col) > 1) {
          stop(
            paste0(
              "No run.id.col was provided, but there are multiple seq.dirs,\n",
              "and the names of the seq.dirs match multiple columns in the ids.tbl.\n",
              "Please provide the name of the run.id.col and re-run."
            )
          )
        } else {
          stop(
            paste0(
              "No run.id.col was provided, but there are multiple seq.dirs,\n",
              "and the names of the seq.dirs don't match any columns in the ids.tbl.\n",
              "Please add a run IDs column to the ids.tbl, \n",
              "provide the name of the run.id.col, and re-run."
            )
          )
        }
      } else {
        ids.dt[, Run := "Run1"]
        run.id.col <- "Run"
      }
    }
  }
  setkeyv(ids.dt, smpl.id.col)
  for (run.id in names(seq.dirs)) {
    run.ids.dt <- ids.dt[eval(parse(text = run.id.col)) == run.id]
    seq.dir <- seq.dirs[run.id]
    for (smpl in run.ids.dt[[smpl.id.col]]) {
      file.id <- run.ids.dt[smpl][[file.id.col]]
      if (!quiet) {
        cat(paste0(file.id, " -> ", smpl, " ... "), sep = "")
      }
      seq.files <- list.files(
        path = seq.dir,
        pattern = file.id,
        full.names = T
      )
      if (length(seq.files) == 0) {
        stop("no files matched file ID")
      } else {
        read1.file <- seq.files[str_detect(seq.files, "[-_\\.]R1[-_\\.]")]
        read2.file <- seq.files[str_detect(seq.files, "[-_\\.]R2[-_\\.]")]
        lnName.r1 <- paste(smpl, "R1.fastq.gz", sep = split.pattern)
        lnName.r2 <- paste(smpl, "R2.fastq.gz", sep = split.pattern)
        cmd1 <- paste("ln -s", read1.file, lnName.r1)
        cmd2 <- paste("ln -s", read2.file, lnName.r2)
        system(cmd1)
        system(cmd2)
        check.cmd1 <- paste("zcat", lnName.r1, "2>/dev/null | head -n 1")
        check1 <- length(system(check.cmd1, intern = TRUE)) > 0
        check.cmd2 <- paste("zcat", lnName.r2, "2>/dev/null | head -n 1")
        check2 <- length(system(check.cmd2, intern = TRUE)) > 0
        if (!quiet & check1 & check2) {
          cat("good", sep = "\n")
        } else if (!check1 & check2) {
          cat(paste0(lnName.r1, " is empty (links to", read1.file, ")"), sep = "\n")
        } else if (check1 & !check2) {
          cat(paste0(lnName.r2, " is empty (links to", read2.file, ")"), sep = "\n")
        } else {
          cat(
            paste0(
              lnName.r1, " and ", lnName.r2,
              " are empty (link to", read1.file, " and ", read2.file, ", respectively)"
            ),
            sep = "\n"
          )
        }
      }
    }
  }
}
