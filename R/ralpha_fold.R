#' @title Fold Code Sections in RStudio
#' @description Automatically fold code sections in RStudio editor to improve
#'  code readability and navigation
#' @param get_time Logical value indicating whether to track and display
#' function execution time. Default is taken from option "fab_time" or FALSE
#'  if not set.
#' @return Invisible NULL. The function is called for its side effect of folding
#'  code sections in the RStudio editor.
#' @examples
#' \dontrun{
#' # Fold code sections in the current RStudio editor
#' ralpha_fold()
#'
#' # Fold code sections and display timing information
#' ralpha_fold(get_time = TRUE)
#' }
#' @export

ralpha_fold <- function(get_time = getOption("fab_time", default = FALSE)) {

	fab_browse <- getOption("fab_browse", default = FALSE)
	foTmr <- Rtimer$new()
	foTmr$add("0_start folding process")
	colFact <- 1E-3
	srcContext <- rstudioapi::getSourceEditorContext()
	{
		getPos <- function() {
			srcContext %>%
				primary_selection() %>%
				getElement("range")
		}
		PN_DP <- function(posNum) {
			row <- floor(posNum)
			col <- ((posNum - row) / colFact) %>% round(8)
			document_position(row, col)
		}
		DP_PN <- function(docPos) {
			posNum <- (docPos[1] + docPos[2] * colFact) %>% round(8)
			as.numeric(posNum)
		}
		DR_PN <- function(docPos) {
			posNum <- (docPos$start[1] + docPos$start[2] * colFact) %>% round(8)
			as.numeric(posNum)
		}
		PN_DR <- function(posNum_start, posNum_end = posNum_start) {
			SRow <- floor(posNum_start)
			SCol <- ((posNum_start - SRow) / colFact) %>% round(8)
			ERow <- floor(posNum_end)
			ECol <- ((posNum_end - ERow) / colFact) %>% round(8)
			document_range(
				start = document_position(SRow, SCol),
				end = document_position(ERow, ECol)
			)
		}
		endLine <- function(posNum) {
			ceiling(posNum) - colFact
		}
		getIndents <- function() {
			docCode <- srcContext$contents
			codeExtr <- paste0(docCode, "\n")

			# Define constants
			STRING_CHARS <- c('"', "'", "`")
			COMMENT_CHAR <- "#"
			open_bracket <- "{"
			close_bracket <- "}"
			ESCAPE_PATTERN <- "\\\\+%s$"

			# Initialize state variables
			indentLevel <- 0
			inString <- FALSE
			inCom <- FALSE
			outChar <- ""

			allIndents <- data.table(
				is_open = numeric(),
				indentLevel = numeric(),
				rowNum = numeric(),
				chrNum = numeric(),
				partNumber = numeric(),
				partName = character()
			)
			partNumber <- rep(0, 10)

			lineNums <- length(codeExtr) %>% seq_len()
			for (lineNum in lineNums) {
				lineContent <- codeExtr[lineNum]
				line_length <- nchar(lineContent)

				for (i_char in seq_len(line_length)) {
					chr <- substr(lineContent, i_char, i_char)

					if (!inString && !inCom) {
						# Check for string start
						if (chr %in% STRING_CHARS) {
							# Check for escaping
							if (i_char > 1) {
								lineStart <- substr(lineContent, 1, i_char)
								escapePatt <- sprintf(ESCAPE_PATTERN, chr)
								escaping <- stringi::stri_extract(lineStart, regex = escapePatt)
								isEscaped <- !is.na(escaping)
							} else {
								isEscaped <- FALSE
							}

							if (!isEscaped) {
								inString <- TRUE
								outChar <- chr
							}

						} else if (chr == COMMENT_CHAR) {
							# Check for comment start
							inCom <- TRUE
						} else if (chr == open_bracket) {
							# Track opening bracket
							indentLevel <- indentLevel + 1
							partNumber[indentLevel] <- partNumber[indentLevel] + 1
							partName <- partNumber[partNumber > 0] %>% paste(collapse = ".")
							newIdent <- data.table(
								is_open = 1,
								indentLevel = indentLevel,
								partNumber = partNumber[indentLevel],
								rowNum = lineNum,
								chrNum = i_char,
								partName = partName
							)
							allIndents <- allIndents %>% rbind(newIdent)
						} else if (chr == close_bracket) {
							# Track closing bracket
							partNumber[indentLevel + 1] <- 0
							partName <- partNumber[partNumber > 0] %>% paste(collapse = ".")
							newIdent <- data.table(
								is_open = 0,
								indentLevel = indentLevel,
								partNumber = partNumber[indentLevel],
								rowNum = lineNum,
								chrNum = i_char,
								partName = partName
							)
							indentLevel <- indentLevel - 1
							allIndents <- allIndents %>% rbind(newIdent)
						}
					} else if (inCom) {
						if (chr == "\n") {
							inCom <- FALSE
						}
					} else if (inString && chr == outChar) {
						# Check for string end
						if (i_char > 1) {
							lineStart <- substr(lineContent, 1, i_char)
							escapePatt <- sprintf(ESCAPE_PATTERN, chr)
							escaping <- stringi::stri_extract(lineStart, regex = escapePatt)
							nbEsc <- if (is.na(escaping)) 0 else stringi::stri_count(escaping, regex = "\\\\")
							isEscaped <- nbEsc %% 2 == 1
						} else {
							isEscaped <- FALSE
						}

						if (!isEscaped) {
							inString <- FALSE
						}
					}
				}
			}

			allIndents_wEdges <- allIndents %>%
				mutate(partName = paste0("1.", partName)) %>%
				rbind(data.table(is_open = 1, indentLevel = 0, rowNum = 1, chrNum = 0, partNumber = 1, partName = 1), fill = TRUE) %>%
				rbind(data.table(is_open = 0, indentLevel = 0, rowNum = lineNum, chrNum = i_char, partNumber = 1, partName = 1), fill = TRUE) %>%
				printif(FALSE)
			return(allIndents_wEdges)
		}
		foldFromPN <- function(opBracketPN) {
			waitTime <- getOption("fab_wait", default = 0)
			sectionStart <- PN_DP(opBracketPN)
			setCursorPosition(sectionStart)
			Sys.sleep(waitTime)
			executeCommand("expandToMatching")
			Sys.sleep(waitTime)
			executeCommand("fold")
			Sys.sleep(waitTime)
		}
	} # local funs
	# Main execution
	retainPos <- getPos()
	retainPN <- DR_PN(retainPos)
	foTmr$add("1_analyse code") ; allIndents <- getIndents()
	foTmr$add("2_retreat code") ; {
		allIndents_ret <- allIndents %>%
			mutate(brPN = (rowNum + chrNum * colFact) %>% round(8)) %>%
			group_by(partName) %>%
			mutate(endBrPN = lead(brPN)) %>%
			ungroup() %>%
			mutate(opnBrPN = (brPN + colFact) %>% round(8)) %>%
			filter(is_open == 1) %>%
			mutate(cursor_PN = retainPN) %>%
			mutate(endBrEndLine = ceiling(endBrPN) - colFact) %>%
			mutate(contains_cursor = between(cursor_PN, opnBrPN, endBrEndLine)) %>%
			printif(FALSE)
	} # Process indent information
	foTmr$add("3_properly fold")
	if (fab_browse) browser()
	{
		cursor_situation <- allIndents_ret %>%
			filter(contains_cursor) %>%
			filter(indentLevel == max(indentLevel)) %>%
			slice_max(brPN) %>%
			printif(fab_browse)

		parentName <- cursor_situation$partName %>%
			stringi::stri_replace(regex = "\\.[0-9]+$", "")

		parentSection_full <- allIndents_ret %>%
			filter(partName %>% str_detect(paste0("^", parentName, "(\\.|$)"))) %>%
			printif(fab_browse)

		parentSection_full_fold <- parentSection_full %>%
			ungroup() %>%
			filter(indentLevel >= cursor_situation$indentLevel) %>%
			filter(indentLevel <= cursor_situation$indentLevel + 1) %>%
			arrange(desc(indentLevel), contains_cursor)
	} # Determine sections to fold
	lapply(parentSection_full_fold$opnBrPN, foldFromPN) # Perform folding
	{
		currSection <- cursor_situation
		newPlace <- if (nrow(currSection)) {
			round(currSection$opnBrPN - colFact, 5)
		} else {
			warning("newPlace for cursor not found - returning to previous position")
			retainPN
		}
		newPlace_DP <- newPlace %>% PN_DP()
		setCursorPosition(newPlace_DP)
	} # Reposition cursor
	{
		foTmr$add("end")
		foTmr$get() %>%
			as_tibble() %>%
			select(-ct_proc, -ct_num, -dt_num, -tot_num) %>%
			printif(get_time)
	} # Display timing if requested
	invisible(NULL)
}
