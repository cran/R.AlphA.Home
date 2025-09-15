#' @title Unfold Code Sections in RStudio
#' @description ralpha_fold() and ralpha_unfold() allow usage of the R.AlphA
#' code format that keeps long scripts easily readable.
#'
#' This format is based on identifying code parts with brackets, and an
#' optional but recommended comment at the end :
#'
#' \preformatted{
#' \{
#'   ...
#'   code from part 1
#'   ...
#' \} # part 1
#' \{
#'   ...
#' \} # part 2
#' }
#'
#'then appearing as
#' \preformatted{
#' \{...\} # part 1
#' \{...\} # part 2
#' }
#'
#' To stay easy to manipulate, this format requires shortcuts to easily open
#' or close the different sections.
#'
#' ralpha_fold() will fold the different code parts and go back to beginning
#' of current part
#'
#' ralplha_unfold() will unfold a code part and jump to the next braces when
#' relevant.
#'
#' both combined will provide a convenient way to manage what is displayed on
#' screen, ensuring a constant global overview of the document.
#'
#' Shortcuts required:
#' Here are the suggested shortcuts, both for Mac and Windows :
#' \itemize{
#'   \item ralpha_fold   : use ctrl+up
#'   \item ralpha_unfold : use ctrl+down
#' }
#'
#' @return NULL (invisibly).
#' This function performs actions only (cursor movement and unfolding)
#'
#' @import rstudioapi
#' @importFrom data.table data.table
#' @importFrom tibble tibble rowid_to_column
#' @importFrom dplyr filter mutate group_by slice ungroup slice_min pull
#' @importFrom stringr str_locate_all str_locate str_detect
#' @importFrom stringi stri_extract stri_count
#' @export

ralpha_unfold <- function(){
	colFact <- 1E-3
	caseMsg <- 0
	{
		getPos     <- function(){
			srcContext %>%
				primary_selection() %>%
				getElement("range")
		} # getPos : get pos
		PN_DP      <- function(posNum, colFact = 1E-3){
			row <- floor(posNum)
			col <- ((posNum - row) / colFact) %>% round(8)
			document_position(row, col)
		} # PN_DP : pos num to document position
		DP_PN      <- function(docPos){
			posNum <- (docPos[1] + docPos[2] * colFact) %>% round(8)
			as.numeric(posNum)
		} # DP_PN : document position to pos num
		DR_PN      <- function(docPos){
			posNum <- (docPos$start[1] + docPos$start[2] * colFact) %>% round(8)
			as.numeric(posNum)
		} # DR_PN : document range to pos num
		PN_DR      <- function(posNum_start, posNum_end = posNum_start){
			SRow <- floor(posNum_start)
			SCol <- ((posNum_start - SRow) / colFact) %>% round(8)
			ERow <- floor(posNum_end)
			ECol <- ((posNum_end - ERow) / colFact) %>% round(8)
			document_range(
				start = document_position(SRow, SCol)
				, end = document_position(ERow, ECol)
			)
		} # PN_DR : pos num to document range
		endLine    <- function(posNum) ceiling(posNum) - colFact # endLine : endLine of a posNum
	} # local funs
	srcContext <- rstudioapi::getSourceEditorContext()
	retainPos <- getPos();
	retainPN <- DR_PN(retainPos)
	{
		srcAllText <- srcContext %>% getElement("contents")
		cursorLine <- retainPos$start[1]
		cursorCol <- retainPos$start[2]

		textTable <- tibble(lineFull = srcAllText) %>%
			rowid_to_column %>%
			filter(rowid >= cursorLine) %>%
			mutate(hasBracket = str_detect(lineFull, "\\{")) %>%
			mutate(isCurLine = rowid == cursorLine) %>%
			filter(hasBracket|isCurLine) %>%
			group_by(isCurLine) %>% slice(1) %>% ungroup %>%
			mutate(scanStart = ifelse(isCurLine, cursorCol, 1)) %>%
			mutate(lineStart = substr(lineFull, 0, scanStart-1)) %>%
			mutate(lineEnd = substr(lineFull, scanStart, nchar(lineFull))) %>%
			mutate(BrRelPos = str_locate(lineEnd, "\\{")[,1]) %>%
			mutate(BrAbsPos = BrRelPos + scanStart-1) %>%
			mutate(BrAbsPN = (rowid + (BrAbsPos+1) * colFact) %>% round(8)) %>%
			printif(0)

		curLineInfo <- textTable %>%
			filter(isCurLine) %>%
			printif(0)

		noOprBrNextLines <- nrow(textTable %>% filter(!isCurLine)) == 0
		OpBrBefore <- curLineInfo$lineStart %>% str_detect("\\{")
		ClBrBefore <- curLineInfo$lineStart %>% str_detect("\\}")
		OpBrAfter <- curLineInfo$lineEnd %>% str_detect("\\{")
		ClBrAfter <- curLineInfo$lineEnd %>% str_detect("\\}")
		findLastClosing <- curLineInfo$lineStart %>%
			str_locate_all("\\}") %>%
			as.data.frame %>%
			tail(1) %>%
			pull(start) %>%
			printif(0)
		findLastOpen <- curLineInfo$lineStart %>%
			str_locate_all("\\{") %>%
			as.data.frame %>%
			tail(1) %>%
			pull(start) %>%
			printif(0)
		findNextOpen <- curLineInfo$lineEnd %>%
			str_locate_all("\\{") %>%
			as.data.frame %>%
			head(1) %>%
			mutate(absPos = start + cursorCol) %>%
			pull(absPos) %>%
			printif(0)
	} # analyze text

	# cases handling ===========================================================
	case_99 <- noOprBrNextLines & !OpBrAfter
	case_2 <- curLineInfo$lineStart  %>% grepl(pattern = "\\{$"     ) # 2_afterBr → 2 (inutile car inclus dans 1 ? si c'est utile)
	case_3 <- curLineInfo$lineEnd    %>% grepl(pattern = "^\\{"     ) # 3_beforeBR → 2
	case_5 <- curLineInfo$lineFull   %>% grepl(pattern = "\\}.*\\{" ) # 5_endStart → 3
	case_6 <- curLineInfo$lineFull   %>% grepl(pattern = "\\{.*\\}" ) # 6_startEnd
	case_4 <- curLineInfo$lineFull   %>% grepl(pattern = "\\}"      ) # 4_blockEnd → 1/3 - pas complet car inclus cas 3 mais comme on l'a vérifié avant on peut s'en contenter pour l'instant
	case_1 <- TRUE # all other cases                                  # 1_noBr → 2
	if(case_99){
		test <- printif("99 - no more br", caseMsg)
		executeCommand("unfold")
		return(invisible(NULL))
	}
	if(case_2){
		printif("case_2", caseMsg)
		nextBrInfo <- textTable %>%
			filter(!is.na(BrAbsPN)) %>%
			slice_min(rowid) %>%
			printif(0)
		executeCommand("unfold")
		setCursorPosition(nextBrInfo$BrAbsPN %>% PN_DP)
		executeCommand("unfold")
		return(invisible(NULL))
	}
	if(case_3){
		printif("case_3", caseMsg)
		executeCommand("unfold")
		setCursorPosition(document_position(cursorLine, cursorCol + 1))
		return(invisible(NULL))
	}
	if(case_5){
		printif("case_5", caseMsg)
		case_5.1 <- !ClBrBefore & OpBrAfter
		case_5.2 <- ClBrBefore  & OpBrAfter
		case_5.3 <- ClBrBefore  & !OpBrAfter
		if(case_5.1){
			printif("case_5.1", caseMsg)
			nextBrInfo <- textTable %>%
				filter(!is.na(BrAbsPN)) %>%
				slice_min(rowid) %>%
				printif(0)
			executeCommand("unfold")
			setCursorPosition(nextBrInfo$BrAbsPN %>% PN_DP)
			return(invisible(NULL))
		} # 5.1 | } {
		if(case_5.2){
			printif("case_5.2", caseMsg)
			setCursorPosition(document_position(cursorLine, findLastClosing-1))
			executeCommand("unfold")
			setCursorPosition(document_position(cursorLine, findNextOpen-1))
			return(invisible(NULL))
		} # 5.2 } | {
		if(case_5.3){
			printif("case_5.3", caseMsg)
			setCursorPosition(document_position(cursorLine, findLastClosing-1))
			executeCommand("unfold")
			setCursorPosition(document_position(cursorLine, findLastOpen))
			return(invisible(NULL))
		} # 5.3 } { |
	}
	if(case_6){
		printif("case_6", caseMsg)
		if(OpBrBefore){
			setCursorPosition(document_position(cursorLine, findLastOpen))
			executeCommand("unfold")
		} else {
			setCursorPosition(document_position(cursorLine, findNextOpen))
			executeCommand("unfold")
		}
		return(invisible(NULL))
	}
	if(case_4){
		printif("case_4", caseMsg)
		setCursorPosition(document_position(cursorLine, 999))
		executeCommand("unfold")
		setCursorPosition(document_position(cursorLine+1, 0))
		return(invisible(NULL))
	}
	if(case_1){
		printif("case_1", caseMsg)
		nextBrInfo <- textTable %>%
			filter(!is.na(BrAbsPN)) %>%
			slice_min(rowid) %>%
			printif(0)
		setCursorPosition(nextBrInfo$BrAbsPN %>% PN_DP)
		executeCommand("unfold")
		return(invisible(NULL))
	}

	printif("case not found", caseMsg)
	executeCommand("unfold")
}
