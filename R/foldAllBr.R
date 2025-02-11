#' @title Easily Fold Code Parts
#' @description This function works with code split into parts identified by brackets.
#' The format is as follows:
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
#' It automatically identifies parts to fold/unfold easily.
#'
#' Shortcuts required:
#' \itemize{
#'   \item "fold all brackets": shift + alt + S (Windows) / ctrl + shift + up (Mac)
#'   \item "expand fold": shift + alt + D (Windows) / ctrl + shift + down (Mac)
#' }
#'
#' @param time Logical. If `TRUE`, the function will return \code{ggplot} object
#'  visualizing execution times for each step.
#' @param debug_getTbl Logical. If `TRUE`, returns the `docContent` table with
#' tags for debugging purposes.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{debug_info}: A data frame with debugging information if \code{debug_getTbl = TRUE}.
#'   \item \code{timer_plot}: A \code{ggplot} object visualizing execution times if \code{time = TRUE}.
#' }
#' If both parameters are \code{FALSE}, the function returns a list with \code{NULL} values.
#'
#' @importFrom tibble rowid_to_column
#' @importFrom magrittr add
#' @importFrom stringr str_detect str_remove_all str_extract str_remove str_count
#' @import rstudioapi
#' @export
#

foldAllBr <- function(time = FALSE, debug_getTbl = FALSE){

	fnTmr <- timer(step = "start")
	fnTmr <- timer(fnTmr, step = "init, funs")
	colFact <- 1E-3
	{
		# foldBrLine : given a line, fold the bracket ending it ====================
		foldBrLine <- function(opLine, waitTime = 0){
			setCursorPosition(document_position(opLine, 999))
			Sys.sleep(waitTime)
			executeCommand("expandToMatching")
			Sys.sleep(waitTime)
			executeCommand("fold")
			Sys.sleep(waitTime)
		} # foldBrLine

		# getPos : get pos =========================================================
		getPos <- function(){
			getSourceEditorContext() %>%
				primary_selection() %>%
				getElement("range")
		} # getPos

		# PN_DP : pos num to document position =====================================
		PN_DP <- function(posNum){
			row <- floor(posNum)
			col <- ((posNum - row) / colFact) %>% round(8)
			document_position(row, col)
		} # PN_DP
		# DP_PN : document position to pos num ======================================
		DP_PN <- function(docPos){
			posNum <- docPos[1] + (docPos[2] * colFact) %>% round(8)
			as.numeric(posNum)
		} # DP_PN

		# DR_PN : document range to pos num ========================================
		DR_PN <- function(docPos){
			posNum <- docPos$start[1] + (docPos$start[2] * colFact) %>% round(8)
			as.numeric(posNum)
		} # DR_PN

		# PN_DR : pos num to document range ========================================
		PN_DR <- function(posNum_start, posNum_end = posNum_start){
			SRow <- floor(posNum_start)
			SCol <- ((posNum_start - SRow) / colFact) %>% round(8)
			ERow <- floor(posNum_end)
			ECol <- ((posNum_end - ERow) / colFact) %>% round(8)
			document_range(
				start = document_position(SRow, SCol)
				, end = document_position(ERow, ECol)
			)
		} # PN_DR

		# endLine : endLine of a posNum =========
		endLine <- function(posNum) ceiling(posNum) - colFact

	} # local funs
	{
		fnTmr <- timer(fnTmr, step = "read Content")
		retainPos <- getPos()
		curPosNum <- retainPos$start %>% DP_PN
		retainStartRow <- retainPos$start[1]
		retainStartCol <- retainPos$start[2]
		docContent <- getSourceEditorContext()$contents %>%
			data.frame(content = .) %>%
			tibble::rowid_to_column()
	} # read content, get current position

	browseOption <- getOption("FAB_browse")
	if(!is.null(browseOption)) if(browseOption == 1) browser()
	{
		fnTmr <- timer(fnTmr, step = "tags")
		opName <- "+"
		clName <- "-"
		opBrPatt <- "\\{$"
		clBrPatt <- "^\t*\\}(\\))? #" # can also be "})" - must be commented
		comPatt <- "(?<=^( |\t){0,99})#.*"
		passBrPatt <- "^\t*\\}.*\\{$" # to handle "if() {...} else {...} cases

		docContent_tags <- docContent %>%
			mutate(content = content %>% str_remove(comPatt)) %>%
			mutate(
				opBr = content %>% str_detect(opBrPatt)
				, clBr = content %>% str_detect(clBrPatt)
				, passBr = content %>% str_detect(passBrPatt)
				, anyBr = pmax(opBr, clBr)
				, brTag = paste0(
					NULL
					, ifelse(opBr, opName, "")
					, ifelse(clBr, clName, "")
					, ifelse(passBr, "__", "") # mostly to avoid '} else {' pbs
				)
			) %>%
			identity

		fnTmr <- timer(fnTmr, step = "countSwitches")
		docContent_incs <- docContent_tags %>%
			countSwitches("brTag", opName, clName) %>%
			identity

		fnTmr <- timer(fnTmr, step = "ret : other treatments")
		docContentRet <- docContent_incs %>%
			as_tibble %>%
			mutate(conCat = paste("0", lvl_1, lvl_2, lvl_3, sep = "_")) %>%
			mutate(conCatLim = conCat %>% str_remove_all("_0") %>% paste0("_")) %>%
			mutate(isCur = ifelse(retainStartRow == rowid, "=cur=", "_")) %>%
			group_by(conCatLim) %>%
			mutate(isSecStart = rowid == min(rowid)) %>%
			ungroup %>%
			mutate(opBrPlace = content %>% str_extract(paste0(".*", opBrPatt)) %>% nchar) %>%
			mutate(opBrPN = rowid + opBrPlace * colFact) %>%
			select(-opBrPlace) %>%
			mutate(nbTabs = content %>% str_count("\t|\\{$")) %>%
			mutate(checkCat = ifelse(content == "", 0, nbTabs - catLvl)) %>%
			identity
	} # retreat doc content --> docContentRet
	if(debug_getTbl) {
		linesBefore <- 3 ; linesAfter <- 10
		interm_tbl_debug <- docContentRet %>%
			select(rowid, content, anyBr, brTag,
				   conCatLim, isCur, isSecStart, opBrPN,
				   catLvl, nbTabs, checkCat)
	} # debug only : check for problems

	{
		fnTmr <- timer(fnTmr, step = "docCont norm")
		curLine <- docContentRet %>% filter(isCur == "=cur=")
		curPosSec <- curLine$conCatLim # init before check
		curPosCat <- curLine$catLvl
		noBracket <- !str_detect(curLine$content, opBrPatt)
		skipIf <- curPosCat == 0 & noBracket # to check if we're on top level
		if(curLine$isSecStart & !skipIf){

			# cursor on a starting line of a section
			isBfSecStart <- curLine$opBrPN >= curPosNum
			isAtSecStart <- curLine$opBrPN == (curPosNum - colFact) %>% round(10)
			if(isBfSecStart){
				curPosSec <- curLine$conCatLim %>% str_remove("_[0-9]*_$")
				curPosCat <- curLine$catLvl - 1
			} #
			if(isAtSecStart){
				executeCommand("expandToMatching")
				executeCommand("fold")
				setCursorPosition((curPosNum - colFact) %>% PN_DP)
				return(NULL)
			} #
		} # if we're on a section start, and not "at lvl 0 without bracket"

		curSection <- docContentRet %>%
			filter(conCatLim  %>% str_detect(paste0("^",curPosSec))) %>%
			identity
		subSections <- curSection %>%
			filter(catLvl == curPosCat + 1) %>%
			identity
		subSectionsStarts <- subSections %>%
			group_by(conCatLim) %>%
			slice_min(rowid) %>%
			arrange(rowid) %>%
			identity

	} # identify current line, section, subsections
	{
		fnTmr <- timer(fnTmr, step = "check if 1 big")
		onlyOneSec <- FALSE
		docContentRet %>% count(lvl_1, lvl_2, lvl_3)
		if(max(docContentRet$lvl_1) == 1) onlyOneSec <- TRUE
	} # check if the document is made of only 1 section
	{
		fnTmr <- timer(fnTmr, step = "sectionStart line and PN")
		sectionStartLine <- curSection %>%
			slice_min(rowid)

		sectionStart_PN <- sectionStartLine %>%
			pull(opBrPN) %>%
			magrittr::add(colFact)
		if(skipIf) sectionStart_PN <- (1 + colFact) %>% round(8)

	} # sectionStart : get corresponding line and PN

	waitOption <- getOption("FAB_wait") # to visualize function being executed
	if(is.null(waitOption)) waitOption = 0

	fnTmr <- timer(fnTmr, step = "fold")
	subSectionsStarts %>%
		pull(rowid) %>%
		lapply(foldBrLine, waitTime = waitOption) # fold lines

	fnTmr <- timer(fnTmr, step = "put cursor back - end")
	sectionStart_DP <- sectionStart_PN %>% PN_DP
	backToInit <- (onlyOneSec & curPosSec == "0_1")|curPosSec == 0
	if(backToInit) {
		setCursorPosition(curPosNum %>% PN_DP)
	} else {
		setCursorPosition(sectionStart_DP)
	} #

	fnTmr <- fnTmr %>% timer(end = TRUE)
	if(time){
		timerPlot <- fnTmr %>%
			arrange(-timeStamp_num) %>%
			mutate(step = factor(step, levels = step)) %>%
			ggplot(aes(step, dt_num)) +
			geom_col() +
			theme(axis.text = element_text(size = 12)) +
			geom_text(aes(
				label = dt_num %>% round(2)
				, y = pmin(dt_num + 0.06, 3)
			)) +
			coord_flip(ylim = c(0,3)) +
			ggtitle(
				paste0("function : ", "foldAllBr")
				, subtitle = paste0("nrows docContent : ", nrow(docContent))
			)
		lum_0_100(50)
	} # timer plot

	return(list(
		debug_info = if (debug_getTbl) interm_tbl_debug else NULL
		, timer_plot = if (time) timerPlot else NULL
		))
} #

