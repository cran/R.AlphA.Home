#' @title Left Join with Validation Checks
#' @description a custom usage of left_join, with more detailed checks.
#' Performs a left join and verifies that no unexpected duplicates or mismatches
#'  occur. In cas of unexpected results, gives details about what caused the
#'  problem.
#'
#' @param x A data.table representing the left table.
#' @param y A data.table representing the right table.
#' @param ... Additional arguments passed to `dplyr::left_join`.
#' @param req_preserved_x Logical. Ensure that the number of rows in `x`
#' remains unchanged after the join. Default: TRUE.
#' @param req_xAllMatch Logical. Ensure that all rows in `x` find a match in `y`.
#' Default: FALSE.
#' @param behavior Character. Specifies behavior if validation fails.
#' Options: `"warning"` or `"error"`. (default: `"warning"`)
#' @param showNotFound Logical. Show rows from `x` that did not match with `y`.
#' Default: FALSE.
#' @param showProblems Logical. Display the problems encountered during the
#' joining process, if any.
#' @param time Logical. Internal argument used only for testing purposes, timing
#' the function steps
#'
#' @return A data.table containing the joined table.
#' @importFrom tibble rowid_to_column rownames_to_column
#' @importFrom tidyr replace_na
#' @importFrom stringr str_remove
#' @rawNamespace import(dplyr, except = c(first, last, between))
#' @export
#'
#' @examples
#' library(data.table)
#' library(dplyr)
#'
#' # Example 1: Simple left join with all matches
#' table_left <- data.table(id = 1:3, value_left = c("A", "B", "C"))
#' table_right <- data.table(id = 1:3, value_right = c("X", "Y", "Z"))
#' result <- left_join_checks(table_left, table_right, by = "id", req_preserved_x = TRUE)
#' print(result) # Ensures all rows in table_left are preserved
#'
#' # Example 2: Left join with missing matches
#' table_left <- data.table(id = 1:5, value_left = c("A", "B", "C", "D", "E"))
#' table_right <- data.table(id = c(1, 3, 5), value_right = c("X", "Y", "Z"))
#' result <- left_join_checks(
#'   table_left,
#'   table_right,
#'   by = "id",
#'   req_preserved_x = TRUE,
#'   showNotFound = TRUE,
#'   behavior = "warning"
#' )
#' print(result) # Rows from table_left with no matches in table_right are shown
#'
left_join_checks <- function(
		x
		, y
		, ...
		, req_xAllMatch = 1
		, req_preserved_x = 1
		, behavior = "error"
		, showNotFound = FALSE
		, showProblems = TRUE
		, time = FALSE
){

	{
		fnTmr <- timer(step = "Start --")
		# preparation pour merge
		fnTmr <- timer(fnTmr, step = "indexes - inX, inY")
		xMerge <- x %>% rowid_to_column(var = "tmp_xID") %>% mutate(tmp_inX = 1)
		yMerge <- y %>% rowid_to_column(var = "tmp_yID") %>% mutate(tmp_inY = 1)
	} # preparation
	{
		fnTmr <- timer(fnTmr, step = "join itself")
		joinXY <- left_join(
			xMerge
			, yMerge
			, ...
		) %>% replace_na(list(tmp_inX = 0, tmp_inY = 0))
	} # merge
	{
		fnTmr <- timer(fnTmr, step = "chk_preserved_x")
		chk_preserved_x <- all.equal(joinXY$tmp_xID, xMerge$tmp_xID) %>% isTRUE
		fnTmr <- timer(fnTmr, step = "chk_dups_x")
		chk_dups_x <- duplicated(joinXY$tmp_xID) %>% sum
		fnTmr <- timer(fnTmr, step = "chk_preserved_y")
		chk_preserved_y <- all.equal(joinXY$tmp_yID, yMerge$tmp_yID) %>% isTRUE
		fnTmr <- timer(fnTmr, step = "chk_dups_y")
		chk_dups_y <- duplicated(joinXY$tmp_yID) %>% sum
		fnTmr <- timer(fnTmr, step = "joinMatch_prep")
		joinMatch_prep <- joinXY %>% count(tmp_inX, tmp_inY)
		joinMatch <- expand.grid(tmp_inX = 0:1, tmp_inY = 0:1) %>%
			left_join(joinMatch_prep) %>%
			suppressMessages %>%
			replace_na(list(n=0))
		chk_yNotFound <- joinMatch %>% filter(!tmp_inY) %>% pull(n) %>% sum
		chk_xAllMatch <- chk_yNotFound == 0
		fnTmr <- timer(fnTmr, step = "counting problems")
		valuesTable <- mget(ls(pattern = "^chk_")) %>%
			unlist %>%
			data.frame(value = .) %>%
			rownames_to_column("check") %>%
			mutate(key = str_remove(check, "^chk_|^req_"))

		reqsTable <- mget(ls(pattern = "^req_")) %>%
			unlist %>%
			data.frame(req = .) %>%
			rownames_to_column("check") %>%
			mutate(req = req %>% as.numeric) %>%
			mutate(key = str_remove(check, "^chk_|^req_"))

		checksTable <- full_join(valuesTable, reqsTable, by = "key") %>%
			mutate(is_problem = value != req) %>%
			select(key, value, req, is_problem) %>%
			mutate(is_problem = is_problem %>% as.numeric) %>%
			replace_na(list(is_problem = 0)) %>%
			arrange(is_problem %>% desc, req %>% desc)
	} # checks

	nbPbs <- sum(checksTable$is_problem, na.rm = TRUE)
	if (nbPbs) {
		commonMsg <- paste0(nbPbs, " problem(s) during merge",
							"\nsee report for details")

		if (showNotFound & !chk_xAllMatch) print(joinXY %>% filter(!tmp_inY))
		if(showProblems == TRUE) print(checksTable)

		if (behavior == "warning") {
			warning(commonMsg)
		} else if (behavior == "error") {
			stop(commonMsg)
		} # warning or error
	} # react to problems

	fnTmr <- timer(fnTmr, end = TRUE)
	if(time){
		xSize <- nrow(x)
		timePerM <- sum(fnTmr$dt_seconds/xSize*1E6) %>% round(2)
		timerPlot <- fnTmr %>%
			arrange(-timeStamp_num) %>%
			mutate(step = factor(step, levels = step)) %>%
			mutate(secsPerMLines = dt_num / xSize * 1E6) %>%
			ggplot(aes(step, secsPerMLines)) +
			geom_col() +
			theme(axis.text = element_text(size = 12)) +
			geom_text(aes(
				label = secsPerMLines %>% round(2)
				, y = pmin(secsPerMLines + 0.06, 3)
			)) +
			coord_flip(ylim = c(0,3)) +
			ggtitle(
				paste0("function : ", "left_join_checks")
				, paste0(
					"total time per M: ", timePerM, " s"
					, "   -   "
					, "xSize : ", xSize %>% sepThsd
				)
			)
		lum_0_100(60)
		print(timerPlot)
	} # display timer if requested

	# return joined table, without temp columns
	return(joinXY %>% select(-starts_with("tmp_")))
}
