# This is only to avoid "no visible binding" notes in package check. Source solution :
	# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887/3
	# "hideous hack" according to https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
	# countSwitches
	 'stepStr'
	,'findStt'
	,'findEnd'
	,'nbStt'
	,'nbEnd'
	,'catLvl'
	,'inc1'
	,'raw1'
	,'inc2'
	,'raw2'
	,'inc3'
	,'raw3'
	# foldAllBr
	,'.'
	,'content'
	,'opBr'
	,'clBr'
	,'passBr'
	,'lvl_1'
	,'lvl_2'
	,'lvl_3'
	,'conCat'
	,'conCatLim'
	,'opBrPlace'
	,'nbTabs'
	,'catLvl'
	,'anyBr'
	,'brTag'
	,'isCur'
	,'isSecStart'
	,'opBrPN'
	,'checkCat'
	,'timeStamp_num'
	,'step'
	,'dt_num'
	# importAll
	,'fulPath'
	,'locPath'
	,'ext'
	,'fun'
	,'cst'
	,'fName'
	,'toExclude'
	# left_join_checks
	,'tmp_inX'
	,'tmp_inY'
	,'.'
	,'check'
	,'req'
	,'value'
	,'is_problem'
	,'timeStamp_num'
	,'step'
	,'dt_num'
	,'secsPerMLines'
	# ralpha_fold
	,"rowNum"
	,"chrNum"
	,"partName"
	,"is_open"
	,"brPN"
	,"endBrPN"
	,"opnBrPN"
	,"cursor_PN"
	,"endBrEndLine"
	,"contains_cursor"
	,"indentLevel"
	,"ct_proc"
	,"ct_num"
	,"tot_num"
	# ralpha_unfold
	,'lineFull'
	,'hasBracket'
	,'isCurLine'
	,'scanStart'
	,'lineEnd'
	,'BrRelPos'
	,'BrAbsPos'
	,'start'
	,'absPos'
	,'BrAbsPN'
	,'tail'
	,'head'
	# timer
	,'timeStamp_num'
	,'timeStamp'
	,'dt_num'
	,'dt_text'
))
