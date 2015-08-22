*! version 1.0.2, Ben Jann, 03may2007

program define _eststo, byable(onecall)
	version 8.2
	if "`_byvars'"!="" local by "by `_byvars'`_byrc0' : "
	if inlist(`"`1'"',"clear","drop") {
		`by'eststo `0'
	}
	else {
		capt _on_colon_parse `0'
		if !_rc {
			local command `"`s(after)'"'
			if `"`command'"'!="" {
				local command `":`command'"'
			}
			local 0 `"`s(before)'"'
		}
		syntax [anything] [, Esample * ]
		if `"`esample'"'=="" {
			local options `"noesample `options'"'
		}
		if `"`options'"'!="" {
			local options `", `options'"'
		}
		`by'eststo `anything'`options' `command'
	}
end
