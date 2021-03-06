
*! version 1.0  11nov2012 Michael D Barker

/*******************************************************************************
Michael Barker
Nov 9 2012
mdb96@georgetown.edu

strdist.ado file
Implements distance calculation between two strings.  
Uses Levenstein distance as metric.
*******************************************************************************/

version 10

*** Check arguments and call appropriate sub-routine
program define strdist, rclass
    syntax anything [if] [in] , [Generate(name)]

    gettoken first remain : anything , qed(isstring1)
    if `"`first'"' == "" error 102
    gettoken second remain : remain , qed(isstring2)
    if `"`second'"' == "" error 102
    if `"`remain'"' != "" error 103

    * Two string scalar version
    if `isstring1' & `isstring2' strdist0var `if' `in' , first(`"`first'"') second(`"`second'"') gen(`generate')

    * One or two string variable version
    else {

        local strscalar ""

        if `isstring1' {
            local strscalar = `"`first'"'
            local first ""
        }
        else if `isstring2' {
            local strscalar = `"`second'"'
            local second ""
        }

        strdist12var `first' `second' `if' `in' , m(`"`strscalar'"') gen(`generate')
    } 

    * Return values generated by subroutines
    return scalar d = r(d)
    return local strdist "`r(strdist)'"
end

*** One or two variable command
program define strdist12var , rclass
    syntax varlist(min=1 max=2 string) [if] [in] , [Match(string)] [GENerate(name)] 
    marksample touse , strok 

    *** Declare default and confirm newvarname 
    if `"`generate'"' == "" local generate "strdist"
    confirm new variable `generate' 

    tokenize "`varlist'"
    if "`2'"=="" mata: matalev1var("`1'" , `"`match'"' , "`generate'" , "`touse'")
    else         mata: matalev2var("`1'" ,  "`2'"      , "`generate'" , "`touse'")
    
    return local strdist "`generate'"
end

*** Two string scalar command
program define strdist0var, rclass
    syntax [if] [in] , first(string) second(string) [Generate(name)]
    marksample touse

    tempname dist
    mata: st_numscalar("`dist'" , matalev(`"`first'"' , `"`second'"'))

    if `"`generate'"' != "" {
        confirm new variable `generate' 
        generate int `generate' = `dist' if `touse'
        return local strdist "`generate'"
    }
    
    display as result `dist'
    return scalar d = `dist'

end

mata:
/******************************************************************************
   Terminology Note
   key: string to measure each observation against
   trymatch: one of many potential matches to be measured against the key
   TRIES: Nx1 vector of all "trymatch"s
******************************************************************************/

void matalev1var(string scalar varname, string scalar key, 
                 string scalar newvar , string scalar touse) {

	TRIES = st_sdata(. , varname , touse) // Nx1 string vector with potential matches 
	dist = J(rows(TRIES),1,.)             // Nx1 real vector to hold lev distances to each match

	for (t = 1 ; t <= rows(TRIES) ; t++) { 
		dist[t] = matalev(key,TRIES[t,1]) // save distance
    } 
    st_store(. , st_addvar("int", newvar) , touse , dist)
}

void matalev2var(string scalar var1   , string scalar var2, 
                 string scalar newvar , string scalar touse) {

    KEYS  = st_sdata(. , var1 , touse) 
	TRIES = st_sdata(. , var2 , touse) // Nx1 string vector with potential matches 
	dist = J(rows(TRIES),1,.)          // Nx1 real vector to hold lev distances to each match
	
	for (t = 1 ; t <= rows(TRIES) ; t++) { 
	    dist[t] = matalev(KEYS[t,1],TRIES[t,1])
    } 
    st_store(. , st_addvar("int", newvar) , touse , dist)
}

real scalar matalev(string scalar key, string scalar trymatch) {
    keylength = strlen(key)          
	trylength = strlen(trymatch)     
    // declare distance matrix	
	D = J(keylength , trylength , .)
    // Add starting penalties in first column
    D = ((1::keylength) , D)
    // Add starting penalties in first row
    D = ((0..trylength) \ D) 	
    // add penalty for each operation required to reconcile the two strings 
    for (i = 1 ; i <= keylength ; i++ ) { 
        for (j = 1 ; j <= trylength ; j++ ) { 
            if (substr(key, i, 1) == substr(trymatch, j, 1)) {
                D[i+1,j+1] = D[i,j]
            }
            else {         //    (deletion   , insertion  , substition)
                D[i+1,j+1] = min((D[i,j+1]+1 , D[i+1,j]+1 , D[i,j]+1  ))	
            }
        }
    }
    return(D[i,j]) 
    }
end

