* DVH0340
*! version 1.0  2015-03-02 Michael D Barker

/*******************************************************************************
Michael Barker
March 3, 2015
mdb96@georgetown.edu

namedist.ado file
Implements distance calculation between two multiword strings.  
Order of words in strings is not uniform.
Calculate distance between all combinations of words in each string.
Choose minimum distance words.
Uses Levenstein distance as metric.
*******************************************************************************/

version 10


*** Check arguments and call appropriate sub-routine
program define namedist, rclass
    syntax anything [if] [in] , [Generate(name)]

	* Check Arguments
    gettoken first remain : anything , qed(isstring1)
    if `"`first'"' == "" error 102
    gettoken second remain : remain , qed(isstring2)
    if `"`second'"' == "" error 102
    if `"`remain'"' != "" error 103

    * Two string scalar version
    if `isstring1' & `isstring2' { 
		namedist0var `if' `in' , first(`"`first'"') second(`"`second'"') gen(`generate')
	}

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

        namedist12var `first' `second' `if' `in' , match(`"`strscalar'"') gen(`generate')
    } 

	return add
end

*** Two string scalar command
program define namedist0var, rclass
    syntax [if] [in] , first(string) second(string) [Generate(name)]
    marksample touse

    if `"`generate'"' != "" {
        confirm new variable `generate' 
	}

    mata: namedist0var(`"`first'"' , `"`second'"')

    if `"`generate'"' != "" {
        generate int `generate' = `r(d)' if `touse'
        return local strdist "`generate'"
    }

	return add 
end


*** One or two variable command
program define namedist12var , rclass
    syntax varlist(min=1 max=2 string) [if] [in] , [Match(string)] [GENerate(name)] 
    marksample touse , strok 

    *** Declare default and confirm newvarname 
    if `"`generate'"' == "" local generate "namedist"
    confirm new variable `generate' 

    tokenize "`varlist'"
    if "`2'"=="" mata: namedist1var("`1'" , `"`match'"' , "`generate'" , "`touse'")
    else         mata: namedist2var("`1'" ,  "`2'"      , "`generate'" , "`touse'")
    
    return local namedist "`generate'"
end


mata:
/******************************************************************************
   Terminology Note
   key: string to measure each observation against
   trymatch: one of many potential matches to be measured against the key
   TRIES: Nx1 vector of all "trymatch"s
******************************************************************************/

void namedist0var(string scalar aname , string scalar bname) {
	string scalar bestname1, bestname2

	ndist = namedist(aname , bname , bestname1, bestname2)

	st_rclear()
	st_global("r(bestname1)", bestname1)              
	st_global("r(bestname2)", bestname2)              
	st_numscalar("r(d)", ndist)              
}

void namedist1var(string scalar avarname, string scalar bname, 
                 string scalar newvar , string scalar touse) {

	ANAMES = st_sdata(. , avarname , touse) // Nx1 string vector with potential matches 
	dist = J(rows(ANAMES),1,.)             // Nx1 real vector to hold lev distances to each match

	for (t = 1 ; t <= rows(ANAMES) ; t++) { 
		dist[t] = namedist(bname,ANAMES[t,1]) // save distance
    } 
    st_store(. , st_addvar("int", newvar) , touse , dist)
}

void namedist2var(string scalar avarname  , string scalar bvarname, 
                  string scalar newvar , string scalar touse) {

    ANAMES = st_sdata(. , avarname , touse) 
	BNAMES = st_sdata(. , bvarname , touse) // Nx1 string vector with potential matches 
	dist = J(rows(ANAMES),1,.)          	// Nx1 real vector to hold lev distances to each match
	
	for (t = 1 ; t <= rows(ANAMES) ; t++) { 
	    dist[t] = namedist(ANAMES[t,1],BNAMES[t,1])
    } 
    st_store(. , st_addvar("int", newvar) , touse , dist)
}

real scalar worddist(string scalar aword, string scalar bword) {

    // get word lengths
	alength = strlen(aword)        
	blength = strlen(bword)     
	
	// handle empty strings
	if (alength==0) return(blength)
	else if (blength==0) return(alength)
	
    // declare distance matrix	
	D = J(alength , blength , .)
    // Add starting penalties in first column
    D = ((1::alength) , D)
    // Add starting penalties in first row
    D = ((0..blength) \ D) 	
    // add penalty for each operation required to reconcile the two strings 
    for (i = 1 ; i <= alength ; i++ ) { 
        for (j = 1 ; j <= blength ; j++ ) { 
            if (substr(aword, i, 1) == substr(bword, j, 1)) {
                D[i+1,j+1] = D[i,j]
            }
            else {         //    (deletion   , insertion  , substition)
                D[i+1,j+1] = min((D[i,j+1]+1 , D[i+1,j]+1 , D[i,j]+1  ))	
            }
        }
    }
    return(D[i,j]) 
}

real scalar namedist(string scalar aname , string scalar bname, 
					| string scalar bestaname, string scalar bestbname) {

	if (args()==4) {
		bestaname=""
		bestbname=""
	}

	A = tokens(aname)
	B = tokens(bname)

	// One of A or B must have at least 2 names
	// If one has less, fill with empty string up to 2
	// If both have less, return all missing 
	if (cols(A)>=2) {
		while (cols(B)<2) B = (B,"")
	}
	else if (cols(B)>=2) {
		while(cols(A)<2) A = (A,"")
	}
	else {
		return(.)
	}
	
	// Calculate and Store Distances	
	D = J(cols(A) , cols(B) , .)
    for (i=1 ; i<=cols(A) ; i++ ) { 
        for (j=1 ; j<=cols(B) ; j++ ) { 
			D[i,j]= worddist(A[i],B[j])
		}
	}
	
	// Get row and column index of minimum distance
	idx = matrixminindex(D)

	// Store first choice names and distance
	bestaname = A[idx[1]]
	bestbname = B[idx[2]]
	dist = D[idx[1],idx[2]]	

	// For second name, remove row and column of first combination
	D[idx[1],.]=J(1,cols(D),.)
	D[.,idx[2]]=J(rows(D),1,.) 
	// Find next best match	

	idx = matrixminindex(D)
	// Add second names and distances
	// Trim leading and trailing blanks, in case closest match is to empty string
	bestaname = strtrim(bestaname + " " + A[idx[1]])
	bestbname = strtrim(bestbname + " " + B[idx[2]])
	dist = dist + D[idx[1],idx[2]]	
	
	return(dist)
}

real rowvector matrixminindex(real matrix D) {
	real colvector minr, minc
	real matrix w
		
	// Find index of minimum distance pair in distance matrix
	MinC = J(rows(D), 1 , .) 
	MinD= J(rows(D), 1 , .) 
	for (i=1 ; i<=rows(D) ; i++ ) { 
		minindex(D[i,.], 1, minc , w)
		if (nonmissing(minc)>0) {
			MinC[i]=minc[1]
			MinD[i]=D[i,minc[1]]
		}
	}
	minindex(MinD, 1, minr, w)
	if (nonmissing(minr)>0) {
		minr=minr[1]
		minc= MinC[minr]
		return((minr, minc))
	}
	else {
		return((.,.))
	}
}

end

/*

*/

