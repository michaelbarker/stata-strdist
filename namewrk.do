
clear all
/*

	st_rclear()
	st_global("r(bestname1)", "")              
	st_global("r(bestname2)", "")              
	st_numscalar("r(d)", ndist)
*/


mata


real scalar worddist(string scalar aword, string scalar bword) {
    alength = strlen(aword)          
	blength = strlen(bword)     
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


void namedist(string scalar aname , string scalar bname, 
				string scalar bestaname, string scalar bestbname, 
				real scalar dist) {

	A = tokens(aname)
	B = tokens(bname)

	// If either A or B is less than 2, a blank entry 
	if (cols(A)==0) A = ("","")
	else if (cols(A)==1) A = (A,"")

	if (cols(B)==0) B = ("","")
	else if (cols(B)==1) B = (B,"")

	// Store Distances	
	D = J(cols(A) , cols(B) , .)
    for (i=1 ; i<=cols(A) ; i++ ) { 
        for (j=1 ; j<=cols(B) ; j++ ) { 
			if (A[i] == "") {
				D[i,j]= strlen(B[j])
			}
			else if (B[j] == "") {
				D[i,j]= strlen(A[i])
			}
			else {
				D[i,j]= worddist(A[i],B[j])
			}
		}
	}
	D
	idx = matrixminindex(D)

	// Store first choice names and distance
	bestaname = A[idx[1]]
	bestbname = B[idx[2]]
	dist = D[idx[1],idx[2]]	
	dist

	// For second name, remove row and column of first combination
	D[idx[1],.]=J(1,cols(D),.)
	D[.,idx[2]]=J(rows(D),1,.) 
	// Find next best match	
	D
	idx = matrixminindex(D)
	// Add second names and distances
	if (A[idx[1]] != "" ) {
		bestaname = bestaname + " " + A[idx[1]]
	}
	if (B[idx[2]] != "" ) {
		bestbname = bestbname + " " + B[idx[2]]
	}
	dist = dist + D[idx[1],idx[2]]	
	dist
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
	minr=minr[1]
	minc= MinC[minr]
	return((minr, minc))
}

bestaname = ""
bestbname = ""
dist = .
namedist("Billy Jack" , "James Habyarimana", bestaname, bestbname, dist)
bestaname
bestbname
dist

namedist("Jack" , "James Habyarimana", bestaname, bestbname, dist)
bestaname
bestbname
dist

namedist("Jack" , "Habyarimana", bestaname, bestbname, dist)
bestaname
bestbname
dist

namedist("" , "Habyarimana", bestaname, bestbname, dist)
bestaname
bestbname
dist




end
