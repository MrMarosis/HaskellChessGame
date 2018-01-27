
    inList::Int->[Int]->Bool
    inList x [] = False
    inList x (y:ys) = if x==y then True
                        else inList x ys  
    
    isRepeating::Int->[Int]->Bool
