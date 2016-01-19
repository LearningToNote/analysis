bag.make = function(x, n = 100000000000){

    bag = bag_o_words(x)
    bag = sort(table(bag), dec = TRUE)
    n = min(n, length(bag))
    counts = bag[1:n]
    bag = names(bag)[1:n]

    b = list(items = bag, counts = counts,

        apply = function(targ){
            target = apply(data.frame(targ), 1, bag_o_words)

            #idx = apply(target, 2, match, bag)
            m = matrix(0, nrow = length(targ), ncol=length(bag))
            #i ~ row, j ~ column
            for(i in 1:length(targ)) {
                for (j in 1:length(bag)) {
                    if (bag[j] %in% unlist(target[i])) {
                        m[i,j] = 1
                    } else {
                        m[i,j] = 0
                    }

                }
            }
            #sapply(1:nrow(m), function(r){
            #    m[r, idx[r,]] <<- 1
            #})
            colnames(m) = bag
            m
        })
    class(b) = 'bag'
    b
}
bag = bag.make(tdata$BEFORE)
bag$apply(tdata$BEFORE)
