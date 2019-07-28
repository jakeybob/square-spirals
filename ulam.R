# set of functions for drawing square Archimedes spirals

# rounded down quadratic solution (+ve only) giving which loop any given number is on
which_loop <- function(l){
  ceiling((-3 + sqrt(9 + 16*l))/8) # ceiling so there is no zeroth loop except for origin point @ 0
}

# length of nth loop
loop_length <- function(n){
  (8*n) - 1
}

# total length up to and including nth loop
total_length <- function(n){
  n*(4*n + 3)
}

# what position does the point have in the loop (0 indexed)
loop_position <- function(l){
  l - total_length(which_loop(l) - 1)
}

# 0 to n, n rep (2n)-2, n to -n, -n rep (2n)-1, -n to 0
x_coord_loop <- function(n){
  x <- c(seq.int(0, n),
         rep(n, (2*n)-2),
         seq.int(n, -n),
         rep(-n, (2*n)-1),
         seq.int(-n, 0))
  return(x)
}

# rep(-n+1), n), -n+1 to n, rep(n, (2n)-1), n to -n , rep(-n, n-1)
y_coord_loop <- function(n){
  y <- c(rep(-n + 1, n),
         seq.int(-n +1, n),
         rep(n, (2*n) - 1),
         seq.int(n, -n),
         rep(-n, n))
  return(y)
}

# returns x co-ord of point at length l
x_coord_point <- function(l){
  n <- which_loop(l)
  pos <- loop_position(l)
  
  return(x_coord_loop(n)[pos + 1])
}

# returns y co-ord of point at length l
y_coord_point <- function(l){
  n <- which_loop(l)
  pos <- loop_position(l)
  
  return(y_coord_loop(n)[pos + 1])
}

# returns set of x,y co-ords for set of points up to length l
xy_coords <- function(end_length){
  n <- seq(1, which_loop(end_length)) # which loops to draw
  full_size <- total_length(max(n))
  loop_ends_0 <- c(0, sapply(n, total_length))
  
  xs <- integer(full_size)
  ys <- integer(full_size)
  
  for(loop in n){
    x <- x_coord_loop(loop)
    y <- y_coord_loop(loop)
    xs[(loop_ends_0[loop]+1) : loop_ends_0[loop+1]] <- x[-length(x)]
    ys[(loop_ends_0[loop]+1) : loop_ends_0[loop+1]] <- y[-length(y)]
    
  }
  
  coords <- list(x = xs[1:(end_length+1)], 
                 y = ys[1:(end_length+1)])
}
